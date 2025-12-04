@file:OptIn(ExperimentalForeignApi::class)

package codegen

import ast.*
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.toCValues
import llvm.*

internal inline fun <T, R> Iterable<T>.forEachRetLast(crossinline block: (T) -> R): R? {
    var result: R? = null
    for (item in this) {
        result = block(item)
    }
    return result
}

private const val DEFAULT_RAM_SPACE = 0u

data class Symbol(val value: LLVMValueRef, val type: LLVMTypeRef)

open class CodegenContext : AutoCloseable {

    val context: LLVMContextRef = LLVMContextCreate()!!
    val builder: LLVMBuilderRef = LLVMCreateBuilderInContext(C = context)!!
    val module: LLVMModuleRef = LLVMModuleCreateWithNameInContext(
        ModuleID = "Kode", C = context
    )!!

    // Environments
    protected val functions = mutableMapOf<String, Symbol>()
    protected val globals = mutableMapOf<String, Symbol>()
    protected val structs = mutableMapOf<String, LLVMTypeRef?>()
    private val structFields = mutableMapOf<String, List<FieldDecl>>()

    protected val scopes = ArrayDeque<MutableMap<String, Symbol>>()
    var currentFunction: LLVMValueRef? = null

    // --- Scope Management ---
    protected inline fun <R> withScope(crossinline block: () -> R): R {
        pushScope()
        val result = block()
        popScope()
        return result
    }

    protected fun pushScope() = scopes.addLast(mutableMapOf())
    protected fun popScope() = scopes.removeLastOrNull()

    protected fun putLocal(name: String, alloca: LLVMValueRef?, type: LLVMTypeRef?) {
        scopes.lastOrNull()?.set(name, Symbol(alloca!!, type!!))
    }

    protected fun getLocal(name: String): Symbol? {
        return scopes.asReversed().firstNotNullOfOrNull { it[name] }
    }

    fun findSymbol(name: String): Symbol {
        return getLocal(name) ?: globals[name] ?: error("Variable $name not found")
    }

    inline fun <T> withBuilder(crossinline block: LLVMBuilderRef.() -> T): T {
        val builder = LLVMCreateBuilder()!!
        try {
            return with(builder, block)
        } finally {
            LLVMDisposeBuilder(builder)
        }
    }

    // --- LLVM Helpers ---
    fun buildAlloca(name: String, type: LLVMTypeRef): LLVMValueRef = withBuilder {
        val entry = LLVMGetEntryBasicBlock(Fn = currentFunction!!)
        val first = LLVMGetFirstInstruction(BB = entry)
        if (first != null) {
            LLVMPositionBuilderBefore(this, first)
        } else {
            LLVMPositionBuilderAtEnd(this, entry)
        }
        LLVMBuildAlloca(this, type, name)!!
    }

    fun addBasicBlock(name: String): LLVMBasicBlockRef {
        return LLVMAppendBasicBlockInContext(C = context, Fn = currentFunction, name)!!
    }

    val LLVMBasicBlockRef.terminator
        get() = LLVMGetBasicBlockTerminator(BB = this)

    fun load(value: LLVMValueRef, type: LLVMTypeRef, name: String): LLVMValueRef {
        return LLVMBuildLoad2(builder, type, value, name)!!
    }

    fun buildInBoundsGEP(
        ptr: LLVMValueRef,
        type: LLVMTypeRef,
        indices: List<LLVMValueRef>,
        name: String
    ): LLVMValueRef {
        return LLVMBuildInBoundsGEP2(builder, type, ptr, indices.toCValues(), indices.size.toUInt(), name)!!
    }

    // --- Type Helpers ---

    protected val voidType = LLVMVoidTypeInContext(context)!!
    protected val i32Type = LLVMInt32TypeInContext(context)!!
    protected val f32Type = LLVMFloatTypeInContext(context)!!
    protected val f64Type = LLVMDoubleTypeInContext(context)!!
    protected val arrayKind = LLVMArrayType(ElementType = i32Type, ElementCount = 1u)!!.kind
    protected val pointerKind = LLVMPointerType(ElementType = i32Type, AddressSpace = DEFAULT_RAM_SPACE)!!.kind

    val LLVMValueRef?.type: LLVMTypeRef
        get() = LLVMTypeOf(this!!)!!

    val LLVMTypeRef?.elementType: LLVMTypeRef
        get() = LLVMGetElementType(Ty = this)!!

    val LLVMTypeRef.kind: LLVMTypeKind
        get() = LLVMGetTypeKind(Ty = this)

    val LLVMTypeRef.isVoid: Boolean
        get() = this == voidType

    val LLVMTypeRef.isPointer: Boolean
        get() = kind == pointerKind

    val LLVMTypeRef.isArray: Boolean
        get() = kind == arrayKind

    val LLVMTypeRef.isFloatLike: Boolean
        get() = kind == f32Type.kind || kind == f64Type.kind

    // --- Constant Helpers ---
    fun Int.i32(): LLVMValueRef = LLVMConstInt(IntTy = i32Type, N = this.toULong(), SignExtend = 0)!!

    fun Double.toLLVM(): LLVMValueRef = LLVMConstReal(RealTy = f64Type, N = this)!!

    val zero = 0.i32()

    // --- Function Management ---
    fun ensureFunction(name: String, params: List<Param>, type: TypeRef, alien: Boolean) {
        val existing = functions[name]
        if (existing != null) {
            return
        }
        val fnType = type.toLLVM()
        val fn = LLVMAddFunction(M = module, name, FunctionTy = fnType)!!
        if (alien) {
            LLVMSetLinkage(fn, LLVMLinkage.LLVMExternalWeakLinkage)
        }
        for ((idx, param) in params.withIndex()) {
            val arg = LLVMGetParam(fn, idx.toUInt())!!
            LLVMSetValueName(arg, param.name)
        }
        functions[name] = Symbol(value = fn, fnType)
    }

    // --- Struct Management ---
    fun getStructType(name: String): LLVMTypeRef? = structs[name]

    fun getStructFields(name: String): List<FieldDecl> = structFields[name] ?: error("Struct $name not found")

    fun registerStruct(name: String, type: LLVMTypeRef, fields: List<FieldDecl>? = null) {
        structs[name] = type
        fields?.let { structFields[name] = it }
    }

    // --- Global Management ---
    fun addGlobal(name: String, type: LLVMTypeRef) {
        val value = LLVMAddGlobal(module, type, name)!!
        globals[name] = Symbol(value, type)
    }

    // --- Array Type Helper ---
    fun LLVMTypeRef.withDimensions(arrayDims: List<IntLit>): LLVMTypeRef {
        if (arrayDims.isEmpty()) return this
        return arrayDims.reversed().fold(this) { ty, dimExpr ->
            LLVMArrayType(ElementType = ty, ElementCount = dimExpr.value.toUInt())!!
        }
    }

    fun TypeRef.toLLVM(): LLVMTypeRef = when (this) {
        is BuiltinType -> when (kind) {
            BuiltinType.Kind.I32 -> i32Type
            BuiltinType.Kind.U8 -> LLVMInt8TypeInContext(context)!!
            BuiltinType.Kind.F64 -> f64Type
            BuiltinType.Kind.Void -> voidType
        }

        is PointerType -> (0 until levels).fold(initial = base.toLLVM()) { ty, _ ->
            LLVMPointerType(ElementType = ty, AddressSpace = DEFAULT_RAM_SPACE)!!
        }

        is NamedType -> getStructType(name) ?: error("Can't find struct $name")

        is FuncType -> {
            LLVMFunctionType(
                ReturnType = returnType.toLLVM(),
                ParamTypes = paramTypes.map { it.toLLVM() }.toCValues(),
                ParamCount = paramTypes.size.toUInt(),
                IsVarArg = 0
            )!!
        }
    }

    override fun close() {
        LLVMDisposeBuilder(Builder = builder)
        LLVMDisposeModule(M = module)
        LLVMContextDispose(C = context)
    }
}
