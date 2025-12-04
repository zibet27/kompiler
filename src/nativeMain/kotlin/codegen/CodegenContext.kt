@file:OptIn(ExperimentalForeignApi::class)

package codegen

import ast.*
import kotlinx.cinterop.ExperimentalForeignApi
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

    val context: LLVMContextRef = createContext()
    val builder: LLVMBuilderRef = context.createBuilder()
    val module: LLVMModuleRef = context.createModule("Kode")

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
        val tempBuilder = context.createBuilder()
        try {
            return with(tempBuilder, block)
        } finally {
            tempBuilder.dispose()
        }
    }

    // --- LLVM Helpers ---
    fun buildAlloca(name: String, type: LLVMTypeRef): LLVMValueRef = withBuilder {
        val entry = currentFunction!!.entryBasicBlock
        val first = entry.firstInstruction
        if (first != null) {
            positionBefore(first)
        } else {
            positionAtEnd(entry)
        }
        buildAlloca(type, name)
    }

    fun addBasicBlock(name: String): LLVMBasicBlockRef {
        return currentFunction!!.appendBasicBlock(context, name)
    }

    fun load(value: LLVMValueRef, type: LLVMTypeRef, name: String): LLVMValueRef {
        return builder.buildLoad(type, value, name)
    }

    // --- Type Helpers ---

    protected val voidType = context.voidType()
    protected val i32Type = context.i32Type()
    protected val f32Type = context.f32Type()
    protected val f64Type = context.f64Type()
    protected val arrayKind = i32Type.arrayType(1u).kind
    protected val pointerKind = i32Type.pointerType(DEFAULT_RAM_SPACE).kind

    val LLVMTypeRef.isVoid: Boolean
        get() = this == voidType

    val LLVMTypeRef.isPointer: Boolean
        get() = kind == pointerKind

    val LLVMTypeRef.isArray: Boolean
        get() = kind == arrayKind

    val LLVMTypeRef.isFloatLike: Boolean
        get() = kind == f32Type.kind || kind == f64Type.kind

    // --- Constant Helpers ---
    fun Int.i32(): LLVMValueRef = i32Type.constInt(this.toULong())

    fun Double.toLLVM(): LLVMValueRef = f64Type.constReal(this)

    val zero = 0.i32()

    // --- Function Management ---
    fun ensureFunction(name: String, params: List<Param>, type: TypeRef, alien: Boolean) {
        val existing = functions[name]
        if (existing != null) {
            return
        }
        val fnType = type.toLLVM()
        val fn = module.addFunction(name, fnType)
        if (alien) {
            fn.linkage = LLVMLinkage.LLVMExternalWeakLinkage
        }
        for ((idx, param) in params.withIndex()) {
            val arg = fn.getParam(idx.toUInt())
            arg.valueName = param.name
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
        val value = module.addGlobal(type, name)
        globals[name] = Symbol(value, type)
    }

    // --- Array Type Helper ---
    fun LLVMTypeRef.withDimensions(arrayDims: List<IntLit>): LLVMTypeRef {
        if (arrayDims.isEmpty()) return this
        return arrayDims.reversed().fold(this) { ty, dimExpr ->
            ty.arrayType(dimExpr.value.toUInt())
        }
    }

    fun TypeRef.toLLVM(): LLVMTypeRef = when (this) {
        is BuiltinType -> when (kind) {
            BuiltinType.Kind.I32 -> i32Type
            BuiltinType.Kind.U8 -> context.i8Type()
            BuiltinType.Kind.F64 -> f64Type
            BuiltinType.Kind.Void -> voidType
        }

        is PointerType -> (0 until levels).fold(initial = base.toLLVM()) { ty, _ ->
            ty.pointerType(DEFAULT_RAM_SPACE)
        }

        is NamedType -> getStructType(name) ?: error("Can't find struct $name")

        is FuncType -> functionType(
            returnType = returnType.toLLVM(),
            paramTypes = paramTypes.map { it.toLLVM() }
        )
    }

    override fun close() {
        builder.dispose()
        module.dispose()
        context.dispose()
    }
}
