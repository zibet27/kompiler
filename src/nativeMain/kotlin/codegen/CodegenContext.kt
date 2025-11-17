@file:OptIn(ExperimentalForeignApi::class)

package codegen

import ast.BuiltinType
import ast.FieldDecl
import ast.FuncType
import ast.NamedType
import ast.PointerType
import ast.TypeRef
import kotlinx.cinterop.ExperimentalForeignApi
import kotlinx.cinterop.cValuesOf
import kotlinx.cinterop.toKString
import llvm.*

internal inline fun <T, R> Iterable<T>.forEachRetLast(crossinline block: (T) -> R): R? {
    var result: R? = null
    for (item in this) {
        result = block(item)
    }
    return result
}

private const val DEFAULT_RAM_SPACE = 0u

open class CodegenContext : AutoCloseable {

    val context: LLVMContextRef = LLVMContextCreate()!!
    val builder: LLVMBuilderRef = LLVMCreateBuilderInContext(C = context)!!
    val module: LLVMModuleRef = LLVMModuleCreateWithNameInContext(
        ModuleID = "Kode", C = context
    )!!

    // Environments
    protected val functions = mutableMapOf<String, LLVMValueRef?>()
    protected val globals = mutableMapOf<String, LLVMValueRef?>()
    protected val structs = mutableMapOf<String, LLVMTypeRef?>()
    protected val structFields = mutableMapOf<String, List<FieldDecl>>()

    protected val scopes = ArrayDeque<MutableMap<String, LLVMValueRef?>>()
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

    protected fun putLocal(name: String, value: LLVMValueRef?) {
        scopes.lastOrNull()?.set(name, value)
    }

    protected fun getLocal(name: String): LLVMValueRef? {
        return scopes.asReversed().firstNotNullOfOrNull { it[name] }
    }

    fun findAlloca(name: String): LLVMValueRef {
        return getLocal(name) ?: globals[name] ?: error("Variable $name not found")
    }

    // --- LLVM Helpers ---
    fun buildAlloca(type: LLVMTypeRef?, name: String): LLVMValueRef {
        val fn = currentFunction
        val entry = LLVMGetEntryBasicBlock(fn)
        val saveIp = LLVMGetInsertBlock(builder)
        // position at entry for alloca to enable mem 2 reg later
        val firstInstr = LLVMGetFirstInstruction(entry)
        if (firstInstr != null) {
            LLVMPositionBuilderBefore(builder, firstInstr)
        } else {
            LLVMPositionBuilderAtEnd(builder, entry)
        }
        val alloca = LLVMBuildAlloca(builder, type, name)!!
        LLVMPositionBuilderAtEnd(builder, saveIp)
        return alloca
    }

    fun addBasicBlock(name: String): LLVMBasicBlockRef {
        return LLVMAppendBasicBlockInContext(C = context, Fn = currentFunction, name)!!
    }

    // --- Type Helpers ---
    fun typeKind(ty: LLVMTypeRef?): Int = LLVMGetTypeKind(ty).toInt()

    fun isVoid(ty: LLVMTypeRef?): Boolean =
        typeKind(ty) == typeKind(LLVMVoidTypeInContext(context))

    fun isPointer(ty: LLVMTypeRef?): Boolean =
        typeKind(ty) == typeKind(LLVMPointerType(LLVMInt8TypeInContext(context), 0u))

    fun isFloatLike(ty: LLVMTypeRef?): Boolean {
        val k = typeKind(ty)
        val f32 = typeKind(LLVMFloatTypeInContext(context))
        val f64 = typeKind(LLVMDoubleTypeInContext(context))
        return k == f32 || k == f64
    }

    // --- Constant Helpers ---
    fun constI32(value: ULong): LLVMValueRef =
        LLVMConstInt(LLVMInt32TypeInContext(context), value, 0)!!

    val zero = constI32(0U)

    // --- Function Management ---
    fun ensureFunction(name: String, type: LLVMTypeRef?): LLVMValueRef? {
        val existing = functions[name]
        if (existing != null) return existing
        return LLVMAddFunction(module, name, type).also { functions[name] = it }
    }

    // --- Struct Management ---
    fun getStructType(name: String): LLVMTypeRef? = structs[name]

    fun getStructFields(name: String): List<FieldDecl>? = structFields[name]

    fun registerStruct(name: String, type: LLVMTypeRef, fields: List<FieldDecl>? = null) {
        structs[name] = type
        fields?.let { structFields[name] = it }
    }

    // --- Global Management ---
    fun addGlobal(name: String, type: LLVMTypeRef?): LLVMValueRef? {
        return LLVMAddGlobal(module, type, name).also { globals[name] = it }
    }

    // --- Array Type Helper ---
    fun LLVMTypeRef.withDimensions(arrayDims: List<ast.IntLit>): LLVMTypeRef {
        if (arrayDims.isEmpty()) return this
        return arrayDims.reversed().fold(this) { ty, dimExpr ->
            LLVMArrayType(ElementType = ty, ElementCount = dimExpr.value.toUInt())!!
        }
    }

    fun TypeRef.toLLVM(): LLVMTypeRef = when (this) {
        is BuiltinType -> when (kind) {
            BuiltinType.Kind.I32 -> LLVMInt32TypeInContext(context)!!
            BuiltinType.Kind.U8 -> LLVMInt8TypeInContext(context)!!
            BuiltinType.Kind.F64 -> LLVMDoubleTypeInContext(context)!!
            BuiltinType.Kind.Void -> LLVMVoidTypeInContext(context)!!
        }

        is PointerType -> {
            var base = base.toLLVM()
            repeat(levels) {
                base = LLVMPointerType(ElementType = base, AddressSpace = DEFAULT_RAM_SPACE)!!
            }
            base
        }

        is NamedType -> {
            // Look up the struct type from the codegen environment
            getStructType(name) ?: LLVMPointerType(LLVMInt8TypeInContext(context), AddressSpace = DEFAULT_RAM_SPACE)!!
        }

        is FuncType -> {
            val paramTys = paramTypes.map { it.toLLVM() }.toTypedArray()
            val params = cValuesOf(*paramTys)
            LLVMFunctionType(
                ReturnType = returnType.toLLVM(),
                ParamTypes = params,
                ParamCount = params.size.toUInt(),
                IsVarArg = 0
            )!!
        }
    }

    override fun close() {
        LLVMDisposeBuilder(Builder = builder)
        LLVMDisposeModule(M = module)
        LLVMContextDispose(C = context)
    }

    companion object {
        fun generate(ast: ast.Program): String {
            val codegen = Codegen().also { it.visit(ast) }
            val ir = LLVMPrintModuleToString(codegen.module)?.toKString()
            return ir ?: error("Failed to generate LLVM IR")
        }
    }
}
