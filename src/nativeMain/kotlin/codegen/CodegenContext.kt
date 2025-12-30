@file:OptIn(ExperimentalForeignApi::class)

package codegen

import ast.*
import kotlinx.cinterop.ExperimentalForeignApi
import llvm.*
import type.KodeType

internal inline fun <T, R> Iterable<T>.forEachRetLast(crossinline block: (T) -> R): R? {
    var result: R? = null
    for (item in this) {
        result = block(item)
    }
    return result
}

private const val DEFAULT_RAM_SPACE = 0u

data class Symbol(val value: LLVMValueRef, val type: KodeType)

open class CodegenContext : AutoCloseable {

    val context: LLVMContextRef = createContext()
    val builder: LLVMBuilderRef = context.createBuilder()
    val module: LLVMModuleRef = context.createModule("Kode")

    // Environments
    protected val functions = mutableMapOf<String, Symbol>()
    protected val globals = mutableMapOf<String, Symbol>()
    protected val structs = mutableMapOf<String, LLVMTypeRef>()
    private val structFields = mutableMapOf<String, List<FieldDecl>>()

    protected val scopes = ArrayDeque<MutableMap<String, Symbol>>()
    var currentFunction: LLVMValueRef? = null

    // Loop control flow targets for break/continue
    var loopIncrementBlock: LLVMBasicBlockRef? = null
    var loopExitBlock: LLVMBasicBlockRef? = null

    // --- Scope Management ---
    protected inline fun <R> withScope(crossinline block: () -> R): R {
        pushScope()
        val result = block()
        popScope()
        return result
    }

    protected fun pushScope() = scopes.addLast(mutableMapOf())
    protected fun popScope() = scopes.removeLastOrNull()

    protected fun putLocal(name: String, alloca: LLVMValueRef?, type: KodeType) {
        scopes.lastOrNull()?.set(name, Symbol(alloca!!, type))
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
    fun buildAlloca(name: String, llvmType: LLVMTypeRef): LLVMValueRef = withBuilder {
        val entry = currentFunction!!.entryBasicBlock
        val first = entry.firstInstruction
        if (first != null) {
            positionBefore(first)
        } else {
            positionAtEnd(entry)
        }
        buildAlloca(llvmType, name)
    }

    fun addBasicBlock(name: String): LLVMBasicBlockRef {
        return currentFunction!!.appendBasicBlock(context, name)
    }

    fun load(value: LLVMValueRef, type: LLVMTypeRef, name: String): LLVMValueRef {
        return builder.buildLoad(type, value, name)
    }

    // --- Type Helpers ---

    protected val voidType = context.voidType()
    protected val boolType = context.i1Type()
    protected val i32Type = context.i32Type()
    protected val i8Type = context.i8Type()
    protected val f32Type = context.f32Type()
    protected val f64Type = context.f64Type()
    protected val pointerKind = i32Type.pointerType(DEFAULT_RAM_SPACE).kind

    val LLVMTypeRef.isVoid: Boolean
        get() = this == voidType

    val LLVMTypeRef.isPointer: Boolean
        get() = kind == pointerKind

    val LLVMTypeRef.isFloatLike: Boolean
        get() = kind == f32Type.kind || kind == f64Type.kind

    // --- Constant Helpers ---
    fun Int.i32(): LLVMValueRef = i32Type.constInt(this.toULong())

    fun Double.f64(): LLVMValueRef = f64Type.constReal(this)

    val zero = 0.i32()

    // --- Function Management ---
    fun ensureFunction(name: String, params: List<Param>, kodeType: KodeType.Fn, alien: Boolean) {
        if (functions.contains(name)) {
            return
        }
        val llvmFnType = kodeType.toLLVMSignature()
        val fn = module.addFunction(name, llvmFnType)
        if (alien) {
            fn.linkage = LLVMLinkage.LLVMExternalWeakLinkage
        }
        for ((idx, param) in params.withIndex()) {
            val arg = fn.getParam(idx.toUInt())
            arg.valueName = param.name
        }
        functions[name] = Symbol(value = fn, kodeType)
    }

    // --- Struct Management ---
    fun getStructType(name: String): LLVMTypeRef? = structs[name]

    fun getStructFields(name: String): List<FieldDecl> = structFields[name] ?: error("Struct $name not found")

    fun registerStruct(name: String, type: LLVMTypeRef, fields: List<FieldDecl>? = null) {
        structs[name] = type
        fields?.let { structFields[name] = it }
    }

    // --- Global Management ---
    fun addGlobal(name: String, kodeType: KodeType) {
        val value = module.addGlobal(kodeType.toLLVM(), name)
        globals[name] = Symbol(value, kodeType)
    }

    // --- Array Type Helper ---
    fun LLVMTypeRef.withDimensions(dimensions: List<Int>): LLVMTypeRef {
        return dimensions.reversed().fold(this) { ty, dim ->
            ty.arrayType(elementCount = dim.toUInt())
        }
    }

    fun KodeType.toLLVM(): LLVMTypeRef {
        val result = when (this) {
            is KodeType.I32 -> i32Type
            is KodeType.U8 -> i8Type
            is KodeType.F64 -> f64Type
            is KodeType.Void -> voidType
            is KodeType.Unknown -> error("Invalid type")
            is KodeType.Arr -> base.toLLVM().withDimensions(dimensions)
            is KodeType.Obj -> getStructType(name) ?: error("Can't find struct $name")

            // function opaque pointer
            is KodeType.Fn -> i32Type.pointerType(DEFAULT_RAM_SPACE)
            is KodeType.Ptr -> (0 until levels).fold(initial = base.toLLVM()) { ty, _ ->
                ty.pointerType(DEFAULT_RAM_SPACE)
            }
        }
        return result
    }

    fun KodeType.Fn.toLLVMSignature(): LLVMTypeRef = functionType(
        returnType = ret.toLLVM(),
        paramTypes = params.map { it.toLLVM() }
    )

    override fun close() {
        builder.dispose()
        module.dispose()
        context.dispose()
    }
}
