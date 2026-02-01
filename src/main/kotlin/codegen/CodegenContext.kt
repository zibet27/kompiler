package codegen

import ast.*
import llvm.*
import type.KodeType

internal inline fun <T, R> Iterable<T>.forEachRetLast(crossinline block: (T) -> R): R? {
    var result: R? = null
    for (item in this) {
        result = block(item)
    }
    return result
}

data class Symbol(val value: IRValue, val type: KodeType)

open class CodegenContext : AutoCloseable {

    val irContext = IRContext()
    val module = irContext.createModule("Kode")
    val builder = IRBuilder(irContext, module)

    // Environments
    protected val functions = mutableMapOf<String, Symbol>()
    protected val globals = mutableMapOf<String, Symbol>()
    protected val structs = mutableMapOf<String, IRType.Struct>()
    private val structFields = mutableMapOf<String, List<FieldDecl>>()

    protected val scopes = ArrayDeque<MutableMap<String, Symbol>>()
    var currentFunction: IRFunction? = null

    // Loop control flow targets for break/continue
    var loopIncrementBlock: IRBasicBlock? = null
    var loopExitBlock: IRBasicBlock? = null

    // --- Scope Management ---
    protected inline fun <R> withScope(crossinline block: () -> R): R {
        pushScope()
        val result = block()
        popScope()
        return result
    }

    protected fun pushScope() = scopes.addLast(mutableMapOf())
    protected fun popScope() = scopes.removeLastOrNull()

    protected fun putLocal(name: String, alloca: IRValue, type: KodeType) {
        scopes.lastOrNull()?.set(name, Symbol(alloca, type))
    }

    protected fun getLocal(name: String): Symbol? {
        return scopes.asReversed().firstNotNullOfOrNull { it[name] }
    }

    fun findSymbol(name: String): Symbol {
        return getLocal(name) ?: globals[name] ?: functions[name] ?: error("Variable $name not found")
    }

    // --- LLVM Helpers ---
    fun buildAlloca(name: String, llvmType: IRType): IRValue {
        val entry = currentFunction!!.basicBlocks.first()
        val oldBlock = builder.currentBlock
        builder.positionAtEnd(entry)
        
        val alloca = builder.buildAlloca(llvmType, name)
        // Move alloca to the beginning of the entry block, or at least before any non-alloca
        val insts = entry.instructions
        val added = insts.removeAt(insts.size - 1)
        val firstNonAlloca = insts.indexOfFirst { it !is IRInstruction.Alloca }
        if (firstNonAlloca == -1) {
            insts.add(added)
        } else {
            insts.add(firstNonAlloca, added)
        }

        if (oldBlock != null) builder.positionAtEnd(oldBlock)
        return alloca
    }

    private var nextBlockId = 0
    fun addBasicBlock(name: String): IRBasicBlock {
        val uniqueName = "$name.${nextBlockId++}"
        val bb = IRBasicBlock(uniqueName)
        currentFunction!!.basicBlocks.add(bb)
        return bb
    }

    // --- Type Helpers ---

    protected val voidType = IRType.Void
    protected val boolType = IRType.Int(1)
    protected val i32Type = IRType.Int(32)
    protected val i8Type = IRType.Int(8)
    protected val f32Type = IRType.Float
    protected val f64Type = IRType.Double

    // --- Constant Helpers ---
    fun Int.i32(): IRValue = IRIntConstant(this.toLong(), i32Type)
    fun Double.f64(): IRValue = IRFloatConstant(this, f64Type)

    val zero = 0.i32()

    fun getZero(type: IRType): IRValue {
        return when (type) {
            is IRType.Int -> IRIntConstant(0, type)
            is IRType.Float -> IRFloatConstant(0.0, type)
            is IRType.Double -> IRFloatConstant(0.0, type)
            is IRType.Pointer -> IRNullPointerConstant(type)
            else -> error("Zero not supported for $type")
        }
    }

    // --- Function Management ---
    fun ensureFunction(name: String, params: List<Param>, kodeType: KodeType.Fn, alien: Boolean) {
        if (functions.contains(name)) {
            return
        }
        val returnType = kodeType.ret.toIR()
        val paramTypes = kodeType.params.map { it.toIR() }
        
        val irParams = params.zip(paramTypes).map { (p, t) -> IRArgument(t, p.name) }
        val fn = IRFunction(name, returnType, irParams, isExternal = alien)
        
        module.functions.add(fn)
        functions[name] = Symbol(value = fn, kodeType)
    }

    // --- Struct Management ---
    fun getStructType(name: String): IRType.Struct? = structs[name]

    fun getStructFields(name: String): List<FieldDecl> = structFields[name] ?: error("Struct $name not found")

    fun registerStruct(name: String, type: IRType.Struct, fields: List<FieldDecl>? = null) {
        structs[name] = type
        module.namedStructs.add(type)
        fields?.let { structFields[name] = it }
    }

    // --- Global Management ---
    fun addGlobal(name: String, kodeType: KodeType) {
        val global = IRGlobalVariable(name, kodeType.toIR())
        module.globals.add(global)
        globals[name] = Symbol(global, kodeType)
    }

    fun KodeType.toIR(): IRType {
        return when (this) {
            is KodeType.I32 -> i32Type
            is KodeType.U8 -> i8Type
            is KodeType.F64 -> f64Type
            is KodeType.Void -> voidType
            is KodeType.Unknown -> error("Invalid type")
            is KodeType.Arr -> {
                dimensions.reversed().fold(base.toIR()) { ty, dim ->
                    IRType.Array(dim, ty)
                }
            }
            is KodeType.Obj -> getStructType(name) ?: error("Can't find struct $name")
            is KodeType.Fn -> IRType.Pointer(IRType.Function(ret.toIR(), params.map { it.toIR() }))
            is KodeType.Ptr -> (0 until levels).fold(initial = base.toIR()) { ty, _ ->
                IRType.Pointer(ty)
            }
        }
    }

    override fun close() {
    }
}
