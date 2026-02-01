package llvm

open class IRBuilder(val context: IRContext, val module: IRModule) {
    var currentBlock: IRBasicBlock? = null
    private var nextId = 0

    protected fun nextName(prefix: String = ""): String {
        val p = prefix.ifEmpty { "v" }
        return "$p.${nextId++}"
    }

    fun positionAtEnd(block: IRBasicBlock) {
        currentBlock = block
    }

    protected fun <T : IRInstruction> add(inst: T): T {
        currentBlock?.instructions?.add(inst) ?: error("No current block")
        return inst
    }

    fun buildAdd(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.Add, lhs, rhs, name)
    fun buildSub(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.Sub, lhs, rhs, name)
    fun buildMul(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.Mul, lhs, rhs, name)
    fun buildSDiv(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.SDiv, lhs, rhs, name)
    fun buildSRem(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.SRem, lhs, rhs, name)

    fun buildFAdd(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.FAdd, lhs, rhs, name)
    fun buildFSub(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.FSub, lhs, rhs, name)
    fun buildFMul(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.FMul, lhs, rhs, name)
    fun buildFDiv(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.FDiv, lhs, rhs, name)

    fun buildAnd(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.And, lhs, rhs, name)
    fun buildOr(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.Or, lhs, rhs, name)
    fun buildXor(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.Xor, lhs, rhs, name)
    fun buildShl(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.Shl, lhs, rhs, name)
    fun buildAShr(lhs: IRValue, rhs: IRValue, name: String = "") = binary(IRInstruction.Binary.Op.AShr, lhs, rhs, name)

    private fun binary(op: IRInstruction.Binary.Op, lhs: IRValue, rhs: IRValue, name: String) =
        add(IRInstruction.Binary(op, lhs, rhs, lhs.type, nextName(name)))

    fun buildNeg(value: IRValue, name: String = "neg"): IRValue {
        val zero = when (val type = value.type) {
            is IRType.Int -> IRIntConstant(0, type)
            else -> error("Negation not supported for $type")
        }
        return buildSub(zero, value, name)
    }

    fun buildFNeg(value: IRValue, name: String = "neg") = add(IRInstruction.Unary(IRInstruction.Unary.Op.FNeg, value, value.type, nextName(name)))

    fun buildNot(value: IRValue, name: String = "not"): IRValue {
        val minusOne = when (val type = value.type) {
            is IRType.Int -> IRIntConstant(-1, type)
            else -> error("Not not supported for $type")
        }
        return buildXor(value, minusOne, name)
    }

    fun buildAlloca(type: IRType, name: String = "") = add(IRInstruction.Alloca(type, nextName(name)))
    fun buildLoad(type: IRType, ptr: IRValue, name: String = "") = add(IRInstruction.Load(ptr, type, nextName(name)))
    fun buildStore(value: IRValue, ptr: IRValue) = add(IRInstruction.Store(value, ptr))

    fun buildGEP(baseType: IRType, ptr: IRValue, indices: List<IRValue>, name: String = "", inBounds: Boolean = true): IRValue {
        val finalType = when (baseType) {
            is IRType.Struct -> {
                // if indices are constant, we could find the type
                IRType.Pointer(IRType.Int(8)) // Simplified
            }
            is IRType.Array -> IRType.Pointer(baseType.elementType)
            else -> IRType.Pointer(baseType)
        }

        return add(IRInstruction.GEP(baseType, ptr, indices, finalType, nextName(name), inBounds))
    }

    fun buildCall(function: IRValue, args: List<IRValue>, name: String = ""): IRInstruction.Call {
        val fnType = when (val type = function.type) {
            is IRType.Function -> type
            is IRType.Pointer -> type.target as? IRType.Function ?: error("Not a function pointer: $type")
            else -> error("Not a function type: $type")
        }
        return add(IRInstruction.Call(fnType, function, args, nextName(name)))
    }

    fun buildRet(value: IRValue? = null) = add(IRInstruction.Ret(value))
    fun buildRetVoid() = add(IRInstruction.Ret(null))

    fun buildBr(dest: IRBasicBlock) = add(IRInstruction.Br(dest))
    fun buildCondBr(condition: IRValue, thenBlock: IRBasicBlock, elseBlock: IRBasicBlock) = add(IRInstruction.CondBr(condition, thenBlock, elseBlock))

    fun buildPhi(type: IRType, name: String = "") = add(IRInstruction.Phi(type, mutableListOf(), nextName(name)))

    fun buildGlobalStringPtr(value: String, name: String = "str"): IRValue {
        val strConst = IRStringConstant(value)
        val globalName = context.nextGlobalName(name)
        val global = IRGlobalVariable(globalName, strConst.type, strConst, isConstant = true)
        module.globals.add(global)
        
        val zero = IRIntConstant(0, IRType.Int(32))
        return buildGEP(strConst.type, global, listOf(zero, zero), name = "${name}_ptr")
    }

    fun buildICmp(pred: IRInstruction.ICmp.Predicate, lhs: IRValue, rhs: IRValue, name: String = "") = add(IRInstruction.ICmp(pred, lhs, rhs, nextName(name)))
    fun buildFCmp(pred: IRInstruction.FCmp.Predicate, lhs: IRValue, rhs: IRValue, name: String = "") = add(IRInstruction.FCmp(pred, lhs, rhs, nextName(name)))

    fun buildZExt(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.ZExt, value, type, name)
    fun buildSExt(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.SExt, value, type, name)
    fun buildTrunc(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.Trunc, value, type, name)
    fun buildFPToSI(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.FPToSI, value, type, name)
    fun buildSIToFP(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.SIToFP, value, type, name)
    fun buildBitCast(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.BitCast, value, type, name)
    fun buildPtrToInt(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.PtrToInt, value, type, name)
    fun buildIntToPtr(value: IRValue, type: IRType, name: String = "") = cast(IRInstruction.Cast.Op.IntToPtr, value, type, name)

    private fun cast(op: IRInstruction.Cast.Op, value: IRValue, type: IRType, name: String) =
        add(IRInstruction.Cast(op, value, type, nextName(name)))
}

class IRContext {
    private var nextGlobalId = 0
    fun nextGlobalName(prefix: String) = "$prefix.${nextGlobalId++}"

    fun createModule(name: String) = IRModule(name)

    // Type factory
    val void = IRType.Void
    val i1 = IRType.Int(1)
    val i8 = IRType.Int(8)
    val i16 = IRType.Int(16)
    val i32 = IRType.Int(32)
    val i64 = IRType.Int(64)
    val float = IRType.Float
    val double = IRType.Double

    fun int(bits: Int) = IRType.Int(bits)
    fun pointer(target: IRType) = IRType.Pointer(target)
    fun array(size: Int, elementType: IRType) = IRType.Array(size, elementType)
    fun function(returnType: IRType, parameterTypes: List<IRType>, isVarArg: Boolean = false) =
        IRType.Function(returnType, parameterTypes, isVarArg)
    fun struct(name: String?, elementTypes: List<IRType>, isPacked: Boolean = false) =
        IRType.Struct(name, elementTypes, isPacked)
}
