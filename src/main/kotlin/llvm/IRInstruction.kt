package llvm

sealed class IRInstruction(type: IRType, name: String) : IRNamedValue(type, name) {
    fun dump(): String = IRPrinter().print(this).trim()
    abstract override fun <R> accept(visitor: IRVisitor<R>): R

    class Binary(val op: Op, val lhs: IRValue, val rhs: IRValue, type: IRType, name: String) :
        IRInstruction(type, name) {
        enum class Op(val irName: String) {
            Add("add"), Sub("sub"), Mul("mul"), SDiv("sdiv"), SRem("srem"),
            FAdd("fadd"), FSub("fsub"), FMul("fmul"), FDiv("fdiv"),
            And("and"), Or("or"), Xor("xor"), Shl("shl"), AShr("ashr")
        }

        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitBinary(this)
    }

    class Unary(val op: Op, val value: IRValue, type: IRType, name: String) : IRInstruction(type, name) {
        enum class Op(val irName: String) {
            FNeg("fneg")
        }

        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitUnary(this)
    }

    class Alloca(val allocatedType: IRType, name: String) : IRInstruction(IRType.Pointer(allocatedType), name) {
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitAlloca(this)
    }

    class Load(val ptr: IRValue, type: IRType, name: String) : IRInstruction(type, name) {
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitLoad(this)
    }

    class Store(val value: IRValue, val ptr: IRValue) : IRInstruction(IRType.Void, "") {
        override fun ref() = ""
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitStore(this)
    }

    class GEP(
        val baseType: IRType,
        val ptr: IRValue,
        val indices: List<IRValue>,
        type: IRType,
        name: String,
        val inBounds: Boolean = true
    ) : IRInstruction(type, name) {
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitGEP(this)
    }

    class Call(functionType: IRType.Function, val function: IRValue, val args: List<IRValue>, name: String) :
        IRInstruction(functionType.returnType, name) {
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitCall(this)
    }

    class Ret(val value: IRValue?) : IRInstruction(IRType.Void, "") {
        override fun ref() = ""
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitRet(this)
    }

    class Br(val dest: IRBasicBlock) : IRInstruction(IRType.Void, "") {
        override fun ref() = ""
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitBr(this)
    }

    class CondBr(val condition: IRValue, val thenBlock: IRBasicBlock, val elseBlock: IRBasicBlock) :
        IRInstruction(IRType.Void, "") {
        override fun ref() = ""
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitCondBr(this)
    }

    class Phi(type: IRType, val incoming: MutableList<Pair<IRValue, IRBasicBlock>>, name: String) :
        IRInstruction(type, name) {
        fun addIncoming(value: IRValue, block: IRBasicBlock) {
            incoming.add(value to block)
        }
        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitPhi(this)
    }

    class ICmp(val pred: Predicate, val lhs: IRValue, val rhs: IRValue, name: String) :
        IRInstruction(IRType.Int(1), name) {
        enum class Predicate(val irName: String) {
            EQ("eq"), NE("ne"), SGT("sgt"), SGE("sge"), SLT("slt"), SLE("sle"),
            UGT("ugt"), UGE("uge"), ULT("ult"), ULE("ule")
        }

        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitICmp(this)
    }

    class FCmp(val pred: Predicate, val lhs: IRValue, val rhs: IRValue, name: String) :
        IRInstruction(IRType.Int(1), name) {
        enum class Predicate(val irName: String) {
            OEQ("oeq"), ONE("one"), OGT("ogt"), OGE("oge"), OLT("olt"), OLE("ole"),
            UNE("une"), UEQ("ueq")
        }

        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitFCmp(this)
    }

    class Cast(val op: Op, val value: IRValue, type: IRType, name: String) : IRInstruction(type, name) {
        enum class Op(val irName: String) {
            ZExt("zext"), SExt("sext"), Trunc("trunc"),
            FPToSI("fptosi"), SIToFP("sitofp"),
            BitCast("bitcast"), PtrToInt("ptrtoint"), IntToPtr("inttoptr")
        }

        override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitCast(this)
    }
}
