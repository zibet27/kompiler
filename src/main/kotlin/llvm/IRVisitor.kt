package llvm

interface IRVisitor<R> {
    fun visitModule(module: IRModule): R
    fun visitFunction(function: IRFunction): R
    fun visitBasicBlock(block: IRBasicBlock): R

    // Values
    fun visitGlobalVariable(global: IRGlobalVariable): R
    fun visitArgument(argument: IRArgument): R

    // Constants
    fun visitIntConstant(constant: IRIntConstant): R
    fun visitFloatConstant(constant: IRFloatConstant): R
    fun visitNullPointer(constant: IRNullPointerConstant): R
    fun visitArrayConstant(constant: IRArrayConstant): R
    fun visitStringConstant(constant: IRStringConstant): R

    // Instructions
    fun visitBinary(inst: IRInstruction.Binary): R
    fun visitUnary(inst: IRInstruction.Unary): R
    fun visitAlloca(inst: IRInstruction.Alloca): R
    fun visitLoad(inst: IRInstruction.Load): R
    fun visitStore(inst: IRInstruction.Store): R
    fun visitGEP(inst: IRInstruction.GEP): R
    fun visitCall(inst: IRInstruction.Call): R
    fun visitRet(inst: IRInstruction.Ret): R
    fun visitBr(inst: IRInstruction.Br): R
    fun visitCondBr(inst: IRInstruction.CondBr): R
    fun visitPhi(inst: IRInstruction.Phi): R
    fun visitICmp(inst: IRInstruction.ICmp): R
    fun visitFCmp(inst: IRInstruction.FCmp): R
    fun visitCast(inst: IRInstruction.Cast): R
}

interface IRVisitable {
    fun <R> accept(visitor: IRVisitor<R>): R
}
