package llvm

class IRPrinter : IRVisitor<Unit> {
    private val sb = StringBuilder()
    private var indentLevel = 0

    fun print(module: IRModule): String {
        sb.setLength(0)
        visitModule(module)
        return sb.toString()
    }

    fun print(visitable: IRVisitable): String {
        sb.setLength(0)
        when (visitable) {
            is IRModule -> visitModule(visitable)
            is IRFunction -> visitFunction(visitable)
            is IRBasicBlock -> visitBasicBlock(visitable)
            is IRInstruction -> {
                visitable.accept(this)
            }
            is IRGlobalVariable -> visitGlobalVariable(visitable)
            is IRConstant -> visitConstant(visitable)
            is IRArgument -> sb.append(visitable.dump()) // Special case
        }
        return sb.toString()
    }

    private fun visitConstant(constant: IRConstant) {
        when (constant) {
            is IRIntConstant -> visitIntConstant(constant)
            is IRFloatConstant -> visitFloatConstant(constant)
            is IRNullPointerConstant -> visitNullPointer(constant)
            is IRArrayConstant -> visitArrayConstant(constant)
            is IRStringConstant -> visitStringConstant(constant)
        }
    }

    private fun indent() {
        sb.append("  ".repeat(indentLevel))
    }

    private fun line(text: String = "") {
        if (text.isNotEmpty()) indent()
        sb.append(text).append("\n")
    }

    override fun visitModule(module: IRModule) {
        module.namedStructs.forEach { struct ->
            line("%${struct.name} = type { ${struct.elementTypes.joinToString(", ")} }")
        }
        if (module.namedStructs.isNotEmpty()) line()

        module.globals.forEach { global ->
            visitGlobalVariable(global)
        }
        if (module.globals.isNotEmpty()) line()

        module.functions.forEach { function ->
            visitFunction(function)
            line()
        }
    }

    override fun visitFunction(function: IRFunction) {
        if (function.isExternal) {
            line("declare ${function.returnType} @${function.name}(${function.parameters.joinToString(", ") { it.dump() }})")
            return
        }

        line("define ${function.returnType} @${function.name}(${function.parameters.joinToString(", ") { it.dump() }}) {")
        function.basicBlocks.forEach { block ->
            visitBasicBlock(block)
        }
        line("}")
    }

    override fun visitBasicBlock(block: IRBasicBlock) {
        line("${block.name}:")
        indentLevel++
        block.instructions.forEach { inst ->
            indent()
            inst.accept(this)
            sb.append("\n")
        }
        indentLevel--
    }

    override fun visitGlobalVariable(global: IRGlobalVariable) {
        val kind = if (global.isConstant) "constant" else "global"
        val init = global.initializer?.let { " ${it.ref()}" } ?: " zeroinitializer"
        line("@${global.name} = $kind ${global.contentType}$init")
    }

    override fun visitArgument(argument: IRArgument) {
        // Handled in function definition/call
    }

    override fun visitIntConstant(constant: IRIntConstant) {
        sb.append(constant.value)
    }

    override fun visitFloatConstant(constant: IRFloatConstant) {
        sb.append(constant.value)
    }

    override fun visitNullPointer(constant: IRNullPointerConstant) {
        sb.append("null")
    }

    override fun visitArrayConstant(constant: IRArrayConstant) {
        sb.append("[${constant.elements.joinToString(", ") { "${it.type} ${it.ref()}" }}]")
    }

    override fun visitStringConstant(constant: IRStringConstant) {
        sb.append("c\"${constant.value.replace("\\", "\\\\").replace("\"", "\\22").replace("\n", "\\0A")}\\00\"")
    }

    override fun visitBinary(inst: IRInstruction.Binary) {
        sb.append("${inst.ref()} = ${inst.op.irName} ${inst.lhs.type} ${inst.lhs.ref()}, ${inst.rhs.ref()}")
    }

    override fun visitUnary(inst: IRInstruction.Unary) {
        sb.append("${inst.ref()} = ${inst.op.irName} ${inst.value.type} ${inst.value.ref()}")
    }

    override fun visitAlloca(inst: IRInstruction.Alloca) {
        sb.append("${inst.ref()} = alloca ${inst.allocatedType}")
    }

    override fun visitLoad(inst: IRInstruction.Load) {
        sb.append("${inst.ref()} = load ${inst.type}, ${inst.ptr.type} ${inst.ptr.ref()}")
    }

    override fun visitStore(inst: IRInstruction.Store) {
        sb.append("store ${inst.value.type} ${inst.value.ref()}, ${inst.ptr.type} ${inst.ptr.ref()}")
    }

    override fun visitGEP(inst: IRInstruction.GEP) {
        sb.append("${inst.ref()} = getelementptr ${if (inst.inBounds) "inbounds " else ""}${inst.baseType}, ${inst.ptr.type} ${inst.ptr.ref()}, ${
            inst.indices.joinToString(", ") { "${it.type} ${it.ref()}" }
        }")
    }

    override fun visitCall(inst: IRInstruction.Call) {
        val prefix = if (inst.type == IRType.Void) "" else "${inst.ref()} = "
        sb.append("${prefix}call ${inst.type} ${inst.function.ref()}(${inst.args.joinToString(", ") { "${it.type} ${it.ref()}" }})")
    }

    override fun visitRet(inst: IRInstruction.Ret) {
        sb.append("ret ${inst.value?.let { "${it.type} ${it.ref()}" } ?: "void"}")
    }

    override fun visitBr(inst: IRInstruction.Br) {
        sb.append("br label %${inst.dest.name}")
    }

    override fun visitCondBr(inst: IRInstruction.CondBr) {
        sb.append("br i1 ${inst.condition.ref()}, label %${inst.thenBlock.name}, label %${inst.elseBlock.name}")
    }

    override fun visitPhi(inst: IRInstruction.Phi) {
        sb.append("${inst.ref()} = phi ${inst.type} ${inst.incoming.joinToString(", ") { "[ ${it.first.ref()}, %${it.second.name} ]" }}")
    }

    override fun visitICmp(inst: IRInstruction.ICmp) {
        sb.append("${inst.ref()} = icmp ${inst.pred.irName} ${inst.lhs.type} ${inst.lhs.ref()}, ${inst.rhs.ref()}")
    }

    override fun visitFCmp(inst: IRInstruction.FCmp) {
        sb.append("${inst.ref()} = fcmp ${inst.pred.irName} ${inst.lhs.type} ${inst.lhs.ref()}, ${inst.rhs.ref()}")
    }

    override fun visitCast(inst: IRInstruction.Cast) {
        sb.append("${inst.ref()} = ${inst.op.irName} ${inst.value.type} ${inst.value.ref()} to ${inst.type}")
    }
}
