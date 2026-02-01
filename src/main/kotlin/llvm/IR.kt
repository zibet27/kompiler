package llvm

sealed interface IRValue : IRVisitable {
    val type: IRType
    val name: String?

    fun ref(): String
}

class IRModule(val name: String) : IRVisitable {
    val globals = mutableListOf<IRGlobalVariable>()
    val functions = mutableListOf<IRFunction>()
    val namedStructs = mutableListOf<IRType.Struct>()

    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitModule(this)

    fun dump(): String = IRPrinter().print(this)
}

class IRFunction(
    override val name: String,
    val returnType: IRType,
    val parameters: List<IRArgument>,
    var isExternal: Boolean = false
) : IRValue {
    override val type: IRType get() = IRType.Pointer(IRType.Function(returnType, parameters.map { it.type }))

    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitFunction(this)

    val basicBlocks = mutableListOf<IRBasicBlock>()

    override fun ref() = "@$name"

    fun dump(): String {
        val printer = IRPrinter()
        val module = IRModule("temp")
        module.functions.add(this)
        return printer.print(module).trim()
    }
}

class IRArgument(override val type: IRType, override val name: String) : IRValue {
    override fun ref() = "%$name"
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitArgument(this)
    fun dump() = "$type %$name"
}

class IRBasicBlock(val name: String) : IRVisitable {
    val instructions = mutableListOf<IRInstruction>()

    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitBasicBlock(this)

    val terminator: IRInstruction?
        get() = instructions.lastOrNull()?.let {
            if (it is IRInstruction.Ret || it is IRInstruction.Br || it is IRInstruction.CondBr) it else null
        }

    fun dump(): String = IRPrinter().print(this).trim()
}

class IRGlobalVariable(
    override val name: String,
    val contentType: IRType,
    var initializer: IRValue? = null,
    val isConstant: Boolean = false
) : IRValue {
    override val type: IRType get() = IRType.Pointer(contentType)
    override fun ref() = "@$name"
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitGlobalVariable(this)

    fun dump(): String = IRPrinter().print(this).trim()
}

sealed class IRConstant(override val type: IRType) : IRValue {
    override val name: String? get() = null
}

class IRIntConstant(val value: Long, type: IRType.Int) : IRConstant(type) {
    override fun ref() = value.toString()
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitIntConstant(this)
}

class IRFloatConstant(val value: Double, type: IRType) : IRConstant(type) {
    override fun ref(): String {
        // LLVM expects hex representation for doubles sometimes, but let's start with a simple string
        return value.toString()
    }
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitFloatConstant(this)
}

class IRNullPointerConstant(type: IRType.Pointer) : IRConstant(type) {
    override fun ref() = "null"
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitNullPointer(this)
}

class IRArrayConstant(val elements: List<IRValue>, type: IRType.Array) : IRConstant(type) {
    override fun ref() = "[${elements.joinToString(", ") { "${it.type} ${it.ref()}" }}]"
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitArrayConstant(this)
}

class IRStringConstant(val value: String) : IRConstant(IRType.Array(value.length + 1, IRType.Int(8))) {
    // This is a bit tricky in LLVM IR, usually handled as a global constant array
    override fun ref() = "c\"${value.replace("\\", "\\\\").replace("\"", "\\22").replace("\n", "\\0A")}\\00\""
    override fun <R> accept(visitor: IRVisitor<R>): R = visitor.visitStringConstant(this)
}

// Named IR values (instructions, arguments)
abstract class IRNamedValue(override val type: IRType, override var name: String) : IRValue {
    override fun ref() = "%$name"
}
