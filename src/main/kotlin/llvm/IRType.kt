package llvm

sealed class IRType {
    abstract override fun toString(): String

    object Void : IRType() {
        override fun toString() = "void"
    }

    data class Int(val bits: kotlin.Int) : IRType() {
        override fun toString() = "i$bits"
    }

    object Float : IRType() {
        override fun toString() = "float"
    }

    object Double : IRType() {
        override fun toString() = "double"
    }

    data class Pointer(val target: IRType) : IRType() {
        override fun toString() = "ptr"
    }

    data class Array(val size: kotlin.Int, val elementType: IRType) : IRType() {
        override fun toString() = "[$size x $elementType]"
    }

    data class Function(val returnType: IRType, val parameterTypes: List<IRType>, val isVarArg: Boolean = false) : IRType() {
        override fun toString(): String {
            val params = parameterTypes.joinToString(", ") { it.toString() }
            return "$returnType ($params${if (isVarArg) ", ..." else ""})"
        }
    }

    data class Struct(val name: String?, val elementTypes: List<IRType>, val isPacked: Boolean = false) : IRType() {
        override fun toString(): String {
            if (name != null) return "%$name"
            val elements = elementTypes.joinToString(", ") { it.toString() }
            return "{ $elements }"
        }
    }

    val isFloatLike: Boolean
        get() = this is Float || this is Double
}
