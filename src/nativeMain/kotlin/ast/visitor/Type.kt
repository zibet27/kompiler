package ast.visitor

/** Simple type model. */
sealed interface Type {
    data object I32 : Type
    data object U8 : Type
    data object F64 : Type
    data object Void : Type
    data class Ptr(val base: Type, val levels: Int) : Type
    data class Fn(val params: List<Type>, val ret: Type) : Type
    data class Obj(val name: String, val fields: Map<String, Type>, val isComplete: Boolean) : Type
    data object Unknown : Type
}

internal val Type.isSimple
    get() = isNumeric // the same for now

internal val Type.isNumeric
    get() = when (this) {
        Type.I32, Type.U8, Type.F64 -> true
        else -> false
    }

fun Type.canImplicitlyCastTo(other: Type): Boolean = when {
    this == other -> true
    this.isSimple && other.isSimple -> true
    this is Type.Ptr && other is Type.Ptr -> {
        val canBaseCast = base.canImplicitlyCastTo(other.base) || base == Type.Void || other.base == Type.Void
        levels == other.levels && canBaseCast
    }

    else -> false
}