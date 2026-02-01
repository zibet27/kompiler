package type

/**
 * Kode type model.
 */
sealed interface KodeType {
    data object I32 : KodeType
    data object U8 : KodeType
    data object F64 : KodeType
    data object Void : KodeType
    data class Ptr(val base: KodeType, val levels: Int) : KodeType {
        init {
            require(levels > 0) { "Pointer level must be positive." }
        }

        fun referenced(): KodeType = when (levels) {
            1 -> base
            else -> Ptr(base, levels - 1)
        }
    }

    data class Fn(val params: List<KodeType>, val ret: KodeType) : KodeType
    data class Obj(val name: String, val fields: List<Pair<String, KodeType>>, val isComplete: Boolean) : KodeType

    data class Arr(val base: KodeType, val dimensions: List<Int>) : KodeType {
        init {
            require(dimensions.isNotEmpty()) { "Array type must have at least one dimension" }
        }

        fun totalSize(): Int = dimensions.fold(1) { acc, dim -> acc * dim }

        fun indexed(): KodeType = when (dimensions.size) {
            1 -> base
            else -> Arr(base, dimensions.subList(1, dimensions.size))
        }
    }

    object Unknown : KodeType
}

fun KodeType.withDimensions(dimensions: List<Int>): KodeType = when {
    dimensions.isEmpty() -> this
    this is KodeType.Arr -> KodeType.Arr(base, this.dimensions + dimensions)
    else -> KodeType.Arr(base = this, dimensions)
}

val KodeType.isNumeric
    get() = when (this) {
        KodeType.I32, KodeType.U8, KodeType.F64 -> true
        else -> false
    }

val KodeType.isFloatLike get() = this == KodeType.F64

fun KodeType.canImplicitlyCastTo(other: KodeType): Boolean = when {
    this == other -> true
    this is KodeType.Ptr && other is KodeType.Ptr -> {
        val canCastBase = base.canImplicitlyCastTo(other.base) || base == KodeType.Void || other.base == KodeType.Void
        levels == other.levels && canCastBase
    }

    else -> false
}

fun KodeType.canCastTo(other: KodeType): Boolean = when {
    this == other -> true
    this.isNumeric && other.isNumeric -> true
    this is KodeType.Ptr && other is KodeType.Ptr -> true
    else -> false
}