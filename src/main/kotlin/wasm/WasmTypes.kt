package wasm

import llvm.IRType

/**
 * WebAssembly value types
 */
enum class WasmValType(val code: Int) {
    I32(0x7F),
    I64(0x7E),
    F32(0x7D),
    F64(0x7C),
    V128(0x7B),      // SIMD (not used yet)
    FUNCREF(0x70),   // Reference types
    EXTERNREF(0x6F); // Reference types

    companion object {
        /**
         * Convert LLVM IR type to Wasm value type
         */
        fun fromIRType(type: IRType): WasmValType {
            return when (type) {
                is IRType.Int -> when {
                    type.bits <= 32 -> I32
                    type.bits <= 64 -> I64
                    else -> error("Unsupported integer size: ${type.bits}")
                }
                is IRType.Float -> F32
                is IRType.Double -> F64
                is IRType.Pointer -> I32  // Pointers are i32 in linear memory model
                is IRType.Struct -> I32   // Structs are passed as pointers (i32) in Wasm
                is IRType.Array -> I32    // Arrays are passed as pointers (i32) in Wasm
                is IRType.Void -> error("Void is not a value type")
                else -> error("Cannot convert $type to Wasm value type")
            }
        }
    }
}

/**
 * WebAssembly block types
 */
sealed class WasmBlockType {
    object Empty : WasmBlockType()
    data class Value(val type: WasmValType) : WasmBlockType()
    data class FuncType(val typeIndex: Int) : WasmBlockType()

    fun encode(emitter: WasmEmitter) {
        when (this) {
            is Empty -> emitter.writeByte(0x40)
            is Value -> emitter.writeByte(type.code)
            is FuncType -> emitter.writeS32Leb(typeIndex)
        }
    }
}

/**
 * WebAssembly function type
 */
data class WasmFuncType(
    val params: List<WasmValType>,
    val results: List<WasmValType>
) {
    fun encode(emitter: WasmEmitter) {
        emitter.writeByte(0x60) // function type code
        emitter.writeVector(params) { type -> writeByte(type.code) }
        emitter.writeVector(results) { type -> writeByte(type.code) }
    }

    companion object {
        fun fromIRFunction(returnType: IRType, paramTypes: List<IRType>): WasmFuncType {
            val params = paramTypes.map { WasmValType.fromIRType(it) }
            val results = if (returnType is IRType.Void) {
                emptyList()
            } else {
                listOf(WasmValType.fromIRType(returnType))
            }
            return WasmFuncType(params, results)
        }
    }
}

/**
 * WebAssembly opcodes
 * Reference: https://webassembly.github.io/spec/core/binary/instructions.html
 */
object WasmOp {
    // Control flow
    const val UNREACHABLE = 0x00
    const val NOP = 0x01
    const val BLOCK = 0x02
    const val LOOP = 0x03
    const val IF = 0x04
    const val ELSE = 0x05
    const val END = 0x0B
    const val BR = 0x0C
    const val BR_IF = 0x0D
    const val BR_TABLE = 0x0E
    const val RETURN = 0x0F

    // Call operators
    const val CALL = 0x10
    const val CALL_INDIRECT = 0x11

    // Parametric operators
    const val DROP = 0x1A
    const val SELECT = 0x1B

    // Variable access
    const val LOCAL_GET = 0x20
    const val LOCAL_SET = 0x21
    const val LOCAL_TEE = 0x22
    const val GLOBAL_GET = 0x23
    const val GLOBAL_SET = 0x24

    // Memory instructions
    const val I32_LOAD = 0x28
    const val I64_LOAD = 0x29
    const val F32_LOAD = 0x2A
    const val F64_LOAD = 0x2B
    const val I32_LOAD8_S = 0x2C
    const val I32_LOAD8_U = 0x2D
    const val I32_LOAD16_S = 0x2E
    const val I32_LOAD16_U = 0x2F
    const val I64_LOAD8_S = 0x30
    const val I64_LOAD8_U = 0x31
    const val I64_LOAD16_S = 0x32
    const val I64_LOAD16_U = 0x33
    const val I64_LOAD32_S = 0x34
    const val I64_LOAD32_U = 0x35

    const val I32_STORE = 0x36
    const val I64_STORE = 0x37
    const val F32_STORE = 0x38
    const val F64_STORE = 0x39
    const val I32_STORE8 = 0x3A
    const val I32_STORE16 = 0x3B
    const val I64_STORE8 = 0x3C
    const val I64_STORE16 = 0x3D
    const val I64_STORE32 = 0x3E

    const val MEMORY_SIZE = 0x3F
    const val MEMORY_GROW = 0x40

    // Numeric constants
    const val I32_CONST = 0x41
    const val I64_CONST = 0x42
    const val F32_CONST = 0x43
    const val F64_CONST = 0x44

    // i32 operations
    const val I32_EQZ = 0x45
    const val I32_EQ = 0x46
    const val I32_NE = 0x47
    const val I32_LT_S = 0x48
    const val I32_LT_U = 0x49
    const val I32_GT_S = 0x4A
    const val I32_GT_U = 0x4B
    const val I32_LE_S = 0x4C
    const val I32_LE_U = 0x4D
    const val I32_GE_S = 0x4E
    const val I32_GE_U = 0x4F

    // i64 operations
    const val I64_EQZ = 0x50
    const val I64_EQ = 0x51
    const val I64_NE = 0x52
    const val I64_LT_S = 0x53
    const val I64_LT_U = 0x54
    const val I64_GT_S = 0x55
    const val I64_GT_U = 0x56
    const val I64_LE_S = 0x57
    const val I64_LE_U = 0x58
    const val I64_GE_S = 0x59
    const val I64_GE_U = 0x5A

    // f32 operations
    const val F32_EQ = 0x5B
    const val F32_NE = 0x5C
    const val F32_LT = 0x5D
    const val F32_GT = 0x5E
    const val F32_LE = 0x5F
    const val F32_GE = 0x60

    // f64 operations
    const val F64_EQ = 0x61
    const val F64_NE = 0x62
    const val F64_LT = 0x63
    const val F64_GT = 0x64
    const val F64_LE = 0x65
    const val F64_GE = 0x66

    // i32 numeric operations
    const val I32_CLZ = 0x67
    const val I32_CTZ = 0x68
    const val I32_POPCNT = 0x69
    const val I32_ADD = 0x6A
    const val I32_SUB = 0x6B
    const val I32_MUL = 0x6C
    const val I32_DIV_S = 0x6D
    const val I32_DIV_U = 0x6E
    const val I32_REM_S = 0x6F
    const val I32_REM_U = 0x70
    const val I32_AND = 0x71
    const val I32_OR = 0x72
    const val I32_XOR = 0x73
    const val I32_SHL = 0x74
    const val I32_SHR_S = 0x75
    const val I32_SHR_U = 0x76
    const val I32_ROTL = 0x77
    const val I32_ROTR = 0x78

    // i64 numeric operations
    const val I64_CLZ = 0x79
    const val I64_CTZ = 0x7A
    const val I64_POPCNT = 0x7B
    const val I64_ADD = 0x7C
    const val I64_SUB = 0x7D
    const val I64_MUL = 0x7E
    const val I64_DIV_S = 0x7F
    const val I64_DIV_U = 0x80
    const val I64_REM_S = 0x81
    const val I64_REM_U = 0x82
    const val I64_AND = 0x83
    const val I64_OR = 0x84
    const val I64_XOR = 0x85
    const val I64_SHL = 0x86
    const val I64_SHR_S = 0x87
    const val I64_SHR_U = 0x88
    const val I64_ROTR = 0x89
    const val I64_ROTL = 0x8A

    // f32 numeric operations
    const val F32_ABS = 0x8B
    const val F32_NEG = 0x8C
    const val F32_CEIL = 0x8D
    const val F32_FLOOR = 0x8E
    const val F32_TRUNC = 0x8F
    const val F32_NEAREST = 0x90
    const val F32_SQRT = 0x91
    const val F32_ADD = 0x92
    const val F32_SUB = 0x93
    const val F32_MUL = 0x94
    const val F32_DIV = 0x95
    const val F32_MIN = 0x96
    const val F32_MAX = 0x97
    const val F32_COPYSIGN = 0x98

    // f64 numeric operations
    const val F64_ABS = 0x99
    const val F64_NEG = 0x9A
    const val F64_CEIL = 0x9B
    const val F64_FLOOR = 0x9C
    const val F64_TRUNC = 0x9D
    const val F64_NEAREST = 0x9E
    const val F64_SQRT = 0x9F
    const val F64_ADD = 0xA0
    const val F64_SUB = 0xA1
    const val F64_MUL = 0xA2
    const val F64_DIV = 0xA3
    const val F64_MIN = 0xA4
    const val F64_MAX = 0xA5
    const val F64_COPYSIGN = 0xA6

    // Conversions
    const val I32_WRAP_I64 = 0xA7
    const val I32_TRUNC_F32_S = 0xA8
    const val I32_TRUNC_F32_U = 0xA9
    const val I32_TRUNC_F64_S = 0xAA
    const val I32_TRUNC_F64_U = 0xAB
    const val I64_EXTEND_I32_S = 0xAC
    const val I64_EXTEND_I32_U = 0xAD
    const val I64_TRUNC_F32_S = 0xAE
    const val I64_TRUNC_F32_U = 0xAF
    const val I64_TRUNC_F64_S = 0xB0
    const val I64_TRUNC_F64_U = 0xB1
    const val F32_CONVERT_I32_S = 0xB2
    const val F32_CONVERT_I32_U = 0xB3
    const val F32_CONVERT_I64_S = 0xB4
    const val F32_CONVERT_I64_U = 0xB5
    const val F32_DEMOTE_F64 = 0xB6
    const val F64_CONVERT_I32_S = 0xB7
    const val F64_CONVERT_I32_U = 0xB8
    const val F64_CONVERT_I64_S = 0xB9
    const val F64_CONVERT_I64_U = 0xBA
    const val F64_PROMOTE_F32 = 0xBB
    const val I32_REINTERPRET_F32 = 0xBC
    const val I64_REINTERPRET_F64 = 0xBD
    const val F32_REINTERPRET_I32 = 0xBE
    const val F64_REINTERPRET_I64 = 0xBF

    // Sign extension (newer proposal)
    const val I32_EXTEND8_S = 0xC0
    const val I32_EXTEND16_S = 0xC1
    const val I64_EXTEND8_S = 0xC2
    const val I64_EXTEND16_S = 0xC3
    const val I64_EXTEND32_S = 0xC4
}

/**
 * WebAssembly limits (for memory and tables)
 */
data class WasmLimits(val min: Int, val max: Int? = null) {
    fun encode(emitter: WasmEmitter) {
        if (max != null) {
            emitter.writeByte(0x01) // has maximum
            emitter.writeU32Leb(min)
            emitter.writeU32Leb(max)
        } else {
            emitter.writeByte(0x00) // no maximum
            emitter.writeU32Leb(min)
        }
    }
}

/**
 * WebAssembly table type (used for function tables)
 */
class WasmTableType

/**
 * WebAssembly export kind
 */
enum class WasmExportKind(val code: Int) {
    FUNC(0x00),
    TABLE(0x01),
    MEMORY(0x02),
    GLOBAL(0x03)
}
