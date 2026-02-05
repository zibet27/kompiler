package wasm

import llvm.*

/**
 * Emits WebAssembly instructions from LLVM IR instructions.
 *
 * This class handles the translation of individual LLVM instructions to Wasm bytecode.
 * It works in conjunction with LocalsManager to resolve SSA values to local indices.
 */
class InstructionEmitter(
    private val emitter: WasmEmitter,
    private val localsManager: LocalsManager,
    private val functionIndexMap: Map<IRFunction, Int>,
    private val globalAddressMap: Map<IRGlobalVariable, Int>,
    private val typeIndexMap: Map<WasmFuncType, Int>,
    stackFrameStart: Int = 1024,
) {
    // Simple stack allocator for alloca instructions
    // Each function gets its own stack frame starting at stackFrameStart
    private var stackPointer = stackFrameStart
    private val allocaAddresses = mutableMapOf<IRInstruction.Alloca, Int>()

    /**
     * Emit an instruction and store its result in the appropriate local
     */
    fun emitInstruction(instruction: IRInstruction) {
        when (instruction) {
            is IRInstruction.Binary -> emitBinary(instruction)
            is IRInstruction.Unary -> emitUnary(instruction)
            is IRInstruction.Alloca -> emitAlloca(instruction)
            is IRInstruction.Load -> emitLoad(instruction)
            is IRInstruction.Store -> emitStore(instruction)
            is IRInstruction.GEP -> emitGEP(instruction)
            is IRInstruction.Call -> emitCall(instruction)
            is IRInstruction.ICmp -> emitICmp(instruction)
            is IRInstruction.FCmp -> emitFCmp(instruction)
            is IRInstruction.Cast -> emitCast(instruction)
            is IRInstruction.Phi -> {
                // PHI nodes should have been resolved already
                error("PHI node should have been resolved: ${instruction.dump()}")
            }
            // Terminators are handled separately by the Relooper
            is IRInstruction.Ret,
            is IRInstruction.Br,
            is IRInstruction.CondBr -> {
                // Skip, handled by Relooper
            }
        }
    }

    /**
     * Emit code to load a value onto the stack.
     * If it's a constant, emit the constant.
     * If it's an SSA value, load it from its local.
     */
    fun emitValue(value: IRValue) {
        when (value) {
            is IRIntConstant -> {
                // Emit constant
                when (value.type) {
                    is IRType.Int -> {
                        if (value.type.bits <= 32) {
                            emitter.writeByte(WasmOp.I32_CONST)
                            emitter.writeS32Leb(value.value.toInt())
                        } else {
                            emitter.writeByte(WasmOp.I64_CONST)
                            emitter.writeS64Leb(value.value)
                        }
                    }

                    else -> error("Unexpected constant type: ${value.type}")
                }
            }

            is IRFloatConstant -> {
                when (value.type) {
                    is IRType.Float -> {
                        emitter.writeByte(WasmOp.F32_CONST)
                        emitter.writeF32(value.value.toFloat())
                    }

                    is IRType.Double -> {
                        emitter.writeByte(WasmOp.F64_CONST)
                        emitter.writeF64(value.value)
                    }

                    else -> error("Unexpected constant type: ${value.type}")
                }
            }

            is IRNullPointerConstant -> {
                // Null pointer = 0 in linear memory model
                emitter.writeByte(WasmOp.I32_CONST)
                emitter.writeS32Leb(0)
            }

            is IRGlobalVariable -> {
                // Global variables - emit their memory address
                val address = globalAddressMap[value] ?: error("Global variable ${value.name} not found in address map")
                emitter.writeByte(WasmOp.I32_CONST)
                emitter.writeS32Leb(address)
            }

            is IRFunction -> {
                // Function references - emit their function index as i32
                val funcIndex = functionIndexMap[value] ?: error("Function ${value.name} not found in index map")
                emitter.writeByte(WasmOp.I32_CONST)
                emitter.writeS32Leb(funcIndex)
            }

            else -> {
                // Try to load from local if available
                if (localsManager.hasLocal(value)) {
                    val localIndex = localsManager.getLocal(value)
                    emitter.writeByte(WasmOp.LOCAL_GET)
                    emitter.writeU32Leb(localIndex)
                } else {
                    error("Value ${value.ref()} not allocated as local and not a constant")
                }
            }
        }
    }

    /**
     * Emit code to store the value on top of stack into a local
     */
    private fun emitLocalSet(value: IRValue) {
        val localIndex = localsManager.getLocal(value)
        emitter.writeByte(WasmOp.LOCAL_SET)
        emitter.writeU32Leb(localIndex)
    }

    private fun emitBinary(inst: IRInstruction.Binary) {
        // Push operands
        emitValue(inst.lhs)
        emitValue(inst.rhs)

        // Emit operation
        val isFloat = inst.type.isFloatLike
        val is64 = (inst.type as? IRType.Int)?.bits == 64

        val opcode = when (inst.op) {
            IRInstruction.Binary.Op.Add -> if (isFloat) if (is64) WasmOp.F64_ADD else WasmOp.F32_ADD else if (is64) WasmOp.I64_ADD else WasmOp.I32_ADD
            IRInstruction.Binary.Op.Sub -> if (isFloat) if (is64) WasmOp.F64_SUB else WasmOp.F32_SUB else if (is64) WasmOp.I64_SUB else WasmOp.I32_SUB
            IRInstruction.Binary.Op.Mul -> if (isFloat) if (is64) WasmOp.F64_MUL else WasmOp.F32_MUL else if (is64) WasmOp.I64_MUL else WasmOp.I32_MUL
            IRInstruction.Binary.Op.SDiv -> if (isFloat) if (is64) WasmOp.F64_DIV else WasmOp.F32_DIV else if (is64) WasmOp.I64_DIV_S else WasmOp.I32_DIV_S
            IRInstruction.Binary.Op.SRem -> if (is64) WasmOp.I64_REM_S else WasmOp.I32_REM_S
            IRInstruction.Binary.Op.FAdd -> if (is64) WasmOp.F64_ADD else WasmOp.F32_ADD
            IRInstruction.Binary.Op.FSub -> if (is64) WasmOp.F64_SUB else WasmOp.F32_SUB
            IRInstruction.Binary.Op.FMul -> if (is64) WasmOp.F64_MUL else WasmOp.F32_MUL
            IRInstruction.Binary.Op.FDiv -> if (is64) WasmOp.F64_DIV else WasmOp.F32_DIV
            IRInstruction.Binary.Op.And -> if (is64) WasmOp.I64_AND else WasmOp.I32_AND
            IRInstruction.Binary.Op.Or -> if (is64) WasmOp.I64_OR else WasmOp.I32_OR
            IRInstruction.Binary.Op.Xor -> if (is64) WasmOp.I64_XOR else WasmOp.I32_XOR
            IRInstruction.Binary.Op.Shl -> if (is64) WasmOp.I64_SHL else WasmOp.I32_SHL
            IRInstruction.Binary.Op.AShr -> if (is64) WasmOp.I64_SHR_S else WasmOp.I32_SHR_S
        }
        emitter.writeByte(opcode)

        // Store result
        emitLocalSet(inst)
    }

    private fun emitUnary(inst: IRInstruction.Unary) {
        emitValue(inst.value)

        val opcode = when (inst.op) {
            IRInstruction.Unary.Op.FNeg -> {
                when (inst.type) {
                    is IRType.Float -> WasmOp.F32_NEG
                    is IRType.Double -> WasmOp.F64_NEG
                    else -> error("FNeg on non-float type")
                }
            }
        }
        emitter.writeByte(opcode)

        emitLocalSet(inst)
    }

    private fun emitAlloca(inst: IRInstruction.Alloca) {
        // Allocate space on our simple linear stack
        val address = stackPointer

        // Calculate size needed for this type
        val size = when (val type = inst.allocatedType) {
            is IRType.Int -> (type.bits + 7) / 8  // Round up to bytes
            is IRType.Float -> 4
            is IRType.Double -> 8
            is IRType.Pointer -> 4  // 32-bit pointers
            is IRType.Array -> {
                val elementSize = calculateElementSize(type.elementType)
                type.size * elementSize
            }

            is IRType.Struct -> calculateStructSize(type)
            else -> error("Cannot allocate unsupported type: $type")
        }

        // Align to 4-byte boundary
        val alignedSize = (size + 3) and 3.inv()

        // Store the address for this alloca
        allocaAddresses[inst] = address
        stackPointer += alignedSize

        // Emit the address as a constant into the local
        emitter.writeByte(WasmOp.I32_CONST)
        emitter.writeS32Leb(address)
        emitLocalSet(inst)
    }

    private fun calculateElementSize(type: IRType): Int {
        return when (type) {
            is IRType.Int -> (type.bits + 7) / 8
            is IRType.Float -> 4
            is IRType.Double -> 8
            is IRType.Pointer -> 4
            is IRType.Array -> {
                val elementSize = calculateElementSize(type.elementType)
                type.size * elementSize
            }

            is IRType.Struct -> calculateStructSize(type)
            else -> error("Cannot calculate size for unsupported type: $type")
        }
    }

    private fun calculateStructSize(structType: IRType.Struct): Int {
        var size = 0
        for (elementType in structType.elementTypes) {
            size += calculateElementSize(elementType)
        }
        return size
    }

    private fun emitLoad(inst: IRInstruction.Load) {
        // Special handling for struct loads - just pass the address
        // In WebAssembly, structs are always accessed by pointer
        if (inst.type is IRType.Struct) {
            emitValue(inst.ptr)
            emitLocalSet(inst)
            return
        }

        // Load from linear memory
        emitValue(inst.ptr)

        val opcode = when (inst.type) {
            is IRType.Int -> {
                when (inst.type.bits) {
                    8 -> WasmOp.I32_LOAD8_U
                    16 -> WasmOp.I32_LOAD16_U
                    32 -> WasmOp.I32_LOAD
                    64 -> WasmOp.I64_LOAD
                    else -> error("Unsupported int size for load")
                }
            }

            is IRType.Float -> WasmOp.F32_LOAD
            is IRType.Double -> WasmOp.F64_LOAD
            is IRType.Pointer -> WasmOp.I32_LOAD
            is IRType.Array -> WasmOp.I32_LOAD   // Arrays loaded as pointer addresses
            else -> error("Unsupported type for load: ${inst.type}")
        }

        emitter.writeByte(opcode)
        // Alignment (as power of 2) - using natural alignment
        val alignment = calculateAlignment(inst.type)
        emitter.writeU32Leb(alignment)
        // Offset
        emitter.writeU32Leb(0)

        emitLocalSet(inst)
    }

    private fun emitStore(inst: IRInstruction.Store) {
        // Special handling for struct stores - byte-by-byte copy
        if (inst.value.type is IRType.Struct) {
            val structType = inst.value.type as IRType.Struct
            val size = calculateStructSize(structType)

            // Copy byte by byte: for each byte, load from src and store to dest
            for (byteOffset in 0 until size) {
                // Push dest address + offset
                emitValue(inst.ptr)
                if (byteOffset > 0) {
                    emitter.writeByte(WasmOp.I32_CONST)
                    emitter.writeS32Leb(byteOffset)
                    emitter.writeByte(WasmOp.I32_ADD)
                }

                // Push src address + offset
                emitValue(inst.value)
                if (byteOffset > 0) {
                    emitter.writeByte(WasmOp.I32_CONST)
                    emitter.writeS32Leb(byteOffset)
                    emitter.writeByte(WasmOp.I32_ADD)
                }

                // Load byte from src (consumes src address, leaves byte value)
                emitter.writeByte(WasmOp.I32_LOAD8_U)
                emitter.writeU32Leb(0)  // alignment
                emitter.writeU32Leb(0)  // offset

                // Now stack is: [dest_addr, byte_value]
                // Store byte to dest (consumes both)
                emitter.writeByte(WasmOp.I32_STORE8)
                emitter.writeU32Leb(0)  // alignment
                emitter.writeU32Leb(0)  // offset
            }
            return
        }

        // Push address
        emitValue(inst.ptr)
        // Push value
        emitValue(inst.value)

        val opcode = when (inst.value.type) {
            is IRType.Int -> {
                when ((inst.value.type as IRType.Int).bits) {
                    8 -> WasmOp.I32_STORE8
                    16 -> WasmOp.I32_STORE16
                    32 -> WasmOp.I32_STORE
                    64 -> WasmOp.I64_STORE
                    else -> error("Unsupported int size for store")
                }
            }

            is IRType.Float -> WasmOp.F32_STORE
            is IRType.Double -> WasmOp.F64_STORE
            is IRType.Pointer -> WasmOp.I32_STORE
            is IRType.Array -> WasmOp.I32_STORE   // Arrays stored as pointer addresses
            else -> error("Unsupported type for store: ${inst.value.type}")
        }

        emitter.writeByte(opcode)
        // Alignment
        val alignment = calculateAlignment(inst.value.type)
        emitter.writeU32Leb(alignment)
        // Offset
        emitter.writeU32Leb(0)
    }

    private fun calculateAlignment(type: IRType): Int {
        // Return alignment as power of 2
        return when (type) {
            is IRType.Int -> when {
                (type.bits / 8) >= 4 -> 2  // 4-byte align
                (type.bits / 8) >= 2 -> 1  // 2-byte align
                else -> 0                   // 1-byte align
            }

            is IRType.Float -> 2     // 4-byte align
            is IRType.Double -> 3    // 8-byte align
            is IRType.Pointer -> 2   // 4-byte align
            is IRType.Array -> 2     // 4-byte align
            else -> error("Cannot calculate alignment for type: $type")
        }
    }

    private fun calculateFieldOffset(structType: IRType.Struct, fieldIndex: Int): Int {
        var offset = 0
        for (i in 0 until fieldIndex) {
            offset += calculateElementSize(structType.elementTypes[i])
        }
        return offset
    }

    private fun emitGEP(inst: IRInstruction.GEP) {
        // GEP calculates pointer arithmetic
        // Syntax: getelementptr BaseType, ptr Ptr, indices...
        // First index: array/pointer offset (how many BaseType elements to skip)
        // Subsequent indices: field/element offsets within the type

        // Start with base pointer
        emitValue(inst.ptr)

        var currentType = inst.baseType

        for ((i, indexValue) in inst.indices.withIndex()) {
            if (i == 0) {
                // First index: pointer/array indexing
                // Skip baseType elements
                if (indexValue is IRIntConstant && indexValue.value == 0L) {
                    // Offset of 0, no-op
                    continue
                }

                val elementSize = calculateElementSize(currentType)

                emitValue(indexValue)
                if (elementSize != 1) {
                    emitter.writeByte(WasmOp.I32_CONST)
                    emitter.writeS32Leb(elementSize)
                    emitter.writeByte(WasmOp.I32_MUL)
                }
                emitter.writeByte(WasmOp.I32_ADD)
            } else {
                // Subsequent indices: field/element access
                when (currentType) {
                    is IRType.Struct -> {
                        // Struct field access
                        if (indexValue is IRIntConstant) {
                            val fieldIndex = indexValue.value.toInt()
                            val offset = calculateFieldOffset(currentType, fieldIndex)
                            if (offset > 0) {
                                emitter.writeByte(WasmOp.I32_CONST)
                                emitter.writeS32Leb(offset)
                                emitter.writeByte(WasmOp.I32_ADD)
                            }
                            currentType = currentType.elementTypes[fieldIndex]
                        } else {
                            // Dynamic field index - not common
                            error("Dynamic struct field indexing not supported")
                        }
                    }

                    is IRType.Array -> {
                        // Array element access
                        val elementSize = calculateElementSize(currentType.elementType)

                        emitValue(indexValue)
                        if (elementSize != 1) {
                            emitter.writeByte(WasmOp.I32_CONST)
                            emitter.writeS32Leb(elementSize)
                            emitter.writeByte(WasmOp.I32_MUL)
                        }
                        emitter.writeByte(WasmOp.I32_ADD)
                        currentType = currentType.elementType
                    }

                    else -> error("GEP on non-aggregate type: $currentType")
                }
            }
        }

        // Stack now has one value: the computed address
        emitLocalSet(inst)
    }

    private fun emitCall(inst: IRInstruction.Call) {
        // Check if this is a direct or indirect call
        if (inst.function is IRFunction) {
            // Direct call - use CALL instruction
            // Push arguments
            for (arg in inst.args) {
                emitValue(arg)
            }

            val funcIndex = functionIndexMap[inst.function]
                ?: error("Function ${inst.function.name} not found in index map")
            emitter.writeByte(WasmOp.CALL)
            emitter.writeU32Leb(funcIndex)
        } else {
            // Indirect call - use CALL_INDIRECT instruction
            // Push arguments first
            for (arg in inst.args) {
                emitValue(arg)
            }

            // Push the function index (loaded from pointer) - MUST BE LAST
            emitValue(inst.function)

            // Emit call_indirect with type index
            emitter.writeByte(WasmOp.CALL_INDIRECT)

            // Get the function type for the call
            val funcType = inst.function.type as IRType.Pointer
            val funcSigType = funcType.target as IRType.Function

            // For now, use type index 0 (we'll need to properly map types)
            // This is a simplification - in a full implementation we'd need a type index map
            val typeIndex = getTypeIndexForCall(funcSigType)
            emitter.writeU32Leb(typeIndex)

            // Table index (always 0 for the default table)
            emitter.writeU32Leb(0)
        }

        // Store result if non-void
        if (inst.type != IRType.Void) {
            emitLocalSet(inst)
        }
    }

    /**
     * Get the type index for a function signature.
     */
    private fun getTypeIndexForCall(funcType: IRType.Function): Int {
        val wasmFuncType = WasmFuncType.from(
            funcType.returnType,
            funcType.parameterTypes
        )
        return typeIndexMap[wasmFuncType]
            ?: error("Function type $funcType not found in type index map")
    }

    private fun emitICmp(inst: IRInstruction.ICmp) {
        emitValue(inst.lhs)
        emitValue(inst.rhs)

        val is64 = (inst.lhs.type as? IRType.Int)?.bits == 64

        val opcode = when (inst.pred) {
            IRInstruction.ICmp.Predicate.EQ -> if (is64) WasmOp.I64_EQ else WasmOp.I32_EQ
            IRInstruction.ICmp.Predicate.NE -> if (is64) WasmOp.I64_NE else WasmOp.I32_NE
            IRInstruction.ICmp.Predicate.SGT -> if (is64) WasmOp.I64_GT_S else WasmOp.I32_GT_S
            IRInstruction.ICmp.Predicate.SGE -> if (is64) WasmOp.I64_GE_S else WasmOp.I32_GE_S
            IRInstruction.ICmp.Predicate.SLT -> if (is64) WasmOp.I64_LT_S else WasmOp.I32_LT_S
            IRInstruction.ICmp.Predicate.SLE -> if (is64) WasmOp.I64_LE_S else WasmOp.I32_LE_S
            IRInstruction.ICmp.Predicate.UGT -> if (is64) WasmOp.I64_GT_U else WasmOp.I32_GT_U
            IRInstruction.ICmp.Predicate.UGE -> if (is64) WasmOp.I64_GE_U else WasmOp.I32_GE_U
            IRInstruction.ICmp.Predicate.ULT -> if (is64) WasmOp.I64_LT_U else WasmOp.I32_LT_U
            IRInstruction.ICmp.Predicate.ULE -> if (is64) WasmOp.I64_LE_U else WasmOp.I32_LE_U
        }
        emitter.writeByte(opcode)

        emitLocalSet(inst)
    }

    private fun emitFCmp(inst: IRInstruction.FCmp) {
        emitValue(inst.lhs)
        emitValue(inst.rhs)

        val is64 = inst.lhs.type is IRType.Double

        val opcode = when (inst.pred) {
            IRInstruction.FCmp.Predicate.OEQ, IRInstruction.FCmp.Predicate.UEQ -> if (is64) WasmOp.F64_EQ else WasmOp.F32_EQ
            IRInstruction.FCmp.Predicate.ONE, IRInstruction.FCmp.Predicate.UNE -> if (is64) WasmOp.F64_NE else WasmOp.F32_NE
            IRInstruction.FCmp.Predicate.OGT -> if (is64) WasmOp.F64_GT else WasmOp.F32_GT
            IRInstruction.FCmp.Predicate.OGE -> if (is64) WasmOp.F64_GE else WasmOp.F32_GE
            IRInstruction.FCmp.Predicate.OLT -> if (is64) WasmOp.F64_LT else WasmOp.F32_LT
            IRInstruction.FCmp.Predicate.OLE -> if (is64) WasmOp.F64_LE else WasmOp.F32_LE
        }
        emitter.writeByte(opcode)

        emitLocalSet(inst)
    }

    private fun emitCast(inst: IRInstruction.Cast) {
        emitValue(inst.value)

        // Emit cast operation if needed
        when (inst.op) {
            IRInstruction.Cast.Op.ZExt -> {
                // Zero extension is implicit in Wasm for i1->i32 (value is already 0 or 1)
                // No operation needed for other small to large zero extends
            }

            IRInstruction.Cast.Op.SExt -> {
                // Sign extension - only needed for i32->i64
                if (inst.type is IRType.Int && inst.type.bits == 64) {
                    emitter.writeByte(WasmOp.I64_EXTEND_I32_S)
                }
                // Other cases (i8->i32, i16->i32) are implicit in Wasm's i32 operations
            }

            IRInstruction.Cast.Op.Trunc -> {
                // Truncation is implicit in Wasm (i64->i32 wraps, i32->i8 uses low bits)
            }

            IRInstruction.Cast.Op.FPToSI -> {
                val is64 = (inst.type as IRType.Int).bits == 64
                val srcIs64 = inst.value.type is IRType.Double
                val opcode = when {
                    is64 && srcIs64 -> WasmOp.I64_TRUNC_F64_S
                    is64 && !srcIs64 -> WasmOp.I64_TRUNC_F32_S
                    !is64 && srcIs64 -> WasmOp.I32_TRUNC_F64_S
                    else -> WasmOp.I32_TRUNC_F32_S
                }
                emitter.writeByte(opcode)
            }

            IRInstruction.Cast.Op.SIToFP -> {
                val srcIs64 = (inst.value.type as? IRType.Int)?.bits == 64
                val opcode = when (inst.type) {
                    is IRType.Float -> if (srcIs64) WasmOp.F32_CONVERT_I64_S else WasmOp.F32_CONVERT_I32_S
                    is IRType.Double -> if (srcIs64) WasmOp.F64_CONVERT_I64_S else WasmOp.F64_CONVERT_I32_S
                    else -> error("Invalid SIToFP target type: ${inst.type}")
                }
                emitter.writeByte(opcode)
            }

            IRInstruction.Cast.Op.BitCast -> {
                // Reinterpretation, no operation in Wasm (types remain the same at runtime)
            }

            IRInstruction.Cast.Op.PtrToInt -> {
                // Pointer is already i32 in Wasm linear memory model
            }

            IRInstruction.Cast.Op.IntToPtr -> {
                // i32 is already pointer-like in Wasm linear memory model
            }
        }

        emitLocalSet(inst)
    }

    /**
     * Emit a PHI copy instruction
     */
    fun emitPhiCopy(phiCopy: PhiResolver.PhiCopy) {
        // Emit: source value -> destination local
        emitValue(phiCopy.sourceValue)
        emitter.writeByte(WasmOp.LOCAL_SET)
        emitter.writeU32Leb(phiCopy.destinationLocal)
    }
}
