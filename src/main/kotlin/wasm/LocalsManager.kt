package wasm

import llvm.*

/**
 * Manages the mapping from LLVM SSA registers to WebAssembly local indices.
 *
 * In LLVM IR, each instruction produces a new SSA register (e.g., %1, %val).
 * In WebAssembly, we use numbered locals that can be set and get multiple times.
 *
 * This class:
 * - Assigns a unique local index to each SSA value
 * - Tracks the type of each local
 * - Distinguishes between function parameters (which are also locals) and local variables
 */
class LocalsManager(private val function: IRFunction) {

    // Map from LLVM IR value to Wasm local index
    private val valueToLocal = mutableMapOf<IRValue, Int>()

    // Map from local index to Wasm type
    private val localTypes = mutableListOf<WasmValType>()

    // Next available local index
    private var nextLocalIndex = 0

    init {
        // First, allocate locals for function parameters
        // In Wasm, parameters are locals 0, 1, 2, ...
        for (param in function.parameters) {
            val wasmType = WasmValType.from(param.type)
            allocateLocal(param, wasmType)
        }
    }

    /**
     * Allocate a local for the given SSA value
     */
    fun allocateLocal(value: IRValue, type: WasmValType): Int {
        val index = nextLocalIndex++
        valueToLocal[value] = index
        localTypes.add(type)
        return index
    }

    /**
     * Get the local index for an SSA value.
     * If not yet allocated, allocates a new local.
     */
    fun getOrAllocateLocal(value: IRValue): Int {
        return valueToLocal[value] ?: run {
            val wasmType = WasmValType.from(value.type)
            allocateLocal(value, wasmType)
        }
    }

    /**
     * Get the local index for an SSA value (must be already allocated)
     */
    fun getLocal(value: IRValue): Int {
        return valueToLocal[value]
            ?: error("Value ${value.ref()} not allocated as local")
    }

    /**
     * Check if a value has an allocated local
     */
    fun hasLocal(value: IRValue): Boolean {
        return value in valueToLocal
    }

    /**
     * Get the local variables (excluding parameters) grouped by type.
     * This is needed for the Code section encoding in Wasm.
     *
     * Returns a list of (count, type) pairs
     */
    fun getLocalVarsByType(): List<Pair<Int, WasmValType>> {
        val paramCount = function.parameters.size
        val localVarTypes = localTypes.drop(paramCount)

        if (localVarTypes.isEmpty()) {
            return emptyList()
        }

        // Group consecutive locals of the same type
        val grouped = mutableListOf<Pair<Int, WasmValType>>()
        var currentType = localVarTypes[0]
        var count = 1

        for (i in 1 until localVarTypes.size) {
            if (localVarTypes[i] == currentType) {
                count++
            } else {
                grouped.add(count to currentType)
                currentType = localVarTypes[i]
                count = 1
            }
        }
        grouped.add(count to currentType)

        return grouped
    }

    /**
     * Pre-allocate locals for all instructions in the function that produce values.
     * This is called during the analysis phase before code generation.
     */
    fun analyzeAndAllocateLocals() {
        for (block in function.basicBlocks) {
            for (instruction in block.instructions) {
                // Skip instructions that don't produce values (void type)
                if (instruction.type == IRType.Void) continue

                // Skip instructions that already have locals (e.g., parameters)
                if (hasLocal(instruction)) continue

                // Allocate a local for this instruction's result
                val wasmType = WasmValType.from(instruction.type)
                allocateLocal(instruction, wasmType)
            }
        }
    }
}
