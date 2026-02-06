package llvm.opt.analysis

import llvm.*

/**
 * Information about the use of a value.
 */
data class UseInfo(
    /** The instruction that uses the value */
    val instruction: IRInstruction,
    /** The basic block containing the use */
    val block: IRBasicBlock,
    /** Index of the operand (for multi-operand instructions) */
    val operandIndex: Int
)

/**
 * Result of use-def analysis for a function.
 */
class UseDefInfo private constructor(
    val function: IRFunction,
    private val usesMap: Map<IRValue, List<UseInfo>>,
    private val defBlockMap: Map<IRValue, IRBasicBlock?>
) {
    fun getUses(value: IRValue): List<UseInfo> {
        return usesMap[value] ?: emptyList()
    }

    fun getDefBlock(value: IRValue): IRBasicBlock? {
        return defBlockMap[value]
    }

    companion object {
        /**
         * Compute use-def info for a function.
         */
        fun compute(function: IRFunction): UseDefInfo {
            val uses = mutableMapOf<IRValue, MutableList<UseInfo>>()
            val defBlock = mutableMapOf<IRValue, IRBasicBlock?>()

            // Arguments are defined at function entry (no block)
            for (arg in function.parameters) {
                defBlock[arg] = null
            }

            // Process all instructions
            for (block in function.basicBlocks) {
                for (instruction in block.instructions) {
                    // Record definition location
                    if (instruction.type != IRType.Void) {
                        defBlock[instruction] = block
                    }

                    // Record uses
                    for ((index, operand) in instruction.operands().withIndex()) {
                        uses.getOrPut(operand) { mutableListOf() }
                            .add(UseInfo(instruction, block, index))
                    }
                }
            }

            return UseDefInfo(function, uses, defBlock)
        }
    }
}

/**
 * Get all operand values used by an instruction.
 */
fun IRInstruction.operands(): List<IRValue> {
    return when (this) {
        is IRInstruction.Binary -> listOf(lhs, rhs)
        is IRInstruction.Unary -> listOf(value)
        is IRInstruction.Alloca -> emptyList()
        is IRInstruction.Load -> listOf(ptr)
        is IRInstruction.Store -> listOf(value, ptr)
        is IRInstruction.GEP -> listOf(ptr) + indices
        is IRInstruction.Call -> listOf(function) + args
        is IRInstruction.Ret -> listOfNotNull(value)
        is IRInstruction.Br -> emptyList()
        is IRInstruction.CondBr -> listOf(condition)
        is IRInstruction.Phi -> incoming.map { it.first }
        is IRInstruction.ICmp -> listOf(lhs, rhs)
        is IRInstruction.FCmp -> listOf(lhs, rhs)
        is IRInstruction.Cast -> listOf(value)
    }
}

/**
 * Helper to replace all uses of a value with another value.
 */
fun IRFunction.replaceAllUses(oldValue: IRValue, newValue: IRValue) {
    for (block in basicBlocks) {
        for (instruction in block.instructions) {
            replaceUsesInInstruction(instruction, oldValue, newValue)
        }
    }
}

/**
 * Replace uses of oldValue with newValue in a single instruction.
 */
fun replaceUsesInInstruction(instruction: IRInstruction, oldValue: IRValue, newValue: IRValue) {
    when (instruction) {
        is IRInstruction.Binary -> {
            if (instruction.lhs === oldValue) {
                setField(instruction, "lhs", newValue)
            }
            if (instruction.rhs === oldValue) {
                setField(instruction, "rhs", newValue)
            }
        }

        is IRInstruction.Unary -> {
            if (instruction.value === oldValue) {
                setField(instruction, "value", newValue)
            }
        }

        is IRInstruction.Load -> {
            if (instruction.ptr === oldValue) {
                setField(instruction, "ptr", newValue)
            }
        }

        is IRInstruction.Store -> {
            if (instruction.value === oldValue) {
                setField(instruction, "value", newValue)
            }
            if (instruction.ptr === oldValue) {
                setField(instruction, "ptr", newValue)
            }
        }

        is IRInstruction.GEP -> {
            if (instruction.ptr === oldValue) {
                setField(instruction, "ptr", newValue)
            }
            // Handle indices list
            val newIndices = instruction.indices.map { if (it === oldValue) newValue else it }
            if (newIndices != instruction.indices) {
                setField(instruction, "indices", newIndices)
            }
        }

        is IRInstruction.Call -> {
            if (instruction.function === oldValue) {
                setField(instruction, "function", newValue)
            }
            // Handle args list
            val newArgs = instruction.args.map { if (it === oldValue) newValue else it }
            if (newArgs != instruction.args) {
                setField(instruction, "args", newArgs)
            }
        }

        is IRInstruction.Ret -> {
            if (instruction.value === oldValue) {
                setField(instruction, "value", newValue)
            }
        }

        is IRInstruction.CondBr -> {
            if (instruction.condition === oldValue) {
                setField(instruction, "condition", newValue)
            }
        }

        is IRInstruction.Phi -> {
            for (i in instruction.incoming.indices) {
                val (value, block) = instruction.incoming[i]
                if (value === oldValue) {
                    instruction.incoming[i] = newValue to block
                }
            }
        }

        is IRInstruction.ICmp -> {
            if (instruction.lhs === oldValue) {
                setField(instruction, "lhs", newValue)
            }
            if (instruction.rhs === oldValue) {
                setField(instruction, "rhs", newValue)
            }
        }

        is IRInstruction.FCmp -> {
            if (instruction.lhs === oldValue) {
                setField(instruction, "lhs", newValue)
            }
            if (instruction.rhs === oldValue) {
                setField(instruction, "rhs", newValue)
            }
        }

        is IRInstruction.Cast -> {
            if (instruction.value === oldValue) {
                setField(instruction, "value", newValue)
            }
        }

        is IRInstruction.Alloca -> { /* no value operands */
        }

        is IRInstruction.Br -> { /* no value operands */
        }
    }
}

/**
 * Helper to set a field via reflection.
 * This is needed because IR instruction fields are val.
 */
private fun setField(obj: Any, fieldName: String, value: Any?) {
    val field = obj::class.java.getDeclaredField(fieldName)
    field.isAccessible = true
    field.set(obj, value)
}
