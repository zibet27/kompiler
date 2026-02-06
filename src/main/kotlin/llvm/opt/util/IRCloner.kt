package llvm.opt.util

import llvm.*

/**
 * Utility for cloning IR structures with value remapping.
 * 
 * Used primarily for function inlining to clone the callee's body
 * into the caller with fresh names and remapped values.
 */
class IRCloner(
    private val valueMap: MutableMap<IRValue, IRValue> = mutableMapOf(),
    private val blockMap: MutableMap<IRBasicBlock, IRBasicBlock> = mutableMapOf(),
    private val namePrefix: String = "inline"
) {
    private var nameCounter = 0

    fun freshName(originalName: String): String {
        return "${namePrefix}.${originalName}.${nameCounter++}"
    }

    fun mapValue(original: IRValue, cloned: IRValue) {
        valueMap[original] = cloned
    }

    fun getMappedValue(original: IRValue): IRValue {
        return valueMap[original] ?: original
    }

    fun mapBlock(original: IRBasicBlock, cloned: IRBasicBlock) {
        blockMap[original] = cloned
    }

    fun getMappedBlock(original: IRBasicBlock): IRBasicBlock {
        return blockMap[original] ?: original
    }

    fun cloneBlockHeader(original: IRBasicBlock): IRBasicBlock {
        val cloned = IRBasicBlock(freshName(original.name))
        mapBlock(original, cloned)
        return cloned
    }

    fun cloneBlockInstructions(original: IRBasicBlock, cloned: IRBasicBlock) {
        for (instruction in original.instructions) {
            val clonedInst = cloneInstruction(instruction)
            if (clonedInst != null) {
                cloned.instructions.add(clonedInst)
                // Map the original instruction to the cloned one
                if (instruction.type != IRType.Void) {
                    mapValue(instruction, clonedInst)
                }
            }
        }
    }

    fun cloneInstruction(instruction: IRInstruction): IRInstruction? {
        return when (instruction) {
            is IRInstruction.Binary -> IRInstruction.Binary(
                instruction.op,
                getMappedValue(instruction.lhs),
                getMappedValue(instruction.rhs),
                instruction.type,
                freshName(instruction.name)
            )

            is IRInstruction.Unary -> IRInstruction.Unary(
                instruction.op,
                getMappedValue(instruction.value),
                instruction.type,
                freshName(instruction.name)
            )

            is IRInstruction.Alloca -> IRInstruction.Alloca(
                instruction.allocatedType,
                freshName(instruction.name)
            )

            is IRInstruction.Load -> IRInstruction.Load(
                getMappedValue(instruction.ptr),
                instruction.type,
                freshName(instruction.name)
            )

            is IRInstruction.Store -> IRInstruction.Store(
                getMappedValue(instruction.value),
                getMappedValue(instruction.ptr)
            )

            is IRInstruction.GEP -> IRInstruction.GEP(
                instruction.baseType,
                getMappedValue(instruction.ptr),
                instruction.indices.map { getMappedValue(it) },
                instruction.type,
                freshName(instruction.name),
                instruction.inBounds
            )

            is IRInstruction.Call -> IRInstruction.Call(
                (instruction.function.type as? IRType.Pointer)?.target as? IRType.Function
                    ?: (instruction.function.type as IRType.Function),
                getMappedValue(instruction.function),
                instruction.args.map { getMappedValue(it) },
                if (instruction.type == IRType.Void) "" else freshName(instruction.name)
            )

            is IRInstruction.Ret -> IRInstruction.Ret(
                instruction.value?.let { getMappedValue(it) }
            )

            is IRInstruction.Br -> IRInstruction.Br(
                getMappedBlock(instruction.dest)
            )

            is IRInstruction.CondBr -> IRInstruction.CondBr(
                getMappedValue(instruction.condition),
                getMappedBlock(instruction.thenBlock),
                getMappedBlock(instruction.elseBlock)
            )

            is IRInstruction.Phi -> {
                val clonedPhi = IRInstruction.Phi(
                    instruction.type,
                    mutableListOf(),
                    freshName(instruction.name)
                )
                // PHI incoming values will be filled after all blocks are processed
                clonedPhi
            }

            is IRInstruction.ICmp -> IRInstruction.ICmp(
                instruction.pred,
                getMappedValue(instruction.lhs),
                getMappedValue(instruction.rhs),
                freshName(instruction.name)
            )

            is IRInstruction.FCmp -> IRInstruction.FCmp(
                instruction.pred,
                getMappedValue(instruction.lhs),
                getMappedValue(instruction.rhs),
                freshName(instruction.name)
            )

            is IRInstruction.Cast -> IRInstruction.Cast(
                instruction.op,
                getMappedValue(instruction.value),
                instruction.type,
                freshName(instruction.name)
            )
        }
    }

    /**
     * Fix up PHI nodes after all blocks have been cloned.
     * This fills in the incoming values with properly remapped values and blocks.
     */
    fun fixupPhiNodes(originalBlocks: List<IRBasicBlock>) {
        for (originalBlock in originalBlocks) {
            val clonedBlock = getMappedBlock(originalBlock)

            for ((i, originalInst) in originalBlock.instructions.withIndex()) {
                if (originalInst is IRInstruction.Phi) {
                    val clonedInst = clonedBlock.instructions[i] as? IRInstruction.Phi
                        ?: continue

                    for ((value, block) in originalInst.incoming) {
                        clonedInst.addIncoming(
                            getMappedValue(value),
                            getMappedBlock(block)
                        )
                    }
                }
            }
        }
    }

    /**
     * Clone an entire function body (for inlining).
     * Returns the cloned blocks.
     */
    fun cloneFunctionBody(callee: IRFunction): List<IRBasicBlock> {
        // First pass: create all blocks
        val clonedBlocks = callee.basicBlocks.map { cloneBlockHeader(it) }

        // Second pass: clone instructions
        for ((original, cloned) in callee.basicBlocks.zip(clonedBlocks)) {
            cloneBlockInstructions(original, cloned)
        }

        // Third pass: fix up PHI nodes
        fixupPhiNodes(callee.basicBlocks)

        return clonedBlocks
    }
}

fun IRFunction.countInstructions(): Int = basicBlocks.sumOf { it.instructions.size }