package wasm

import llvm.*

/**
 * Resolves PHI nodes by inserting copies at the end of predecessor blocks.
 *
 * LLVM IR uses PHI nodes for SSA form at control flow merge points:
 *   %result = phi i32 [ %value1, %block1 ], [ %value2, %block2 ]
 *
 * WebAssembly doesn't have PHI nodes. Instead, we need to:
 * 1. Allocate a local for the PHI result
 * 2. Insert `local.set` instructions at the end of each predecessor block
 *    to copy the incoming value into the PHI result local
 * 3. Remove the PHI instruction from the main instruction stream
 *
 * This transformation must happen BEFORE the Relooper processes the CFG,
 * as it modifies the basic blocks.
 */
class PhiResolver(
    private val function: IRFunction,
    private val localsManager: LocalsManager
) {
    // Map from block to list of phi copies that need to be inserted at the end
    private val blockPhiCopies = mutableMapOf<IRBasicBlock, MutableList<PhiCopy>>()

    /**
     * Get PHI copies for a block (to be emitted before terminator)
     */
    fun getPhiCopies(block: IRBasicBlock): List<PhiCopy> {
        return blockPhiCopies[block] ?: emptyList()
    }

    /**
     * Resolve all PHI nodes in the function by inserting copies.
     */
    fun resolve() {
        // Collect all PHI nodes first (to avoid concurrent modification)
        val phiNodes = mutableListOf<PhiInfo>()

        for (block in function.basicBlocks) {
            for (instruction in block.instructions) {
                if (instruction is IRInstruction.Phi) {
                    phiNodes.add(PhiInfo(block, instruction))
                }
            }
        }

        // Process each PHI node
        for (phiInfo in phiNodes) {
            resolvePhi(phiInfo)
        }
    }

    private data class PhiInfo(
        val block: IRBasicBlock,
        val phi: IRInstruction.Phi
    )

    private fun resolvePhi(phiInfo: PhiInfo) {
        val (block, phi) = phiInfo

        // Ensure the PHI result has a local allocated
        val resultLocal = localsManager.getOrAllocateLocal(phi)

        // For each incoming edge: (value, predecessor block)
        for ((incomingValue, predecessorBlock) in phi.incoming) {
            // Insert a copy at the end of the predecessor block
            // We need to insert BEFORE the terminator (br, condbr, ret)
            insertCopyBeforeTerminator(predecessorBlock, incomingValue, resultLocal)
        }

        // Remove the PHI instruction from the current block
        // (It will be replaced by the inserted copies)
        block.instructions.remove(phi)
    }

    /**
     * Insert a copy instruction (local.set) before the terminator of a block.
     *
     * The copy stores the incoming value into the PHI result local.
     */
    private fun insertCopyBeforeTerminator(
        block: IRBasicBlock,
        incomingValue: IRValue,
        destinationLocal: Int
    ) {
        // Store phi copy in our separate map instead of modifying instructions list
        val phiCopy = PhiCopy(incomingValue, destinationLocal)
        blockPhiCopies.getOrPut(block) { mutableListOf() }.add(phiCopy)
    }

    /**
     * A marker interface for PHI copy pseudo-instructions.
     * Since we can't extend IRInstruction from outside the llvm package,
     * we store these as regular objects and check for them explicitly.
     */
    data class PhiCopy(
        val sourceValue: IRValue,
        val destinationLocal: Int
    ) {
        override fun toString() = "phi_copy local.$destinationLocal = ${sourceValue.ref()}"
    }
}

/**
 * Helper to detect if a basic block contains PHI nodes
 */
fun IRBasicBlock.hasPhiNodes(): Boolean {
    return instructions.any { it is IRInstruction.Phi }
}

/**
 * Helper to get all PHI nodes in a basic block
 */
fun IRBasicBlock.getPhiNodes(): List<IRInstruction.Phi> {
    return instructions.filterIsInstance<IRInstruction.Phi>()
}

/**
 * Helper to get non-PHI instructions in a basic block.
 * In LLVM IR, PHI nodes must appear at the beginning of a block.
 */
fun IRBasicBlock.getNonPhiInstructions(): List<IRInstruction> {
    return instructions.filterNot { it is IRInstruction.Phi }
}
