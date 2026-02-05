package llvm

/**
 * Reorders basic blocks in a function to improve CFG layout for the Relooper.
 *
 * The goal is to place blocks in an order that minimizes forward jumps and makes
 * control flow more natural for structured control flow generation.
 *
 * Strategy:
 * 1. Entry block first
 * 2. BFS/topological order for reachable blocks
 * 3. Place loop bodies before loop exits when possible
 */
class IRReorderBlocks {

    fun reorder(function: IRFunction) {
        if (function.basicBlocks.isEmpty()) return

        val blocks = function.basicBlocks.toList()
        val newOrder = mutableListOf<IRBasicBlock>()
        val visited = mutableSetOf<IRBasicBlock>()

        // Build successor map
        val successors = mutableMapOf<IRBasicBlock, List<IRBasicBlock>>()
        for (block in blocks) {
            successors[block] = getSuccessors(block)
        }

        // Start with entry block
        val entry = blocks.first()

        // DFS traversal to maintain better locality
        fun visit(block: IRBasicBlock) {
            if (block in visited) return
            visited.add(block)
            newOrder.add(block)

            // Visit successors in order
            for (succ in successors[block] ?: emptyList()) {
                visit(succ)
            }
        }

        visit(entry)

        // Add any unreachable blocks at the end
        for (block in blocks) {
            if (block !in visited) {
                newOrder.add(block)
            }
        }

        // Replace the blocks list
        function.basicBlocks.clear()
        function.basicBlocks.addAll(newOrder)
    }

    private fun getSuccessors(block: IRBasicBlock): List<IRBasicBlock> {
        return when (val term = block.terminator) {
            is IRInstruction.Br -> listOf(term.dest)
            is IRInstruction.CondBr -> listOf(term.thenBlock, term.elseBlock)
            else -> emptyList()
        }
    }
}
