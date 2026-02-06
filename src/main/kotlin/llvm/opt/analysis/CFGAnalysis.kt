package llvm.opt.analysis

import llvm.IRBasicBlock
import llvm.IRFunction
import llvm.IRInstruction

/**
 * Result of CFG analysis for a function.
 */
class CFGInfo private constructor(
    val function: IRFunction,
    private val predecessorMap: Map<IRBasicBlock, Set<IRBasicBlock>>,
    private val successorMap: Map<IRBasicBlock, Set<IRBasicBlock>>
) {
    fun predecessors(block: IRBasicBlock): Set<IRBasicBlock> {
        return predecessorMap[block] ?: emptySet()
    }

    fun successors(block: IRBasicBlock): Set<IRBasicBlock> {
        return successorMap[block] ?: emptySet()
    }

    val entryBlock: IRBasicBlock?
        get() = function.basicBlocks.firstOrNull()

    /**
     * Check if a block is reachable from the entry.
     */
    fun isReachable(block: IRBasicBlock): Boolean {
        val entry = entryBlock ?: return false
        if (block == entry) return true

        val visited = mutableSetOf<IRBasicBlock>()
        val worklist = ArrayDeque<IRBasicBlock>()
        worklist.add(entry)

        while (worklist.isNotEmpty()) {
            val current = worklist.removeFirst()
            if (current == block) return true
            if (current in visited) continue
            visited.add(current)

            for (succ in successors(current)) {
                if (succ in visited) continue
                worklist.add(succ)
            }
        }

        return false
    }

    fun reversePostOrder(): List<IRBasicBlock> {
        val entry = entryBlock ?: return emptyList()
        val visited = mutableSetOf<IRBasicBlock>()
        val postOrder = mutableListOf<IRBasicBlock>()

        fun dfs(block: IRBasicBlock) {
            if (block in visited) return
            visited.add(block)

            for (succ in successors(block)) {
                dfs(succ)
            }

            postOrder.add(block)
        }

        dfs(entry)
        return postOrder.reversed()
    }

    companion object {
        fun compute(function: IRFunction): CFGInfo {
            val predecessors = mutableMapOf<IRBasicBlock, MutableSet<IRBasicBlock>>()
            val successors = mutableMapOf<IRBasicBlock, MutableSet<IRBasicBlock>>()

            // Initialize empty sets for all blocks
            for (block in function.basicBlocks) {
                predecessors[block] = mutableSetOf()
                successors[block] = mutableSetOf()
            }

            // Build the maps
            for (block in function.basicBlocks) {
                val ss = block.getSuccessors()
                successors[block]!!.addAll(ss)

                for (succ in ss) {
                    predecessors[succ]?.add(block)
                }
            }

            return CFGInfo(function, predecessors, successors)
        }

        private fun IRBasicBlock.getSuccessors(): List<IRBasicBlock> = when (val term = terminator) {
            is IRInstruction.Br -> listOf(term.dest)
            is IRInstruction.CondBr -> listOf(term.thenBlock, term.elseBlock)
            is IRInstruction.Ret -> emptyList()
            else -> emptyList()
        }
    }
}
