package llvm.opt.analysis

import llvm.IRBasicBlock
import llvm.IRFunction
import llvm.IRInstruction

/**
 * Control Flow Graph analysis.
 * 
 * Computes predecessor and successor relationships between basic blocks.
 */
class CFGAnalysis : FunctionAnalysis<CFGInfo> {
    override val name = "cfg"

    override fun analyze(target: IRFunction): CFGInfo {
        return CFGInfo.compute(target)
    }
}

/**
 * Result of CFG analysis for a function.
 */
class CFGInfo private constructor(
    val function: IRFunction,
    private val predecessorMap: Map<IRBasicBlock, Set<IRBasicBlock>>,
    private val successorMap: Map<IRBasicBlock, Set<IRBasicBlock>>
) {
    /**
     * Get predecessor blocks of a block.
     */
    fun predecessors(block: IRBasicBlock): Set<IRBasicBlock> {
        return predecessorMap[block] ?: emptySet()
    }

    /**
     * Get successor blocks of a block.
     */
    fun successors(block: IRBasicBlock): Set<IRBasicBlock> {
        return successorMap[block] ?: emptySet()
    }

    /**
     * Get the entry block of the function.
     */
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
                if (succ !in visited) {
                    worklist.add(succ)
                }
            }
        }
        
        return false
    }

    /**
     * Get all blocks reachable from the entry.
     */
    fun reachableBlocks(): Set<IRBasicBlock> {
        val entry = entryBlock ?: return emptySet()
        val visited = mutableSetOf<IRBasicBlock>()
        val worklist = ArrayDeque<IRBasicBlock>()
        worklist.add(entry)
        
        while (worklist.isNotEmpty()) {
            val current = worklist.removeFirst()
            if (current in visited) continue
            visited.add(current)
            
            for (succ in successors(current)) {
                if (succ !in visited) {
                    worklist.add(succ)
                }
            }
        }
        
        return visited
    }

    /**
     * Perform a reverse post-order traversal of the CFG.
     * This ordering ensures that, except for back edges, all predecessors
     * of a block come before the block in the ordering.
     */
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
        /**
         * Compute CFG info for a function.
         */
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
                val succs = getBlockSuccessors(block)
                successors[block]!!.addAll(succs)
                
                for (succ in succs) {
                    predecessors[succ]?.add(block)
                }
            }
            
            return CFGInfo(function, predecessors, successors)
        }

        /**
         * Get the successors of a block from its terminator.
         */
        fun getBlockSuccessors(block: IRBasicBlock): List<IRBasicBlock> {
            return when (val term = block.terminator) {
                is IRInstruction.Br -> listOf(term.dest)
                is IRInstruction.CondBr -> listOf(term.thenBlock, term.elseBlock)
                is IRInstruction.Ret -> emptyList()
                else -> emptyList()
            }
        }
    }
}
