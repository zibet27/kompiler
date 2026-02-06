package llvm.opt.analysis

import llvm.IRBasicBlock
import llvm.IRFunction

/**
 * Result of dominance analysis for a function.
 */
class DominanceInfo private constructor(
    val function: IRFunction,
    private val idom: Map<IRBasicBlock, IRBasicBlock?>,
    private val dominanceFrontiers: Map<IRBasicBlock, Set<IRBasicBlock>>,
    private val domTreeChildren: Map<IRBasicBlock, Set<IRBasicBlock>>
) {
    /**
     * Check if block A dominates block B.
     * A block dominates itself.
     */
    fun dominates(a: IRBasicBlock, b: IRBasicBlock): Boolean {
        if (a == b) return true
        
        var current: IRBasicBlock? = b
        while (current != null) {
            current = idom[current]
            if (current == a) return true
        }
        
        return false
    }

    /**
     * Get the dominance frontier of a block.
     * The dominance frontier of a block A is the set of blocks B where:
     * - A dominates a predecessor of B, but
     * - A does not strictly dominate B
     */
    fun dominanceFrontier(block: IRBasicBlock): Set<IRBasicBlock> {
        return dominanceFrontiers[block] ?: emptySet()
    }

    fun domTreeChildren(block: IRBasicBlock): Set<IRBasicBlock> {
        return domTreeChildren[block] ?: emptySet()
    }

    companion object {
        /**
         * Compute dominance info for a function using the Cooper-Harvey-Kennedy algorithm.
         */
        fun compute(function: IRFunction): DominanceInfo {
            val blocks = function.basicBlocks
            if (blocks.isEmpty()) {
                return DominanceInfo(function, emptyMap(), emptyMap(), emptyMap())
            }
            
            val cfg = CFGInfo.compute(function)
            val entry = blocks.first()
            
            // Compute reverse post-order numbering
            val rpo = cfg.reversePostOrder()
            val rpoNumber = rpo.withIndex().associate { (index, block) -> block to index }
            
            // Initialize idom map - entry has no dominator
            val idom = mutableMapOf<IRBasicBlock, IRBasicBlock?>()
            idom[entry] = null
            
            // Iterative dominator computation (Cooper-Harvey-Kennedy)
            var changed = true
            while (changed) {
                changed = false
                
                for (block in rpo) {
                    if (block == entry) continue
                    
                    val preds = cfg.predecessors(block)
                    if (preds.isEmpty()) continue
                    
                    // Find the first processed predecessor
                    var newIdom: IRBasicBlock? = null
                    for (pred in preds) {
                        if (pred in idom) {
                            newIdom = pred
                            break
                        }
                    }
                    
                    if (newIdom == null) continue

                    // Intersect with other predecessors
                    for (pred in preds) {
                        if (pred == newIdom) continue
                        if (pred in idom) {
                            newIdom = intersect(pred, newIdom!!, idom, rpoNumber)
                        }
                    }
                    
                    if (idom[block] != newIdom) {
                        idom[block] = newIdom
                        changed = true
                    }
                }
            }

            val domTreeChildren = mutableMapOf<IRBasicBlock, MutableSet<IRBasicBlock>>()
            for (block in blocks) {
                domTreeChildren[block] = mutableSetOf()
            }
            for ((block, dom) in idom) {
                if (dom != null) {
                    domTreeChildren[dom]!!.add(block)
                }
            }
            
            // Compute dominance frontiers
            val dominanceFrontiers = computeDominanceFrontiers(blocks, cfg, idom)
            
            return DominanceInfo(function, idom, dominanceFrontiers, domTreeChildren)
        }

        /**
         * Intersect two dominators in the dominator tree.
         */
        private fun intersect(
            b1: IRBasicBlock,
            b2: IRBasicBlock,
            idom: Map<IRBasicBlock, IRBasicBlock?>,
            rpoNumber: Map<IRBasicBlock, Int>
        ): IRBasicBlock {
            var finger1: IRBasicBlock? = b1
            var finger2: IRBasicBlock? = b2
            
            while (finger1 != finger2) {
                while (finger1 != null && finger2 != null && 
                       (rpoNumber[finger1] ?: Int.MAX_VALUE) > (rpoNumber[finger2] ?: Int.MAX_VALUE)) {
                    finger1 = idom[finger1]
                }
                while (finger1 != null && finger2 != null && 
                       (rpoNumber[finger2] ?: Int.MAX_VALUE) > (rpoNumber[finger1] ?: Int.MAX_VALUE)) {
                    finger2 = idom[finger2]
                }
            }
            
            return finger1 ?: b1
        }

        /**
         * Compute dominance frontiers for all blocks.
         */
        private fun computeDominanceFrontiers(
            blocks: List<IRBasicBlock>,
            cfg: CFGInfo,
            idom: Map<IRBasicBlock, IRBasicBlock?>
        ): Map<IRBasicBlock, Set<IRBasicBlock>> {
            val df = mutableMapOf<IRBasicBlock, MutableSet<IRBasicBlock>>()
            for (block in blocks) {
                df[block] = mutableSetOf()
            }
            
            for (block in blocks) {
                val preds = cfg.predecessors(block)
                if (preds.size >= 2) {
                    // This is a join point
                    for (pred in preds) {
                        var runner: IRBasicBlock? = pred
                        while (runner != null && runner != idom[block]) {
                            df[runner]!!.add(block)
                            runner = idom[runner]
                        }
                    }
                }
            }
            
            return df
        }
    }
}

/**
 * Computes the iterated dominance frontier.
 * Given a set of definition blocks, returns all blocks where PHI nodes may be needed.
 */
fun DominanceInfo.iteratedDominanceFrontier(defBlocks: Set<IRBasicBlock>): Set<IRBasicBlock> {
    val result = mutableSetOf<IRBasicBlock>()
    val worklist = ArrayDeque(defBlocks)
    val processed = mutableSetOf<IRBasicBlock>()
    
    while (worklist.isNotEmpty()) {
        val block = worklist.removeFirst()
        if (block in processed) continue
        processed.add(block)
        
        for (dfBlock in dominanceFrontier(block)) {
            if (dfBlock !in result) {
                result.add(dfBlock)
                worklist.add(dfBlock)
            }
        }
    }
    
    return result
}
