package llvm.opt.pass

import llvm.*
import llvm.opt.analysis.*

/**
 * Memory to Register (Mem2Reg) optimization pass.
 * 
 * Promotes stack allocations (alloca) to SSA registers where possible.
 * This is a key optimization that enables many other optimizations by
 * converting memory operations to an SSA form.
 * 
 * An alloca is promotable if:
 * - It allocates a scalar type (not aggregates accessed via GEP)
 * - All uses are direct loads and stores (no address taken)
 * - The address doesn't escape (not passed to calls)
 * 
 * Algorithm:
 * 1. Find all promotable allocas
 * 2. For each alloca, compute where PHI nodes are needed (iterated dominance frontier)
 * 3. Insert PHI nodes
 * 4. Rename variables using dominator tree traversal
 * 5. Remove the original allocas, loads, and stores
 */
class Mem2RegPass : OptimizationPass {
    override val name = "mem2reg"

    override fun runOnFunction(function: IRFunction): Boolean {
        if (function.basicBlocks.isEmpty()) return false

        // Find all allocas in the entry block
        val entryBlock = function.basicBlocks.first()
        val allocas = entryBlock.instructions.filterIsInstance<IRInstruction.Alloca>()
        if (allocas.isEmpty()) return false

        // Filter to promotable allocas
        val promotable = allocas.filter { it.isPromotable(function) }
        if (promotable.isEmpty()) return false

        // Compute analyses
        val cfg = CFGInfo.compute(function)
        val domInfo = DominanceInfo.compute(function)

        // Process each promotable alloca
        for (alloca in promotable) {
            promoteAlloca(alloca, function, cfg, domInfo)
        }

        return true
    }

    /**
     * Check if an alloca is promotable to a register.
     */
    private fun IRInstruction.Alloca.isPromotable(function: IRFunction): Boolean {
        // Only promote scalar types
        if (!allocatedType.isScalarType()) {
            return false
        }

        // Check all uses - must be direct loads/stores only
        for (block in function.basicBlocks) {
            for (instruction in block.instructions) {
                when (instruction) {
                    is IRInstruction.Load -> {
                        // Load from this alloca is fine
                    }
                    // Store TO this alloca is fine, but store OF this alloca's address is not (address escapes)
                    is IRInstruction.Store -> if (instruction.value === this) {
                        return false
                    }

                    // GEP on the alloca means we're doing aggregate access
                    is IRInstruction.GEP -> if (instruction.ptr === this) {
                        return false
                    }

                    // Address passed to call means it escapes
                    is IRInstruction.Call -> if (this in instruction.args) {
                        return false
                    }

                    // PHI of a pointer means address escapes
                    is IRInstruction.Phi -> if (instruction.incoming.any { it.first === this }) {
                        return false
                    }

                    // Check if alloca is used as an operand (other than in load ptr)
                    else -> if (this in instruction.operands()) {
                        return false
                    }
                }
            }
        }

        return true
    }

    /**
     * Check if a type can be held in a register.
     */
    private fun IRType.isScalarType(): Boolean = when (this) {
        is IRType.Int -> true
        is IRType.Float -> true
        is IRType.Double -> true
        is IRType.Pointer -> true
        else -> false
    }

    /**
     * Promote a single alloca to an SSA form.
     */
    private fun promoteAlloca(
        alloca: IRInstruction.Alloca,
        function: IRFunction,
        cfg: CFGInfo,
        domInfo: DominanceInfo
    ) {
        // Collect stores and loads for this alloca
        val stores = mutableListOf<StoreInfo>()
        val loads = mutableListOf<LoadInfo>()

        for (block in function.basicBlocks) {
            for (instruction in block.instructions) {
                when (instruction) {
                    is IRInstruction.Store -> {
                        if (instruction.ptr === alloca) {
                            stores.add(StoreInfo(instruction, block))
                        }
                    }

                    is IRInstruction.Load -> {
                        if (instruction.ptr === alloca) {
                            loads.add(LoadInfo(instruction, block))
                        }
                    }

                    else -> {}
                }
            }
        }

        // If there are no stores, all loads get undefined value
        if (stores.isEmpty()) {
            val undef = createUndef(alloca.allocatedType)
            for (load in loads) {
                function.replaceAllUses(load.instruction, undef)
            }
            removeAllocaAndUses(alloca, stores, loads, function)
            return
        }

        // If there's only one store, and it dominates all loads, simple case
        if (stores.size == 1) {
            val store = stores[0]
            val allLoadsDominated = loads.all { load ->
                if (load.block == store.block) {
                    // Same block: check instruction order
                    val storeIdx = store.block.instructions.indexOf(store.instruction)
                    val loadIdx = load.block.instructions.indexOf(load.instruction)
                    storeIdx < loadIdx
                } else {
                    domInfo.dominates(store.block, load.block)
                }
            }

            if (allLoadsDominated) {
                for (load in loads) {
                    function.replaceAllUses(load.instruction, store.instruction.value)
                }
                removeAllocaAndUses(alloca, stores, loads, function)
                return
            }
        }

        // General case: need PHI nodes
        alloca.promoteWithPhi(stores, function, cfg, domInfo)
    }

    /**
     * Promote an alloca using PHI node insertion.
     */
    private fun IRInstruction.Alloca.promoteWithPhi(
        stores: List<StoreInfo>,
        function: IRFunction,
        cfg: CFGInfo,
        domInfo: DominanceInfo
    ) {
        // Compute blocks where PHI nodes are needed
        val defBlocks = stores.map { it.block }.toSet()
        val phiBlocks = domInfo.iteratedDominanceFrontier(defBlocks)

        // Insert PHI nodes
        val phiNodes = mutableMapOf<IRBasicBlock, IRInstruction.Phi>()
        var phiCounter = 0
        for (block in phiBlocks) {
            val phi = IRInstruction.Phi(
                allocatedType,
                incoming = mutableListOf(),
                name = "${name}.phi.${phiCounter++}"
            )
            // Insert PHI at the beginning of the block
            block.instructions.add(0, phi)
            phiNodes[block] = phi
        }

        // Rename pass: walk dominator tree and track reaching definitions
        val reachingDef = mutableMapOf<IRBasicBlock, IRValue>()
        val processedBlocks = mutableSetOf<IRBasicBlock>()

        // Initialize with undefined for entry
        val entryBlock = function.basicBlocks.first()

        // Process blocks in dominator tree order
        fun processBlock(block: IRBasicBlock, incomingDef: IRValue) {
            if (block in processedBlocks) return
            processedBlocks.add(block)

            var currentDef: IRValue = incomingDef

            // If this block has a PHI for this alloca, that's the new definition
            phiNodes[block]?.let { phi ->
                currentDef = phi
            }

            // Process instructions in order
            val toRemove = mutableListOf<IRInstruction>()
            for (instruction in block.instructions.toList()) {
                when (instruction) {
                    is IRInstruction.Store -> if (instruction.ptr === this) {
                        currentDef = instruction.value
                        toRemove.add(instruction)
                    }

                    is IRInstruction.Load -> if (instruction.ptr === this) {
                        function.replaceAllUses(instruction, currentDef)
                        toRemove.add(instruction)
                    }

                    else -> {}
                }
            }

            // Remove processed loads and stores
            block.instructions.removeAll(toRemove)

            // Record reaching definition at the end of this block
            reachingDef[block] = currentDef

            // Fill in PHI incoming values for successors
            for (succ in cfg.successors(block)) {
                phiNodes[succ]?.let { phi ->
                    // Add incoming value from this block
                    val existingFromBlock = phi.incoming.indexOfFirst { it.second == block }
                    if (existingFromBlock == -1) {
                        phi.addIncoming(currentDef, block)
                    }
                }
            }

            // Process dominated children - use sorted order for determinism
            val children = domInfo.domTreeChildren(block).sortedBy { it.name }
            for (child in children) {
                processBlock(child, currentDef)
            }
        }

        // Start processing from the entry block
        processBlock(entryBlock, createUndef(allocatedType))

        // Fill in remaining PHI incoming edges from unprocessed predecessors
        for ((block, phi) in phiNodes) {
            for (pred in cfg.predecessors(block)) {
                val existingFromPred = phi.incoming.any { it.second == pred }
                if (!existingFromPred) {
                    val def = reachingDef[pred] ?: createUndef(allocatedType)
                    phi.addIncoming(def, pred)
                }
            }
        }

        // Remove trivial PHIs (all incoming values are the same)
        phiNodes.values.removeTrivialPhis(function)

        // Remove the alloca
        val entryInstructions = function.basicBlocks.first().instructions
        entryInstructions.remove(this)
    }

    /**
     * Remove PHI nodes where all incoming values are the same.
     */
    private fun Collection<IRInstruction.Phi>.removeTrivialPhis(function: IRFunction) {
        var changed = true
        while (changed) {
            changed = false
            for (phi in this) {
                val uniqueValues = phi.incoming.map { it.first }
                    .filter { it !== phi }
                    .distinct()

                if (uniqueValues.size == 1) {
                    // All incoming values are the same (or self-references)
                    val replacement = uniqueValues[0]
                    function.replaceAllUses(phi, replacement)

                    // Remove the PHI
                    for (block in function.basicBlocks) {
                        block.instructions.remove(phi)
                    }
                    changed = true
                } else if (uniqueValues.isEmpty()) {
                    // PHI only references itself - unreachable, use undef
                    val undef = createUndef(phi.type)
                    function.replaceAllUses(phi, undef)
                    for (block in function.basicBlocks) {
                        block.instructions.remove(phi)
                    }
                    changed = true
                }
            }
        }
    }

    /**
     * Remove the alloca and all its loads/stores.
     */
    private fun removeAllocaAndUses(
        alloca: IRInstruction.Alloca,
        stores: List<StoreInfo>,
        loads: List<LoadInfo>,
        function: IRFunction
    ) {
        // Remove stores
        for (store in stores) {
            store.block.instructions.remove(store.instruction)
        }

        // Remove loads
        for (load in loads) {
            load.block.instructions.remove(load.instruction)
        }

        // Remove alloca
        function.basicBlocks.first().instructions.remove(alloca)
    }

    /**
     * Create an undefined value of the given type.
     * In LLVM, this would be "undef", but we'll use a zero constant.
     */
    private fun createUndef(type: IRType): IRValue {
        return when (type) {
            is IRType.Int -> IRIntConstant(0, type)
            is IRType.Float -> IRFloatConstant(0.0, type)
            is IRType.Double -> IRFloatConstant(0.0, type)
            is IRType.Pointer -> IRNullPointerConstant(type)
            else -> error("Cannot create undef for type: $type")
        }
    }
}

private data class StoreInfo(val instruction: IRInstruction.Store, val block: IRBasicBlock)
private data class LoadInfo(val instruction: IRInstruction.Load, val block: IRBasicBlock)
