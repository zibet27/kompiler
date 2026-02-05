package llvm.opt.pass

import llvm.*
import llvm.opt.analysis.CFGInfo
import llvm.opt.util.IRCloner
import llvm.opt.util.countInstructions
import llvm.opt.util.findReturns

/**
 * Function inlining optimization pass.
 * 
 * Replaces function calls with the body of the called function.
 * This can improve performance by eliminating call overhead and
 * enabling further optimizations.
 * 
 * Inlining criteria:
 * - Function is not external
 * - Function is not recursive (directly or indirectly)
 * - Function size is below threshold
 * - Call site is not in an infinite loop (optional)
 */
class InliningPass(
    /** Maximum instruction count for a function to be inlined */
    private val sizeThreshold: Int = 50,
    /** Always inline functions with this many instructions or fewer */
    private val alwaysInlineThreshold: Int = 5,
    /** Maximum total growth allowed (ratio of new size to old size) */
    private val maxGrowthRatio: Double = 2.0
) : OptimizationPass {
    
    override val name = "inline"

    /** Global counter for unique inline naming across all inlines */
    private var inlineCounter = 0

    override fun runOnModule(module: IRModule): Boolean {
        var modified = false
        
        // Build call graph to detect recursion
        val callGraph = buildCallGraph(module)
        val recursiveFunctions = findRecursiveFunctions(callGraph)
        
        // Process each function
        for (function in module.functions.toList()) {
            if (function.isExternal) continue
            
            val functionModified = processFunction(function, module, recursiveFunctions)
            modified = modified || functionModified
        }
        
        return modified
    }

    override fun runOnFunction(function: IRFunction): Boolean {
        // This pass needs module context, so this is a no-op
        return false
    }

    /**
     * Process a single function, inlining call sites where beneficial.
     * Re-scans after each inline to avoid stale block references.
     */
    private fun processFunction(
        caller: IRFunction,
        module: IRModule,
        recursiveFunctions: Set<IRFunction>
    ): Boolean {
        var modified = false
        val originalSize = countInstructions(caller)
        var currentSize = originalSize

        // Keep inlining until no more opportunities (re-scan after each inline)
        var madeProgress: Boolean
        do {
            madeProgress = false

            // Find best inlineable call site (smallest callee that passes criteria)
            val callSite = findBestCallSite(caller, module, recursiveFunctions, currentSize, originalSize)

            if (callSite != null) {
                val inlined = inlineCallSite(callSite, caller, module)
                if (inlined) {
                    modified = true
                    madeProgress = true
                    currentSize = countInstructions(caller)
                }
            }
        } while (madeProgress)

        return modified
    }

    /**
     * Find the best call site to inline (smallest callee that passes criteria).
     */
    private fun findBestCallSite(
        caller: IRFunction,
        module: IRModule,
        recursiveFunctions: Set<IRFunction>,
        currentSize: Int,
        originalSize: Int
    ): CallSiteInfo? {
        var bestSite: CallSiteInfo? = null
        var bestSize = Int.MAX_VALUE

        for (block in caller.basicBlocks) {
            for (instruction in block.instructions) {
                if (instruction is IRInstruction.Call) {
                    val callee = resolveCallee(instruction.function, module)
                    if (callee != null) {
                        val callSite = CallSiteInfo(instruction, block, callee)
                        if (shouldInline(callSite, caller, recursiveFunctions, currentSize, originalSize)) {
                            val calleeSize = countInstructions(callee)
                            if (calleeSize < bestSize) {
                                bestSize = calleeSize
                                bestSite = callSite
                            }
                        }
                    }
                }
            }
        }

        return bestSite
    }

    /**
     * Decide whether to inline a call site.
     */
    private fun shouldInline(
        callSite: CallSiteInfo,
        caller: IRFunction,
        recursiveFunctions: Set<IRFunction>,
        currentSize: Int,
        originalSize: Int
    ): Boolean {
        val callee = callSite.callee
        
        // Don't inline external functions
        if (callee.isExternal) return false
        
        // Don't inline recursive functions
        if (callee in recursiveFunctions) return false
        
        // Don't inline self-recursive calls
        if (callee == caller) return false
        
        val calleeSize = countInstructions(callee)
        
        // Always inline very small functions
        if (calleeSize <= alwaysInlineThreshold) return true
        
        // Don't inline large functions
        if (calleeSize > sizeThreshold) return false
        
        // Check growth ratio
        val projectedSize = currentSize + calleeSize - 1  // -1 for the call instruction
        if (projectedSize > originalSize * maxGrowthRatio) return false
        
        return true
    }

    /**
     * Inline a single call site.
     */
    private fun inlineCallSite(
        callSite: CallSiteInfo,
        caller: IRFunction,
        module: IRModule
    ): Boolean {
        val call = callSite.call
        val block = callSite.block
        val callee = callSite.callee
        
        // Create the cloner with unique prefix and map parameters to arguments
        val inlineId = inlineCounter++
        val cloner = IRCloner(namePrefix = "inl${inlineId}.${callee.name}")
        for ((param, arg) in callee.parameters.zip(call.args)) {
            cloner.mapValue(param, arg)
        }
        
        // Clone the callee's body
        val clonedBlocks = cloner.cloneFunctionBody(callee)
        if (clonedBlocks.isEmpty()) return false
        
        val clonedEntry = clonedBlocks.first()
        
        // Find return instructions in the cloned body
        val returns = mutableListOf<Pair<IRInstruction.Ret, IRBasicBlock>>()
        for (clonedBlock in clonedBlocks) {
            for (inst in clonedBlock.instructions) {
                if (inst is IRInstruction.Ret) {
                    returns.add(inst to clonedBlock)
                }
            }
        }
        
        // Split the caller block at the call site
        val callIndex = block.instructions.indexOf(call)
        val instructionsAfterCall = block.instructions.subList(callIndex + 1, block.instructions.size).toList()
        
        // Remove instructions after call (and the call itself)
        while (block.instructions.size > callIndex) {
            block.instructions.removeAt(block.instructions.size - 1)
        }
        
        // Create continuation block for instructions after the call
        val continueBlock = IRBasicBlock("inl.continue.${cloner.freshName("cont")}")
        continueBlock.instructions.addAll(instructionsAfterCall)
        
        // Add branch from caller block to inlined entry
        block.instructions.add(IRInstruction.Br(clonedEntry))
        
        // Handle return value
        if (call.type != IRType.Void && returns.isNotEmpty()) {
            if (returns.size == 1) {
                // Single return - use the value directly
                val (ret, retBlock) = returns[0]
                ret.value?.let { retVal ->
                    // Replace uses of the call result with the return value
                    replaceCallResult(call, retVal, caller, continueBlock)
                }
                
                // Replace return with branch to continue block
                retBlock.instructions.remove(ret)
                retBlock.instructions.add(IRInstruction.Br(continueBlock))
            } else {
                // Multiple returns - need a PHI node
                val phi = IRInstruction.Phi(call.type, mutableListOf(), "inl.retval.${cloner.freshName("phi")}")
                continueBlock.instructions.add(0, phi)
                
                for ((ret, retBlock) in returns) {
                    val retVal = ret.value ?: continue
                    phi.addIncoming(retVal, retBlock)
                    
                    // Replace return with branch to continue block
                    retBlock.instructions.remove(ret)
                    retBlock.instructions.add(IRInstruction.Br(continueBlock))
                }
                
                // Replace uses of the call result with the PHI
                replaceCallResult(call, phi, caller, continueBlock)
            }
        } else {
            // Void return - just redirect to continue block
            for ((ret, retBlock) in returns) {
                retBlock.instructions.remove(ret)
                retBlock.instructions.add(IRInstruction.Br(continueBlock))
            }
        }
        
        // Insert the cloned blocks and continue block into the caller
        val blockIndex = caller.basicBlocks.indexOf(block)
        caller.basicBlocks.addAll(blockIndex + 1, clonedBlocks)
        caller.basicBlocks.add(blockIndex + 1 + clonedBlocks.size, continueBlock)
        
        return true
    }

    /**
     * Replace uses of a call result with a new value.
     */
    private fun replaceCallResult(
        call: IRInstruction.Call,
        newValue: IRValue,
        function: IRFunction,
        continueBlock: IRBasicBlock
    ) {
        for (block in function.basicBlocks) {
            for (inst in block.instructions) {
                replaceValueInInstruction(inst, call, newValue)
            }
        }
        // Also check the continue block
        for (inst in continueBlock.instructions) {
            replaceValueInInstruction(inst, call, newValue)
        }
    }

    /**
     * Replace occurrences of oldValue with newValue in an instruction.
     */
    private fun replaceValueInInstruction(inst: IRInstruction, oldValue: IRValue, newValue: IRValue) {
        when (inst) {
            is IRInstruction.Binary -> {
                if (inst.lhs === oldValue) setField(inst, "lhs", newValue)
                if (inst.rhs === oldValue) setField(inst, "rhs", newValue)
            }
            is IRInstruction.Unary -> {
                if (inst.value === oldValue) setField(inst, "value", newValue)
            }
            is IRInstruction.Load -> {
                if (inst.ptr === oldValue) setField(inst, "ptr", newValue)
            }
            is IRInstruction.Store -> {
                if (inst.value === oldValue) setField(inst, "value", newValue)
                if (inst.ptr === oldValue) setField(inst, "ptr", newValue)
            }
            is IRInstruction.GEP -> {
                if (inst.ptr === oldValue) setField(inst, "ptr", newValue)
                // Handle indices list
                val newIndices = inst.indices.map { if (it === oldValue) newValue else it }
                if (newIndices != inst.indices) {
                    setField(inst, "indices", newIndices)
                }
            }
            is IRInstruction.Call -> {
                if (inst.function === oldValue) setField(inst, "function", newValue)
                // Handle args list
                val newArgs = inst.args.map { if (it === oldValue) newValue else it }
                if (newArgs != inst.args) {
                    setField(inst, "args", newArgs)
                }
            }
            is IRInstruction.Ret -> {
                if (inst.value === oldValue) setField(inst, "value", newValue)
            }
            is IRInstruction.CondBr -> {
                if (inst.condition === oldValue) setField(inst, "condition", newValue)
            }
            is IRInstruction.Phi -> {
                for (i in inst.incoming.indices) {
                    val (value, block) = inst.incoming[i]
                    if (value === oldValue) {
                        inst.incoming[i] = newValue to block
                    }
                }
            }
            is IRInstruction.ICmp -> {
                if (inst.lhs === oldValue) setField(inst, "lhs", newValue)
                if (inst.rhs === oldValue) setField(inst, "rhs", newValue)
            }
            is IRInstruction.FCmp -> {
                if (inst.lhs === oldValue) setField(inst, "lhs", newValue)
                if (inst.rhs === oldValue) setField(inst, "rhs", newValue)
            }
            is IRInstruction.Cast -> {
                if (inst.value === oldValue) setField(inst, "value", newValue)
            }
            else -> {}
        }
    }

    /**
     * Resolve a call target to a function.
     */
    private fun resolveCallee(target: IRValue, module: IRModule): IRFunction? {
        return when (target) {
            is IRFunction -> target
            else -> module.functions.find { it.ref() == target.ref() }
        }
    }

    /**
     * Build a call graph for the module.
     */
    private fun buildCallGraph(module: IRModule): Map<IRFunction, Set<IRFunction>> {
        val callGraph = mutableMapOf<IRFunction, MutableSet<IRFunction>>()
        
        for (function in module.functions) {
            callGraph[function] = mutableSetOf()
            
            if (function.isExternal) continue
            
            for (block in function.basicBlocks) {
                for (inst in block.instructions) {
                    if (inst is IRInstruction.Call) {
                        val callee = resolveCallee(inst.function, module)
                        if (callee != null) {
                            callGraph[function]!!.add(callee)
                        }
                    }
                }
            }
        }
        
        return callGraph
    }

    /**
     * Find all functions that are recursive (directly or indirectly).
     */
    private fun findRecursiveFunctions(callGraph: Map<IRFunction, Set<IRFunction>>): Set<IRFunction> {
        val recursive = mutableSetOf<IRFunction>()
        
        for (function in callGraph.keys) {
            if (isRecursive(function, callGraph, mutableSetOf())) {
                recursive.add(function)
            }
        }
        
        return recursive
    }

    /**
     * Check if a function is recursive (can reach itself through calls).
     */
    private fun isRecursive(
        function: IRFunction,
        callGraph: Map<IRFunction, Set<IRFunction>>,
        visited: MutableSet<IRFunction>
    ): Boolean {
        if (function in visited) return true
        visited.add(function)
        
        for (callee in callGraph[function] ?: emptySet()) {
            if (isRecursive(callee, callGraph, visited)) {
                return true
            }
        }
        
        visited.remove(function)
        return false
    }

    @Suppress("UNCHECKED_CAST")
    private fun setField(obj: Any, fieldName: String, value: Any?) {
        val field = obj::class.java.getDeclaredField(fieldName)
        field.isAccessible = true
        field.set(obj, value)
    }
}

private data class CallSiteInfo(
    val call: IRInstruction.Call,
    val block: IRBasicBlock,
    val callee: IRFunction
)
