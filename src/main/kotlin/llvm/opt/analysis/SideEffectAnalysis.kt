package llvm.opt.analysis

import llvm.*

/**
 * Side effect analysis.
 * 
 * Determines which instructions and functions have side effects,
 * read memory, or write memory. Used for optimization decisions.
 */
class SideEffectAnalysis : ModuleAnalysis<SideEffectInfo> {
    override val name = "side-effects"

    override fun analyze(target: IRModule): SideEffectInfo {
        return SideEffectInfo.compute(target)
    }
}

/**
 * Result of side effect analysis.
 */
class SideEffectInfo private constructor(
    private val module: IRModule,
    private val functionEffects: Map<IRFunction, FunctionEffects>
) {
    /**
     * Check if an instruction has side effects.
     * Side effects include: writing to memory, I/O, exceptions, etc.
     */
    fun hasSideEffects(instruction: IRInstruction): Boolean {
        return when (instruction) {
            is IRInstruction.Store -> true
            is IRInstruction.Call -> {
                val callee = resolveCallee(instruction.function)
                callee == null || !isPure(callee)
            }
            // These are pure computations
            is IRInstruction.Binary,
            is IRInstruction.Unary,
            is IRInstruction.Load,
            is IRInstruction.GEP,
            is IRInstruction.ICmp,
            is IRInstruction.FCmp,
            is IRInstruction.Cast,
            is IRInstruction.Phi,
            is IRInstruction.Alloca -> false
            // Control flow
            is IRInstruction.Ret,
            is IRInstruction.Br,
            is IRInstruction.CondBr -> false
        }
    }

    /**
     * Check if an instruction may read from memory.
     */
    fun mayReadMemory(instruction: IRInstruction): Boolean {
        return when (instruction) {
            is IRInstruction.Load -> true
            is IRInstruction.Call -> {
                val callee = resolveCallee(instruction.function)
                callee == null || mayReadMemory(callee)
            }
            else -> false
        }
    }

    /**
     * Check if an instruction may write to memory.
     */
    fun mayWriteMemory(instruction: IRInstruction): Boolean {
        return when (instruction) {
            is IRInstruction.Store -> true
            is IRInstruction.Call -> {
                val callee = resolveCallee(instruction.function)
                callee == null || mayWriteMemory(callee)
            }
            else -> false
        }
    }

    /**
     * Check if a function is pure (no side effects, no memory access).
     */
    fun isPure(function: IRFunction): Boolean {
        return functionEffects[function]?.isPure ?: false
    }

    /**
     * Check if a function may read memory.
     */
    fun mayReadMemory(function: IRFunction): Boolean {
        return functionEffects[function]?.mayReadMemory ?: true
    }

    /**
     * Check if a function may write memory.
     */
    fun mayWriteMemory(function: IRFunction): Boolean {
        return functionEffects[function]?.mayWriteMemory ?: true
    }

    /**
     * Check if a function may have side effects beyond memory.
     */
    fun mayHaveSideEffects(function: IRFunction): Boolean {
        return functionEffects[function]?.mayHaveSideEffects ?: true
    }

    /**
     * Resolve a call target to a function, if possible.
     */
    private fun resolveCallee(target: IRValue): IRFunction? {
        return when (target) {
            is IRFunction -> target
            else -> module.functions.find { it.ref() == target.ref() }
        }
    }

    companion object {
        /**
         * Compute side effect info for a module.
         */
        fun compute(module: IRModule): SideEffectInfo {
            val effects = mutableMapOf<IRFunction, FunctionEffects>()
            
            // First pass: compute local effects (ignoring calls)
            for (function in module.functions) {
                effects[function] = computeLocalEffects(function)
            }
            
            // Iterative pass: propagate effects through call graph
            var changed = true
            while (changed) {
                changed = false
                for (function in module.functions) {
                    if (function.isExternal) continue
                    
                    val current = effects[function]!!
                    var newEffects = current
                    
                    // Check all calls in the function
                    for (block in function.basicBlocks) {
                        for (inst in block.instructions) {
                            if (inst is IRInstruction.Call) {
                                val callee = when (val target = inst.function) {
                                    is IRFunction -> target
                                    else -> module.functions.find { it.ref() == target.ref() }
                                }
                                
                                if (callee != null) {
                                    val calleeEffects = effects[callee] ?: FunctionEffects.UNKNOWN
                                    newEffects = newEffects.merge(calleeEffects)
                                } else {
                                    // Unknown callee - assume worst case
                                    newEffects = FunctionEffects.UNKNOWN
                                }
                            }
                        }
                    }
                    
                    if (newEffects != current) {
                        effects[function] = newEffects
                        changed = true
                    }
                }
            }
            
            return SideEffectInfo(module, effects)
        }

        /**
         * Compute local effects of a function (not considering callees).
         */
        private fun computeLocalEffects(function: IRFunction): FunctionEffects {
            if (function.isExternal) {
                // External functions are assumed to have unknown effects
                return FunctionEffects.UNKNOWN
            }
            
            var mayRead = false
            var mayWrite = false
            var mayHaveSideEffects = false
            
            for (block in function.basicBlocks) {
                for (inst in block.instructions) {
                    when (inst) {
                        is IRInstruction.Load -> mayRead = true
                        is IRInstruction.Store -> {
                            mayWrite = true
                            mayHaveSideEffects = true
                        }
                        is IRInstruction.Call -> {
                            // Will be refined in iterative pass
                            mayRead = true
                            mayWrite = true
                            mayHaveSideEffects = true
                        }
                        else -> {}
                    }
                }
            }
            
            return FunctionEffects(mayRead, mayWrite, mayHaveSideEffects)
        }
    }
}

/**
 * Effects summary for a function.
 */
data class FunctionEffects(
    val mayReadMemory: Boolean,
    val mayWriteMemory: Boolean,
    val mayHaveSideEffects: Boolean
) {
    val isPure: Boolean
        get() = !mayReadMemory && !mayWriteMemory && !mayHaveSideEffects

    /**
     * Merge effects (union of both).
     */
    fun merge(other: FunctionEffects): FunctionEffects {
        return FunctionEffects(
            mayReadMemory = this.mayReadMemory || other.mayReadMemory,
            mayWriteMemory = this.mayWriteMemory || other.mayWriteMemory,
            mayHaveSideEffects = this.mayHaveSideEffects || other.mayHaveSideEffects
        )
    }

    companion object {
        /**
         * Unknown effects - assume the worst.
         */
        val UNKNOWN = FunctionEffects(
            mayReadMemory = true,
            mayWriteMemory = true,
            mayHaveSideEffects = true
        )

        /**
         * Pure function - no effects at all.
         */
        val PURE = FunctionEffects(
            mayReadMemory = false,
            mayWriteMemory = false,
            mayHaveSideEffects = false
        )
    }
}
