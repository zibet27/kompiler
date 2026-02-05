package llvm.opt.pass

import llvm.IRFunction
import llvm.IRModule
import llvm.opt.analysis.AnalysisPass
import kotlin.reflect.KClass

/**
 * Base interface for all optimization passes.
 * 
 * Optimization passes transform IR while preserving program semantics.
 * They return true if the IR was modified, false otherwise.
 */
interface OptimizationPass {
    /** Unique name for this pass */
    val name: String

    /** List of analysis passes this optimization depends on */
    val dependencies: List<KClass<out AnalysisPass<*, *>>>
        get() = emptyList()

    /**
     * Run optimization on the entire module.
     * Default implementation iterates over all functions.
     * @return true if the module was modified
     */
    fun runOnModule(module: IRModule): Boolean {
        var modified = false
        for (function in module.functions) {
            if (!function.isExternal) {
                modified = runOnFunction(function) || modified
            }
        }
        return modified
    }

    /**
     * Run optimization on a single function.
     * @return true if the function was modified
     */
    fun runOnFunction(function: IRFunction): Boolean
}

/**
 * A pass that runs until no more changes are made (fixed-point).
 */
interface IterativePass : OptimizationPass {
    /** Maximum number of iterations before giving up */
    val maxIterations: Int
        get() = 100
}

/**
 * Configuration for a pass.
 */
data class PassConfig(
    val enableLogging: Boolean = false,
    val validateAfterPass: Boolean = false
)
