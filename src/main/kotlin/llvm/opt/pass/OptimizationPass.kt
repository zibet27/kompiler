package llvm.opt.pass

import llvm.IRFunction
import llvm.IRModule

/**
 * Base interface for all optimization passes.
 * 
 * Optimization passes transform IR while preserving program semantics.
 * They return true if the IR was modified, false otherwise.
 */
interface OptimizationPass {
    val name: String

    /**
     * Run optimization on the entire module.
     * The default implementation iterates over all functions.
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
 * Configuration for optimization passes.
 */
data class PassConfig(
    val enableLogging: Boolean = false,
    val validateAfterPass: Boolean = false,
    val printVisualization: Boolean = false
)

/**
 * Configuration for which optimizations to enable/disable.
 */
data class OptimizationConfig(
    val enableMem2Reg: Boolean = true,
    val enableInlining: Boolean = true,
    val printVisualization: Boolean = false,
    val printWasm: Boolean = false
)
