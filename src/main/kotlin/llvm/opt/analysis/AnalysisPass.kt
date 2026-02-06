package llvm.opt.analysis

import llvm.IRFunction
import llvm.IRModule

/**
 * Base interface for analysis passes.
 * 
 * Analysis passes compute information about the IR without modifying it.
 * The result type R is specific to each analysis.
 * 
 * @param T The type being analyzed (IRModule or IRFunction)
 * @param R The result type of the analysis
 */
interface AnalysisPass<T, R> {
    /** Unique name for this analysis */
    val name: String

    /**
     * Run the analysis on the target.
     */
    fun analyze(target: T): R
}

/**
 * Analysis that operates on a single function.
 */
interface FunctionAnalysis<R> : AnalysisPass<IRFunction, R>

/**
 * Analysis that operates on the entire module.
 */
interface ModuleAnalysis<R> : AnalysisPass<IRModule, R>