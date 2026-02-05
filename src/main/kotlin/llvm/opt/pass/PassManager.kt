package llvm.opt.pass

import llvm.IRFunction
import llvm.IRModule
import llvm.opt.analysis.AnalysisPass
import llvm.opt.analysis.FunctionAnalysis
import llvm.opt.analysis.ModuleAnalysis
import kotlin.reflect.KClass

/**
 * Manages and executes optimization passes.
 * 
 * The PassManager orchestrates the execution of optimization passes,
 * handles analysis caching, and supports fixed-point iteration.
 */
class PassManager(
    private val config: PassConfig = PassConfig()
) {
    private val passes = mutableListOf<OptimizationPass>()
    private val analyses = mutableMapOf<KClass<out AnalysisPass<*, *>>, AnalysisPass<*, *>>()
    
    // Analysis result caches
    private val functionAnalysisCache = mutableMapOf<Pair<KClass<*>, IRFunction>, Any?>()
    private val moduleAnalysisCache = mutableMapOf<KClass<*>, Any?>()

    /**
     * Add an optimization pass to the pipeline.
     */
    fun add(pass: OptimizationPass): PassManager {
        passes.add(pass)
        return this
    }

    /**
     * Register an analysis pass for use by optimizations.
     */
    fun <T, R> registerAnalysis(analysisClass: KClass<out AnalysisPass<T, R>>, analysis: AnalysisPass<T, R>): PassManager {
        analyses[analysisClass] = analysis
        return this
    }

    /**
     * Get or compute a function analysis result.
     */
    @Suppress("UNCHECKED_CAST")
    fun <R> getFunctionAnalysis(analysisClass: KClass<out FunctionAnalysis<R>>, function: IRFunction): R {
        val cacheKey = analysisClass to function
        return functionAnalysisCache.getOrPut(cacheKey) {
            val analysis = analyses[analysisClass] as? FunctionAnalysis<R>
                ?: error("Analysis ${analysisClass.simpleName} not registered")
            analysis.analyze(function)
        } as R
    }

    /**
     * Get or compute a module analysis result.
     */
    @Suppress("UNCHECKED_CAST")
    fun <R> getModuleAnalysis(analysisClass: KClass<out ModuleAnalysis<R>>, module: IRModule): R {
        return moduleAnalysisCache.getOrPut(analysisClass) {
            val analysis = analyses[analysisClass] as? ModuleAnalysis<R>
                ?: error("Analysis ${analysisClass.simpleName} not registered")
            analysis.analyze(module)
        } as R
    }

    /**
     * Run all registered passes on the module.
     * @return true if any pass modified the module
     */
    fun runOnModule(module: IRModule): Boolean {
        var modified = false
        
        for (pass in passes) {
            if (config.enableLogging) {
                println("Running pass: ${pass.name}")
            }
            
            val passModified = if (pass is IterativePass) {
                runIterativePass(pass, module)
            } else {
                pass.runOnModule(module)
            }
            
            if (passModified) {
                modified = true
                invalidateAnalyses()
                
                if (config.enableLogging) {
                    println("  -> Modified")
                }
            }
        }
        
        return modified
    }

    /**
     * Run passes until no changes are made (fixed-point iteration).
     * @return true if any modifications were made
     */
    fun runToFixedPoint(module: IRModule): Boolean {
        var anyModified = false
        var iteration = 0
        val maxIterations = 100
        
        do {
            val modified = runOnModule(module)
            if (modified) {
                anyModified = true
            } else {
                break
            }
            iteration++
        } while (iteration < maxIterations)
        
        if (config.enableLogging && iteration >= maxIterations) {
            println("Warning: Fixed-point iteration did not converge after $maxIterations iterations")
        }
        
        return anyModified
    }

    /**
     * Run a single pass on the module.
     */
    fun runPass(pass: OptimizationPass, module: IRModule): Boolean {
        val modified = pass.runOnModule(module)
        if (modified) {
            invalidateAnalyses()
        }
        return modified
    }

    private fun runIterativePass(pass: IterativePass, module: IRModule): Boolean {
        var anyModified = false
        var iteration = 0
        
        do {
            val modified = pass.runOnModule(module)
            if (modified) {
                anyModified = true
                invalidateAnalyses()
            } else {
                break
            }
            iteration++
        } while (iteration < pass.maxIterations)
        
        return anyModified
    }

    /**
     * Invalidate all cached analysis results.
     * Called after any transformation pass modifies the IR.
     */
    fun invalidateAnalyses() {
        functionAnalysisCache.clear()
        moduleAnalysisCache.clear()
    }

    /**
     * Clear all passes and analyses.
     */
    fun clear() {
        passes.clear()
        analyses.clear()
        invalidateAnalyses()
    }
}

/**
 * Extension for easy pass manager construction.
 */
fun passManager(config: PassConfig = PassConfig(), init: PassManager.() -> Unit): PassManager {
    return PassManager(config).apply(init)
}
