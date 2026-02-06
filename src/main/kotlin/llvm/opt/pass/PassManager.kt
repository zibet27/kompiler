package llvm.opt.pass

import llvm.IRFunction
import llvm.IRModule
import llvm.IRPrinter
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
     * Run all registered passes on the module.
     * @return true if any pass modified the module
     */
    fun runOnModule(module: IRModule): Boolean {
        var modified = false
        val printer = if (config.printVisualization) IRPrinter() else null

        if (config.printVisualization) {
            println("=".repeat(60))
            println("OPTIMIZATION VISUALIZATION")
            println("=".repeat(60))
            println("\n--- BEFORE OPTIMIZATIONS ---")
            println(printer!!.print(module))
        }

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

                if (config.printVisualization) {
                    println("\n--- AFTER ${pass.name.uppercase()} ---")
                    println(printer!!.print(module))
                }
            } else if (config.printVisualization) {
                println("\n--- ${pass.name.uppercase()} (no changes) ---")
            }
        }

        if (config.printVisualization) {
            println("=".repeat(60))
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

    private fun runIterativePass(pass: IterativePass, module: IRModule): Boolean {
        var anyModified = false
        var iteration = 0

        do {
            if (!pass.runOnModule(module)) {
                break
            }
            anyModified = true
            invalidateAnalyses()
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
}
