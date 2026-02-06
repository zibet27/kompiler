package llvm.opt.pass

import llvm.IRModule
import llvm.IRPrinter

/**
 * Manages and executes optimization passes.
 *
 * The PassManager orchestrates the execution of optimization passes.
 */
class PassManager(
    private val config: PassConfig = PassConfig()
) {
    private val passes = mutableListOf<OptimizationPass>()

    fun add(pass: OptimizationPass) = passes.add(pass)

    /**
     * Run all registered passes on the module.
     * @return true if any pass modified the module
     */
    fun runOnModule(module: IRModule): Boolean {
        val printer = if (config.printVisualization) IRPrinter() else null

        if (config.printVisualization) {
            println("=".repeat(60))
            println("OPTIMIZATION VISUALIZATION")
            println("=".repeat(60))
            println("\n--- BEFORE OPTIMIZATIONS ---")
            println(printer!!.print(module))
        }

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
                if (config.enableLogging) println("  -> Modified")

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
            iteration++
        } while (iteration < pass.maxIterations)

        return anyModified
    }
}
