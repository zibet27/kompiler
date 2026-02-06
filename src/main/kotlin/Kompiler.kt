import codegen.Codegen
import llvm.IRModule
import llvm.opt.pass.*
import parser.KodeParser
import type.TypeChecker
import wasm.WasmBackend
import java.io.File

fun String.filename() = substringAfterLast(delimiter = "/").substringBeforeLast(delimiter = ".")

fun String.extension() = substringAfterLast(delimiter = ".")

class Kompiler(
    moduleName: String,
    private val optConfig: OptimizationConfig = OptimizationConfig()
) {
    private val outputDir = "build/out"
    private val outputPath = "$outputDir/${moduleName}.wasm"

    fun compile(source: String) {
        File(outputDir).mkdirs()
        val parser = KodeParser()
        val programAst = parser.parse(source)

        val typeErrors = TypeChecker(programAst).check()
        if (typeErrors.isNotEmpty()) {
            typeErrors.forEach { println(it) }
            return
        }

        // Generate LLVM IR module
        val codegen = Codegen()
        codegen.visit(programAst)
        val irModule: IRModule = codegen.module

        // Run LLVM IR optimizations
        val passConfig = PassConfig(printVisualization = optConfig.printVisualization)
        val passManager = PassManager(passConfig)
        if (optConfig.enableMem2Reg) {
            passManager.add(Mem2RegPass())
        }
        if (optConfig.enableInlining) {
            passManager.add(InliningPass())
        }
        passManager.runOnModule(irModule)

        // Compile to WebAssembly using pure Kotlin backend
        val wasmBytes = WasmBackend().compile(irModule)
        File(outputPath).writeBytes(wasmBytes)

        // Print WASM using wabt if requested
        if (optConfig.printWasm) {
            printWasmText(outputPath)
        }
    }

    private fun printWasmText(wasmPath: String) {
        try {
            val process = ProcessBuilder("wasm2wat", wasmPath)
                .redirectErrorStream(true)
                .start()
            val output = process.inputStream.bufferedReader().readText()
            val exitCode = process.waitFor()
            if (exitCode == 0) {
                println("\n${"=".repeat(60)}")
                println("GENERATED WASM (via wasm2wat)")
                println("=".repeat(60))
                println(output)
            } else {
                println("Warning: wasm2wat failed with exit code $exitCode")
                println(output)
            }
        } catch (e: Exception) {
            println("Warning: Could not run wasm2wat. Is wabt installed? (${e.message})")
        }
    }

    fun run(source: String) {
        compile(source)

        val runStatus = ProcessBuilder("node", "test_runner.js", outputPath)
            .inheritIO()
            .start()
            .waitFor()

        if (runStatus != 0) {
            error("Running WebAssembly executable failed with exit code: $runStatus")
        }
    }
}