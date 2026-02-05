import codegen.Codegen
import llvm.IRModule
import llvm.opt.pass.InliningPass
import llvm.opt.pass.Mem2RegPass
import llvm.opt.pass.PassManager
import parser.KodeParser
import type.TypeChecker
import wasm.WasmBackend
import java.io.File

fun String.filename() = substringAfterLast(delimiter = "/").substringBeforeLast(delimiter = ".")

fun String.extension() = substringAfterLast(delimiter = ".")

class Kompiler(moduleName: String) {
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
        val passManager = PassManager()
        passManager.add(Mem2RegPass())
        passManager.add(InliningPass())
        passManager.runOnModule(irModule)

        // Compile to WebAssembly using pure Kotlin backend
        val wasmBytes = WasmBackend().compile(irModule)
        File(outputPath).writeBytes(wasmBytes)
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