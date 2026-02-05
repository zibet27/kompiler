import type.TypeChecker
import codegen.Codegen
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.writeString
import parser.KodeParser
import wasm.WasmBackend
import llvm.IRModule
import llvm.IRPrinter
import java.io.File

class Kompiler {
    private val outputDir = "build/out"

    private fun executeCommand(command: String): Int {
        val parts = command.split(" ")
        return ProcessBuilder(*parts.toTypedArray())
            .inheritIO()
            .start()
            .waitFor()
    }

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

        // Write LLVM IR text (for debugging)
        val irText = IRPrinter().print(irModule)
        val irPath = Path("$outputDir/output.ll")
        SystemFileSystem.sink(irPath).buffered().use { sink ->
            sink.writeString(irText)
        }

        // Compile to WebAssembly using pure Kotlin backend
        val wasmBytes = WasmBackend().compile(irModule)
        File("$outputDir/output.wasm").writeBytes(wasmBytes)

        println("Compiled to WebAssembly: $outputDir/output.wasm (${wasmBytes.size} bytes)")
    }

    fun run(source: String) {
        compile(source)
        val runStatus = executeCommand("wasmtime $outputDir/output.wasm --invoke main")
        if (runStatus != 0) {
            error("Running WebAssembly executable failed with exit code: $runStatus")
        }
    }
}