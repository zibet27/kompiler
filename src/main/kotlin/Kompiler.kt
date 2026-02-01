import type.TypeChecker
import codegen.Codegen
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.writeString
import parser.KodeParser

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
        java.io.File(outputDir).mkdirs()
        val parser = KodeParser()
        val programAst = parser.parse(source)

        val typeErrors = TypeChecker(programAst).check()
        if (typeErrors.isNotEmpty()) {
            typeErrors.forEach { println(it) }
            return
        }

        val ir = Codegen.generate(programAst)
        val irPath = Path("$outputDir/output.ll")
        SystemFileSystem.sink(irPath).buffered().use { sink ->
            sink.writeString(ir)
        }

        // Compile LLVM IR to WebAssembly
        val objFileResult = executeCommand("llc -march=wasm32 -filetype=obj $outputDir/output.ll -o $outputDir/output.o")
        if (objFileResult != 0) {
            error("Generating WebAssembly object file failed with exit code: $objFileResult")
        }
        // Compile alien runtime (C) to WebAssembly object using bare WASI imports (no libc)
        val alienObjResult = executeCommand(
            "clang --target=wasm32-unknown-unknown -O2 -ffreestanding -fno-builtin -c src/runtime/alien_runtime.c -o $outputDir/alien.o"
        )
        if (alienObjResult != 0) {
            error("Compiling alien runtime failed with exit code: $alienObjResult")
        }
        val linkingResult = executeCommand("wasm-ld $outputDir/output.o $outputDir/alien.o -o $outputDir/output.wasm --export-all")
        if (linkingResult != 0) {
            error("Linking WebAssembly object file failed with exit code: $linkingResult")
        }
    }

    fun run(source: String) {
        compile(source)
        val runStatus = executeCommand("wasmtime $outputDir/output.wasm --invoke main")
        if (runStatus != 0) {
            error("Running WebAssembly executable failed with exit code: $runStatus")
        }
    }
}