import ast.visitor.TypeChecker
import codegen.Codegen
import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import kotlinx.io.writeString
import parser.KodeParser
import platform.posix.system

fun main(args: Array<String>) {
    val path = args.firstOrNull() ?: "examples/9_structs.kode"
    val source: String = SystemFileSystem.source(Path(path)).buffered().readString()
    val runExecutable = args.contains("--run")

    println("Parsing source code...")
    val parser = KodeParser()
    val programAst = parser.parse(source)

    println("Checking types...")
    val typeErrors = TypeChecker(programAst).check()
    if (typeErrors.isNotEmpty()) {
        typeErrors.forEach { println(it) }
        return
    }

    println("Generating LLVM IR...")
    val ir = Codegen.generate(programAst)
    val irPath = Path("output.ll")
    SystemFileSystem.sink(irPath).buffered().use { sink ->
        sink.writeString(ir)
    }

    // Compile LLVM IR to WebAssembly
    println("Generating WebAssembly object file...")
    val objFileResult = system("llc -march=wasm32 -filetype=obj output.ll -o output.o")
    if (objFileResult != 0) {
        println("Generating WebAssembly object file failed with exit code: $objFileResult")
        return
    }
    // Compile alien runtime (C) to WebAssembly object using bare WASI imports (no libc)
    println("Compiling Kode std to WebAssembly object...")
    val alienObjResult = system(
        "clang --target=wasm32-unknown-unknown -O2 -ffreestanding -fno-builtin -c src/runtime/alien_runtime.c -o alien.o"
    )
    if (alienObjResult != 0) {
        println("Compiling alien runtime failed with exit code: $alienObjResult")
        return
    }
    println("Linking WebAssembly object file...")
    val linkingResult = system("wasm-ld output.o alien.o -o output.wasm --export-all")
    if (linkingResult != 0) {
        println("Linking WebAssembly object file failed with exit code: $linkingResult")
        return
    }
    if (runExecutable) {
        println("Running WebAssembly executable...")
        system("wasmtime ./output.wasm --invoke main")
    }
}
