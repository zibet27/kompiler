import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import llvm.opt.pass.OptimizationConfig

fun main(args: Array<String>) {
    if (args.isEmpty() || args.contains("--help") || args.contains("-h")) {
        printUsage()
        return
    }

    val path = args.firstOrNull { !it.startsWith("-") } ?: run {
        println("Error: No input file specified")
        printUsage()
        return
    }

    val source: String = SystemFileSystem.source(Path(path)).buffered().readString()
    val runExecutable = args.contains("--run")

    require(path.extension() == "kode") {
        "Only kode is supported"
    }

    val optConfig = OptimizationConfig(
        enableMem2Reg = !args.contains("--no-mem2reg"),
        enableInlining = !args.contains("--no-inline"),
        printVisualization = args.contains("--print-opt"),
        printWasm = args.contains("--print-wasm")
    )

    val compiler = Kompiler(path.filename(), optConfig)
    if (runExecutable) {
        compiler.run(source)
    } else {
        compiler.compile(source)
    }
}

private fun printUsage() {
    println("""
        |Usage: kompiler <path-to-file> [options]
        |
        |Options:
        |  --run           Run the compiled WebAssembly after compilation
        |  --no-mem2reg    Disable mem2reg optimization (alloca to SSA promotion)
        |  --no-inline     Disable function inlining optimization
        |  --print-opt     Print IR visualization before/after each optimization pass
        |  --print-wasm    Print generated WebAssembly text format (requires wabt/wasm2wat)
        |  -h, --help      Show this help message
    """.trimMargin())
}
