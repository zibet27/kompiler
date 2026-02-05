import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString

fun main(args: Array<String>) {
    val path = args.firstOrNull() ?: run {
        println("Usage: kompiler <path-to-file> [--run]")
        return
    }
    val source: String = SystemFileSystem.source(Path(path)).buffered().readString()
    val runExecutable = args.contains("--run")
    require(path.extension() == "kode") {
        "Only kode is supported"
    }
    val compiler = Kompiler(path.filename())
    if (runExecutable) {
        compiler.run(source)
    } else {
        compiler.compile(source)
    }
}
