import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString

fun main(args: Array<String>) {
    val path = args.firstOrNull() ?: "examples/14_complex_expression.kode"
    val source: String = SystemFileSystem.source(Path(path)).buffered().readString()
    val runExecutable = args.contains("--run")
    val compiler = Kompiler()
    if (runExecutable) {
        compiler.run(source)
    } else {
        compiler.compile(source)
    }
}
