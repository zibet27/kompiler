import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import lexer.Lexer

fun main(args: Array<String>) {
    val path = args.firstOrNull()
    val source: String = if (path != null) {
        SystemFileSystem.source(Path(path)).buffered().readString()
    } else {
        // Default demo program if no path is provided
        """
        fun add{ a: i32, b: i32 }: i32 (
            a + b;
        );

        fun main{}: void (
            add{ 2, 3 };
        );
        """.trimIndent()
    }

    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    for (t in tokens) println(t)
}
