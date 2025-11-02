import kotlinx.io.buffered
import kotlinx.io.files.Path
import kotlinx.io.files.SystemFileSystem
import kotlinx.io.readString
import lexer.LexerImpl
import parser.KodeParser

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

    // 1) Lex
    val lexer = LexerImpl(source)
    val tokens = lexer.tokenize()
    println("-- Tokens --")
    for (t in tokens) println(t)

    // 2) Parse
    println("\n-- Parse Tree --")
    val parser = KodeParser()
    val tree = parser.parse(source)
    println(tree)
}
