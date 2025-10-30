package lexer

/**
 * Source position (1-based line/column; 0-based absolute index).
 */
data class Position(
    val index: Int,
    val line: Int,
    val column: Int,
)

/**
 * A continuous span in the source.
 */
data class Span(
    val start: Position,
    val end: Position,
)

/**
 * A token with its textual representation and source span.
 */
sealed class Token(val span: Span, val lexeme: String) {

    // Single-character symbols
    class LBrace(span: Span) : Token(span, lexeme = "{")

    class RBrace(span: Span) : Token(span, lexeme = "}")
    class LParen(span: Span) : Token(span, lexeme = "(")
    class RParen(span: Span) : Token(span, lexeme = ")")
    class Colon(span: Span) : Token(span, lexeme = ":")
    class Comma(span: Span) : Token(span, lexeme = ",")
    class Semicolon(span: Span) : Token(span, lexeme = ";")
    class Plus(span: Span) : Token(span, lexeme = "+")

    // Identifiers and literals
    class Identifier(span: Span, lexeme: String) : Token(span, lexeme)
    class IntLiteral(span: Span, lexeme: String) : Token(span, lexeme)

    class Fun(span: Span) : Token(span, lexeme = "fun")
    class Void(span: Span) : Token(span, lexeme = "void")
    class I32(span: Span) : Token(span, lexeme = "i32")

    // End of the file
    class EOF(span: Span) : Token(span, lexeme = "<EOF>")

    override fun toString(): String = buildString {
        append("Token")
        append("('")
        append(lexeme)
        append("' @ ")
        append(span.start.line).append(":").append(span.start.column)
        append("-")
        append(span.end.line).append(":").append(span.end.column)
        append(")")
    }
}

/** Lexer error with a precise position. */
class LexError(message: String, position: Position) :
    RuntimeException("$message at ${position.line}:${position.column}")
