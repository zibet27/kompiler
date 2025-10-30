package lexer

/**
 * A simple, single-pass lexer.
 */
class Lexer(private val source: String) {
    private var index = 0
    private var line = 1
    private var column = 1

    private fun currentPosition(): Position = Position(index, line, column)

    private fun isAtEnd(): Boolean = index >= source.length

    private fun peek(offset: Int = 0): Char? {
        return if (index + offset < source.length) source[index + offset] else null
    }

    private fun advance(): Char? {
        if (isAtEnd()) return null
        val ch = source[index++]
        if (ch == '\n') {
            line += 1
            column = 1
        } else {
            column += 1
        }
        return ch
    }

    private fun makeSpan(start: Position): Span = Span(start = start, end = currentPosition())

    private fun skipWhitespace() {
        while (true) {
            val ch = peek() ?: return
            when (ch) {
                ' ', '\t', '\r', '\n' -> advance()
                // Line comments starting with //
                '/' -> if (peek(1) == '/') {
                    while (peek() != null && peek() != '\n') advance()
                } else return

                else -> return
            }
        }
    }

    private fun isAlpha(c: Char) = c == '_' || c.isLetter()
    private fun isAlnum(c: Char) = isAlpha(c) || c.isDigit()

    private fun lexIdentifierOrKeyword(start: Position): Token {
        val sb = StringBuilder()
        while (true) {
            val c = peek() ?: break
            if (!isAlnum(c)) break
            sb.append(c)
            advance()
        }
        val text = sb.toString()
        return when (text) {
            "fun" -> Token.Fun(span = makeSpan(start))
            "void" -> Token.Void(span = makeSpan(start))
            "i32" -> Token.I32(span = makeSpan(start))
            else -> Token.Identifier(span = makeSpan(start), lexeme = text)
        }
    }

    private fun lexNumber(start: Position): Token {
        val text = StringBuilder()
        while (true) {
            val ch = peek() ?: break
            if (!ch.isDigit()) break
            text.append(ch)
            advance()
        }
        return Token.IntLiteral(span = makeSpan(start), lexeme = text.toString())
    }

    fun nextToken(): Token {
        skipWhitespace()
        if (isAtEnd()) {
            return Token.EOF(span = Span(start = currentPosition(), end = currentPosition()))
        }

        val start = currentPosition()
        return when (val ch = advance()!!) {
            '{' -> Token.LBrace(span = makeSpan(start))
            '}' -> Token.RBrace(span = makeSpan(start))
            '(' -> Token.LParen(span = makeSpan(start))
            ')' -> Token.RParen(span = makeSpan(start))
            ':' -> Token.Colon(span = makeSpan(start))
            ',' -> Token.Comma(span = makeSpan(start))
            ';' -> Token.Semicolon(span = makeSpan(start))
            '+' -> Token.Plus(span = makeSpan(start))
            else -> when {
                ch.isDigit() -> {
                    // We consumed the first digit already
                    // Step back one virtual column to re-use number loop
                    // But easier: seed the buffer by rewinding 1 char in indices
                    // Then call lexNumber.
                    index -= 1
                    if (column > 1) column -= 1 else column = 1
                    lexNumber(start)
                }

                isAlpha(ch) -> {
                    index -= 1
                    if (column > 1) column -= 1 else column = 1
                    lexIdentifierOrKeyword(start)
                }

                else -> throw LexError(message = "Unexpected character '$ch'", position = start)
            }
        }
    }

    fun tokenize(): Sequence<Token> = sequence {
        do {
            val token = nextToken().also { yield(value = it) }
        } while (token !is Token.EOF)
    }
}
