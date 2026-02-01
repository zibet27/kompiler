package lexer

interface Lexer {
    fun nextToken(): Token

    fun tokenize(): Sequence<Token>
}

interface LexerFactory {
    fun forSource(source: String): Lexer
}

/**
 * A simple, single-pass lexer.
 */
class LexerImpl(private val source: String) : Lexer {
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
        return when (val text = sb.toString()) {
            "fun" -> Token.Fun(span = makeSpan(start))
            "void" -> Token.Void(span = makeSpan(start))
            "i32" -> Token.I32(span = makeSpan(start))
            "u8" -> Token.U8(span = makeSpan(start))
            "f64" -> Token.F64(span = makeSpan(start))
            "alien" -> Token.Alien(span = makeSpan(start))
            "object" -> Token.Object(span = makeSpan(start))
            "typealias" -> Token.TypeAlias(span = makeSpan(start))
            "if" -> Token.If(span = makeSpan(start))
            "else" -> Token.Else(span = makeSpan(start))
            "for" -> Token.For(span = makeSpan(start))
            "while" -> Token.While(span = makeSpan(start))
            "do" -> Token.Do(span = makeSpan(start))
            "switch" -> Token.Switch(span = makeSpan(start))
            "with" -> Token.With(span = makeSpan(start))
            "skip" -> Token.Skip(span = makeSpan(start))
            "stop" -> Token.Stop(span = makeSpan(start))
            "ptr" -> Token.Ptr(span = makeSpan(start))
            else -> Token.Identifier(span = makeSpan(start), lexeme = text)
        }
    }

    private fun lexNumber(start: Position): Token {
        val intPart = StringBuilder()
        while (true) {
            val ch = peek() ?: break
            if (!ch.isDigit()) break
            intPart.append(ch)
            advance()
        }
        // Optional fractional part: '.' DIGITS
        if (peek() == '.' && (peek(1)?.isDigit() == true)) {
            // consume '.'
            advance()
            val fracPart = StringBuilder()
            while (true) {
                val ch = peek() ?: break
                if (!ch.isDigit()) break
                fracPart.append(ch)
                advance()
            }
            return Token.FloatLiteral(span = makeSpan(start), lexeme = "$intPart.$fracPart")
        }
        return Token.IntLiteral(span = makeSpan(start), lexeme = intPart.toString())
    }

    private fun lexString(start: Position): Token {
        // opening " was already consumed
        val sb = StringBuilder()
        while (true) {
            val ch = advance() ?: throw LexError("Unterminated string literal", start)
            if (ch == '"') break
            if (ch == '\\') {
                val esc = advance() ?: throw LexError("Unterminated escape sequence", currentPosition())
                when (esc) {
                    'n' -> sb.append('\n')
                    't' -> sb.append('\t')
                    'r' -> sb.append('\r')
                    '"' -> sb.append('"')
                    '\\' -> sb.append('\\')
                    else -> sb.append(esc) // keep unknown escapes as is
                }
            } else {
                sb.append(ch)
            }
        }
        return Token.StringLiteral(span = makeSpan(start), lexeme = sb.toString())
    }

    private fun lexChar(start: Position): Token {
        // opening '\'' already consumed
        val ch = advance() ?: throw LexError("Unterminated char literal", start)
        val value = if (ch == '\\') {
            val esc = advance() ?: throw LexError("Unterminated char escape", currentPosition())
            when (esc) {
                'n' -> '\n'
                't' -> '\t'
                'r' -> '\r'
                '\'' -> '\''
                '"' -> '"'
                '\\' -> '\\'
                else -> esc
            }
        } else ch

        val closing = advance() ?: throw LexError("Unterminated char literal", currentPosition())
        if (closing != '\'') throw LexError("Invalid character literal (expected closing ' )", currentPosition())
        return Token.CharLiteral(span = makeSpan(start), lexeme = value.toString())
    }

    override fun nextToken(): Token {
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
            '[' -> Token.LBracket(span = makeSpan(start))
            ']' -> Token.RBracket(span = makeSpan(start))
            ':' -> Token.Colon(span = makeSpan(start))
            ',' -> Token.Comma(span = makeSpan(start))
            ';' -> Token.Semicolon(span = makeSpan(start))
            '.' -> Token.Dot(span = makeSpan(start))
            '+' -> if (peek() == '+') { advance(); Token.PlusPlus(makeSpan(start)) } else Token.Plus(makeSpan(start))
            '-' -> when (peek()) {
                '-' -> { advance(); Token.MinusMinus(makeSpan(start)) }
                '>' -> { advance(); Token.Arrow(makeSpan(start)) }
                else -> Token.Minus(makeSpan(start))
            }
            '*' -> Token.Star(span = makeSpan(start))
            '%' -> Token.Percent(span = makeSpan(start))
            '=' -> if (peek() == '=') { advance(); Token.EqualEqual(makeSpan(start)) } else Token.Assign(makeSpan(start))
            '!' -> if (peek() == '=') { advance(); Token.BangEqual(makeSpan(start)) } else Token.Bang(makeSpan(start))
            '~' -> if (peek() == '>') { advance(); Token.TildeGreater(makeSpan(start)) } else Token.Tilde(makeSpan(start))
            '&' -> if (peek() == '&') { advance(); Token.AmpAmp(makeSpan(start)) } else Token.Amp(makeSpan(start))
            '|' -> if (peek() == '|') { advance(); Token.PipePipe(makeSpan(start)) } else Token.Pipe(makeSpan(start))
            '^' -> Token.Caret(makeSpan(start))
            '<' -> when (peek()) {
                '=' -> { advance(); Token.LessEqual(makeSpan(start)) }
                '<' -> { advance(); Token.ShiftLeft(makeSpan(start)) }
                else -> Token.Less(makeSpan(start))
            }
            '>' -> when (peek()) {
                '=' -> { advance(); Token.GreaterEqual(makeSpan(start)) }
                '>' -> { advance(); Token.ShiftRight(makeSpan(start)) }
                else -> Token.Greater(makeSpan(start))
            }
            '/' -> if (peek() == '/') {
                // This branch should not happen due to skipWhitespace handling, but keep safe
                while (peek() != null && peek() != '\n') advance()
                nextToken()
            } else Token.Slash(makeSpan(start))
            '"' -> lexString(start)
            '\'' -> lexChar(start)
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

    override fun tokenize(): Sequence<Token> = sequence {
        do {
            val token = nextToken().also { yield(value = it) }
        } while (token !is Token.EOF)
    }

    companion object : LexerFactory {
        override fun forSource(source: String): Lexer = LexerImpl(source)
    }
}
