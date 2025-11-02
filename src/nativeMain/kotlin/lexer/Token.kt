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
    class LBracket(span: Span) : Token(span, lexeme = "[")
    class RBracket(span: Span) : Token(span, lexeme = "]")
    class Colon(span: Span) : Token(span, lexeme = ":")
    class Comma(span: Span) : Token(span, lexeme = ",")
    class Semicolon(span: Span) : Token(span, lexeme = ";")
    class Plus(span: Span) : Token(span, lexeme = "+")
    class Minus(span: Span) : Token(span, lexeme = "-")
    class Star(span: Span) : Token(span, lexeme = "*")
    class Slash(span: Span) : Token(span, lexeme = "/")
    class Percent(span: Span) : Token(span, lexeme = "%")
    class Assign(span: Span) : Token(span, lexeme = "=")
    class Bang(span: Span) : Token(span, lexeme = "!")
    class Tilde(span: Span) : Token(span, lexeme = "~")
    class Amp(span: Span) : Token(span, lexeme = "&")
    class Pipe(span: Span) : Token(span, lexeme = "|")
    class Caret(span: Span) : Token(span, lexeme = "^")
    class Dot(span: Span) : Token(span, lexeme = ".")

    // Identifiers and literals
    class Identifier(span: Span, lexeme: String) : Token(span, lexeme)
    class IntLiteral(span: Span, lexeme: String) : Token(span, lexeme)
    class CharLiteral(span: Span, lexeme: String) : Token(span, lexeme)
    class StringLiteral(span: Span, lexeme: String) : Token(span, lexeme)

    class Fun(span: Span) : Token(span, lexeme = "fun")
    class Void(span: Span) : Token(span, lexeme = "void")
    class I32(span: Span) : Token(span, lexeme = "i32")
    class U8(span: Span) : Token(span, lexeme = "u8")
    class F64(span: Span) : Token(span, lexeme = "f64")
    // Note: no separate 'char' built-in type; 'char' (if present) lexes as IDENT.
    class Alien(span: Span) : Token(span, lexeme = "alien")
    class Object(span: Span) : Token(span, lexeme = "object")
    class TypeAlias(span: Span) : Token(span, lexeme = "typealias")
    class If(span: Span) : Token(span, lexeme = "if")
    class Else(span: Span) : Token(span, lexeme = "else")
    class For(span: Span) : Token(span, lexeme = "for")
    class While(span: Span) : Token(span, lexeme = "while")
    class Do(span: Span) : Token(span, lexeme = "do")
    class Switch(span: Span) : Token(span, lexeme = "switch")
    class With(span: Span) : Token(span, lexeme = "with")
    // Note: no 'cast' keyword; casts use the '~>' operator only.
    class Skip(span: Span) : Token(span, lexeme = "skip")
    class Stop(span: Span) : Token(span, lexeme = "stop")

    // Multi-character operators
    class PlusPlus(span: Span) : Token(span, lexeme = "++")
    class MinusMinus(span: Span) : Token(span, lexeme = "--")
    class EqualEqual(span: Span) : Token(span, lexeme = "==")
    class BangEqual(span: Span) : Token(span, lexeme = "!=")
    class Less(span: Span) : Token(span, lexeme = "<")
    class LessEqual(span: Span) : Token(span, lexeme = "<=")
    class Greater(span: Span) : Token(span, lexeme = ">")
    class GreaterEqual(span: Span) : Token(span, lexeme = ">=")
    class ShiftLeft(span: Span) : Token(span, lexeme = "<<")
    class ShiftRight(span: Span) : Token(span, lexeme = ">>")
    class AmpAmp(span: Span) : Token(span, lexeme = "&&")
    class PipePipe(span: Span) : Token(span, lexeme = "||")
    class Arrow(span: Span) : Token(span, lexeme = "->")
    class TildeGreater(span: Span) : Token(span, lexeme = "~>")

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

enum class TokenType {
    LBRACE, RBRACE, LPAREN, RPAREN, LBRACKET, RBRACKET, COLON, COMMA, SEMICOLON,
    PLUS, MINUS, STAR, SLASH, PERCENT, ASSIGN, BANG, TILDE, AMP, PIPE, CARET, DOT,
    IDENT, INT, CHAR_LIT, STRING_LIT,
    FUN, VOID, I32, U8, F64,
    ALIEN, OBJECT, TYPEALIAS,
    IF, ELSE, FOR, WHILE, DO, SWITCH, WITH, SKIP, STOP,
    PLUSPLUS, MINUSMINUS, EQEQ, BANGEQ, LT, LTE, GT, GTE, SHL, SHR, ANDAND, OROR, ARROW, TILDE_GT,
    EOF
}

val Token.type: TokenType
    get() = when (this) {
        is Token.LBrace -> TokenType.LBRACE
        is Token.RBrace -> TokenType.RBRACE
        is Token.LParen -> TokenType.LPAREN
        is Token.RParen -> TokenType.RPAREN
        is Token.LBracket -> TokenType.LBRACKET
        is Token.RBracket -> TokenType.RBRACKET
        is Token.Colon -> TokenType.COLON
        is Token.Comma -> TokenType.COMMA
        is Token.Semicolon -> TokenType.SEMICOLON
        is Token.Plus -> TokenType.PLUS
        is Token.Minus -> TokenType.MINUS
        is Token.Star -> TokenType.STAR
        is Token.Slash -> TokenType.SLASH
        is Token.Percent -> TokenType.PERCENT
        is Token.Assign -> TokenType.ASSIGN
        is Token.Bang -> TokenType.BANG
        is Token.Tilde -> TokenType.TILDE
        is Token.Amp -> TokenType.AMP
        is Token.Pipe -> TokenType.PIPE
        is Token.Caret -> TokenType.CARET
        is Token.Dot -> TokenType.DOT
        is Token.Identifier -> TokenType.IDENT
        is Token.IntLiteral -> TokenType.INT
        is Token.CharLiteral -> TokenType.CHAR_LIT
        is Token.StringLiteral -> TokenType.STRING_LIT
        is Token.Fun -> TokenType.FUN
        is Token.Void -> TokenType.VOID
        is Token.I32 -> TokenType.I32
        is Token.U8 -> TokenType.U8
        is Token.F64 -> TokenType.F64
        is Token.Alien -> TokenType.ALIEN
        is Token.Object -> TokenType.OBJECT
        is Token.TypeAlias -> TokenType.TYPEALIAS
        is Token.If -> TokenType.IF
        is Token.Else -> TokenType.ELSE
        is Token.For -> TokenType.FOR
        is Token.While -> TokenType.WHILE
        is Token.Do -> TokenType.DO
        is Token.Switch -> TokenType.SWITCH
        is Token.With -> TokenType.WITH
        is Token.Skip -> TokenType.SKIP
        is Token.Stop -> TokenType.STOP
        is Token.PlusPlus -> TokenType.PLUSPLUS
        is Token.MinusMinus -> TokenType.MINUSMINUS
        is Token.EqualEqual -> TokenType.EQEQ
        is Token.BangEqual -> TokenType.BANGEQ
        is Token.Less -> TokenType.LT
        is Token.LessEqual -> TokenType.LTE
        is Token.Greater -> TokenType.GT
        is Token.GreaterEqual -> TokenType.GTE
        is Token.ShiftLeft -> TokenType.SHL
        is Token.ShiftRight -> TokenType.SHR
        is Token.AmpAmp -> TokenType.ANDAND
        is Token.PipePipe -> TokenType.OROR
        is Token.Arrow -> TokenType.ARROW
        is Token.TildeGreater -> TokenType.TILDE_GT
        is Token.EOF -> TokenType.EOF
    }

/** Lexer error with a precise position. */
class LexError(message: String, position: Position) :
    RuntimeException("$message at ${position.line}:${position.column}")
