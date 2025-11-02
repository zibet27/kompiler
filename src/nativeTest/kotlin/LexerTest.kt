package test

import kotlin.test.*
import lexer.*

class LexerTest {
    @Test
    fun simpleProgram_tokensAreProducedInOrder() {
        val program = """
            fun add{ a: i32, b: i32 }: i32 (
                a + b;
            );
        """.trimIndent()

        val tokens = LexerImpl(program).tokenize()
        // Compare simple class names for clarity and easy inference
        val kinds = tokens.map { it::class.simpleName }.toList()

        val expected = listOf(
            "Fun",
            "Identifier", // add
            "LBrace",
            "Identifier", // a
            "Colon",
            "I32",
            "Comma",
            "Identifier", // b
            "Colon",
            "I32",
            "RBrace",
            "Colon",
            "I32",
            "LParen",
            "Identifier", // a
            "Plus",
            "Identifier", // b
            "Semicolon",
            "RParen",
            "Semicolon",
            "EOF"
        )

        assertEquals(expected, kinds, "Token class sequence should match")
    }

    @Test
    fun whitespaceAndCommentsAreSkipped() {
        val program = """
            // leading comment
            fun main{}: void ( // inline
                // inside
            ); // end
        """.trimIndent()

        val tokens = LexerImpl(program).tokenize()
        val kinds = tokens.map { it::class.simpleName }.toList()
        val expected = listOf(
            "Fun",
            "Identifier", // main
            "LBrace",
            "RBrace",
            "Colon",
            "Void",
            "LParen",
            "RParen",
            "Semicolon",
            "EOF"
        )
        assertEquals(expected, kinds)
    }

    @Test
    fun intLiteral_and_spans() {
        val program = """
            fun x{}: void (
                123;
            );
        """.trimIndent()
        val tokens = LexerImpl(program).tokenize()
        val intTok = tokens.first { it is Token.IntLiteral }
        assertEquals("123", intTok.lexeme)
        // The literal should be on line 2, columns start..end depending on indent; check line only for stability
        assertEquals(2, intTok.span.start.line)
        assertEquals(2, intTok.span.end.line)
        assertTrue(intTok.span.end.column > intTok.span.start.column)
    }

    @Test
    fun unexpectedCharacter_reportsPosition() {
        val program = "fun bad{ @ };"
        val ex = assertFailsWith<LexError> { LexerImpl(program).tokenize().toList() }
        assertTrue(ex.message!!.contains("Unexpected character"))
        // Should contain the line:column
        assertTrue(Regex("\\d+:\\d+").containsMatchIn(ex.message!!))
    }
}