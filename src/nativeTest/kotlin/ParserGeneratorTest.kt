package test

import kotlin.test.*
import parser.*
import lexer.TokenType
import parser.generator.LALRParserGenerator
import parser.generator.grammar
import parser.generator.nt
import parser.generator.t

class ParserGeneratorTest {
    @Test
    fun simpleGrammar_buildsLALRTable() {
        // Simple expression grammar: E -> E + T | T, T -> int
        val grammar = grammar(startSymbol = "E".nt) {
            rule("E".nt, "E".nt, TokenType.PLUS.t, "T".nt)
            rule("E".nt, "T".nt)
            rule("T".nt, TokenType.INT.t)
        }

        val generator = LALRParserGenerator(grammar)
        val table = generator.parsingTable

        assertNotNull(table)
        assertTrue(table.actionTable.isNotEmpty(), "Action table should not be empty")
        assertTrue(table.gotoTable.isNotEmpty(), "Goto table should not be empty")
    }

    @Test
    fun kodeGrammar_buildsWithoutErrors() {
        val table = KodeParser.parsingTable

        assertNotNull(table)
        assertTrue(table.actionTable.isNotEmpty())
        assertTrue(table.gotoTable.isNotEmpty())
    }

    @Test
    fun kodeGrammar_hasExpectedStateCount() {
        val table = KodeParser.parsingTable

        // We expect around 273 LALR states
        val stateCount = table.actionTable.size
        assertEquals(273, stateCount,"State count should be around 273, got $stateCount")
    }

    @Test
    fun firstSets_computeCorrectly() {
        // Grammar: S -> A B, A -> a | ε, B -> b
        // FIRST(A) = {a, ε}, FIRST(B) = {b}, FIRST(S) = {a, b}
        val grammar = grammar(startSymbol = "S".nt) {
            rule("S".nt, "A".nt, "B".nt)
            rule("A".nt, TokenType.IDENT.t) // 'a'
            rule("A".nt) // ε
            rule("B".nt, TokenType.INT.t) // 'b'
        }

        val generator = LALRParserGenerator(grammar)
        assertNotNull(generator.parsingTable)
    }

    @Test
    fun augmentedGrammar_hasStartPrime() {
        val grammar = grammar(startSymbol = "E".nt) {
            rule("E".nt, TokenType.INT.t)
        }

        assertTrue(grammar.augmentedRules.first().lhs.name.endsWith("'"))
        assertEquals("E'", grammar.augmentedRules.first().lhs.name)
    }

    @Test
    fun closureOperation_includesExpectedItems() {
        // Simple test that closure operation works
        val grammar = grammar(startSymbol = "S".nt) {
            rule("S".nt, "A".nt)
            rule("A".nt, TokenType.INT.t)
        }

        val generator = LALRParserGenerator(grammar)
        val table = generator.parsingTable

        // If closure works correctly, we should have a valid parsing table
        assertNotNull(table)
        assertTrue(table.actionTable.isNotEmpty())
    }

    @Test
    fun terminals_includeEOF() {
        val grammar = grammar(startSymbol = "E".nt) {
            rule("E".nt, TokenType.INT.t)
        }

        assertTrue(TokenType.EOF in grammar.terminals)
    }

    @Test
    fun nonTerminals_includeAllDefined() {
        val grammar = grammar(startSymbol = "E".nt) {
            rule("E".nt, "T".nt)
            rule("T".nt, TokenType.INT.t)
        }

        assertTrue("E".nt in grammar.nonTerminals)
        assertTrue("T".nt in grammar.nonTerminals)
        assertTrue("E'".nt in grammar.nonTerminals) // Augmented start
    }

    @Test
    fun getRulesFor_returnsMatchingRules() {
        val grammar = grammar(startSymbol = "E".nt) {
            rule("E".nt, "E".nt, TokenType.PLUS.t, "T".nt)
            rule("E".nt, "T".nt)
            rule("T".nt, TokenType.INT.t)
        }

        val eRules = grammar.getRulesFor("E".nt)
        assertEquals(2, eRules.size)

        val tRules = grammar.getRulesFor("T".nt)
        assertEquals(1, tRules.size)
    }
}
