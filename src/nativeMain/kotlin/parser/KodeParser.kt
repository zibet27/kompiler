package parser

import lexer.LexerFactory
import lexer.LexerImpl
import parser.generator.LALRParserGenerator
import parser.generator.ParsingTable
import parser.generator.ParsingTableCache

class KodeParser(
    private val lexerFactory: LexerFactory = LexerImpl.Companion
) {
    private val parser = Parser(parsingTable)

    /**
     * Tokenize [source] with lexer created by [lexerFactory] and run the LALR(1) parser, returning a parse tree.
     */
    fun parse(source: String): ParseNode {
        val lexer = lexerFactory.forSource(source)
        return parser.parse(tokens = lexer.tokenize())
    }

    companion object {
        private val cache = ParsingTableCache(
            cacheFile = "cache/kode-parsing-table.json",
            cacheLockFile = "cache/kode-parsing-table.lock"
        )
        val parsingTable: ParsingTable by lazy {
            val grammar = KodeGrammar.build()
            val grammarHash = grammar.hash()

            cache.load(grammarHash)?.let { return@lazy it }

            // Generate if not cached
            val generator = LALRParserGenerator(grammar)
            val table = generator.parsingTable

            // Save for next time
            runCatching {
                cache.save(table, grammarHash)
            }.onFailure { println("Warning: Could not save parsing table: ${it.message}") }

            table
        }
    }
}
