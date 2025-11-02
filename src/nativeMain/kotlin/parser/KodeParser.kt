package parser

import lexer.LexerFactory
import lexer.LexerImpl
import parser.ast.AstActions
import parser.ast.AstValue
import parser.ast.Program
import parser.generator.LALRParserGenerator
import parser.generator.ParsingTable
import parser.generator.ParsingTableCache

class KodeParser(
    private val lexerFactory: LexerFactory = LexerImpl.Companion
) {
    private val treeParser = Parser(parsingTable, AstActions)

    /**
     * Tokenize [source] with lexer created by [lexerFactory] and run the LALR(1) parser, returning an AST.
     */
    fun parse(source: String): Program {
        val lexer = lexerFactory.forSource(source)
        val result = treeParser.parse(tokens = lexer.tokenize())
        return (result as AstValue.Pgm).program
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
