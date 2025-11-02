package parser

import lexer.LexerFactory
import lexer.LexerImpl

class KodeParser(
    private val lexerFactory: LexerFactory = LexerImpl.Companion
) {
    private val parser = run {
        val grammar = KodeGrammar.build()
        val parsingTable = LALRParserGenerator(grammar).parsingTable
        //Parser(parsingTable)
    }

    /**
     * Tokenize [source] with lexer created by [lexerFactory] and run the LR parser, returning a parse tree.
     */
    fun parse(source: String): ParseNode? {
        val lexer = lexerFactory.forSource(source)
        //return parser.parse(tokens = lexer.tokenize())
        return null
    }
}
