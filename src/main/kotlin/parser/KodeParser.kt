package parser

import lexer.LexerFactory
import lexer.LexerImpl
import lexer.Token
import parser.ast.AstActions
import ast.AstValue
import ast.Program
import parser.generator.GrammarRule
import parser.generator.LALRParserGenerator
import parser.generator.ParsingTable
import parser.generator.ParsingTableCache

class KodeParser(
    private val lexerFactory: LexerFactory = LexerImpl.Companion
) {
    private val treeParser = object : TypeAwareParser<AstValue>(parsingTable, AstActions) {
        override fun transformToken(t: Token): Token {
            return if (t is Token.Identifier && typeNames.contains(t.lexeme)) {
                Token.TypeName(t.span, t.lexeme)
            } else {
                t
            }
        }

        override fun trackTypeName(
            rule: GrammarRule, children: List<AstValue>
        ) {
            // Track ObjectHeader: object IDENT | object TYPENAME
            if (rule.lhs == KodeGrammar.NT.ObjectHeader && children.size == 2) {
                val secondChild = children[1]
                if (secondChild is AstValue.T) {
                    when (val token = secondChild.token) {
                        is Token.Identifier -> typeNames.add(token.lexeme)
                        is Token.TypeName -> typeNames.add(token.lexeme)
                        else -> {}
                    }
                }
            }
            // Track TypeAlias: typealias IDENT = Type ;
            if (rule.lhs == KodeGrammar.NT.TypeAlias && children.size >= 2) {
                val secondChild = children[1]
                if (secondChild is AstValue.T) {
                    val token = secondChild.token
                    if (token is Token.Identifier) {
                        typeNames.add(token.lexeme)
                    }
                }
            }
        }
    }

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
            cache.save(table, grammarHash)

            table
        }
    }
}
