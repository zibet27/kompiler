package parser

import lexer.TokenType

class GrammarBuilder {
    internal val rules = mutableListOf<GrammarRule>()

    fun rule(lhs: NonTerminal, vararg rhs: Symbol) {
        rules += GrammarRule(lhs = lhs, rhs = rhs.toList())
    }
}

fun grammar(startSymbol: NonTerminal, block: GrammarBuilder.() -> Unit): Grammar {
    return Grammar(GrammarBuilder().apply(block).rules, startSymbol)
}

val String.nt get() = NonTerminal(name = this)
val TokenType.t get() = Terminal(t = this)
