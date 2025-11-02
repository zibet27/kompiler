package parser.generator

class GrammarBuilder {
    internal val rules = mutableListOf<GrammarRule>()

    fun rule(lhs: NonTerminal, vararg rhs: Symbol) {
        rules += GrammarRule(lhs = lhs, rhs = rhs.toList())
    }
}

fun grammar(startSymbol: NonTerminal, block: GrammarBuilder.() -> Unit): Grammar {
    return Grammar(GrammarBuilder().apply(block).rules, startSymbol)
}
