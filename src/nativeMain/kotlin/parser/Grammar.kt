package parser

import lexer.TokenType

// Symbols in grammar: terminals are TokenType; non-terminals are NonTerminal.
sealed class Symbol

data class Terminal(val t: TokenType) : Symbol() {
    override fun toString(): String = t.name
}

data class NonTerminal(val name: String) : Symbol() {
    override fun toString(): String = name
}

// Represents a grammar rule: A → B C D
data class GrammarRule(val lhs: NonTerminal, val rhs: List<Symbol>) {
    val isAccepting: Boolean get() = lhs.name.endsWith("'")

    override fun toString(): String = "${lhs.name} → ${rhs.joinToString(" ")}"
}

class Grammar(
    rules: List<GrammarRule>,
    startSymbol: NonTerminal
) {
    val augmentedRules: List<GrammarRule> = buildList {
        add(GrammarRule(NonTerminal(name = "$startSymbol'"), listOf(startSymbol)))
        addAll(rules)
    }

    val augmentedStartRule: GrammarRule get() = augmentedRules.first()

    val nonTerminals: Set<NonTerminal> = augmentedRules.map { it.lhs }.toSet()

    val terminals: Set<TokenType> = buildSet {
        for (rule in augmentedRules) {
            for (symbol in rule.rhs) {
                if (symbol is Terminal) add(symbol.t)
            }
        }
        add(TokenType.EOF)
    }

    fun getRulesFor(symbol: NonTerminal): List<GrammarRule> {
        return augmentedRules.filter { it.lhs == symbol }
    }
}