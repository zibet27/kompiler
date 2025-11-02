package parser.generator

import lexer.TokenType

// Represents an LR(1) item: [ A -> α . β, t ]
data class Item(
    val rule: GrammarRule,
    val dotPosition: Int,
    val terminalSymbol: TokenType
) {
    val symbolAfterDot: Symbol?
        get() = if (dotPosition < rule.rhs.size) rule.rhs[dotPosition] else null

    fun advance(): Item = Item(rule, dotPosition + 1, terminalSymbol)

    override fun toString(): String {
        val before = rule.rhs.take(dotPosition).joinToString(" ")
        val after = rule.rhs.drop(dotPosition).joinToString(" ")
        return "[ ${rule.lhs.name} → $before • $after , $terminalSymbol]"
    }
}

// Represents the "core" of an item (rule + dot position, without a lookahead)
data class ItemCore(val rule: GrammarRule, val dotPosition: Int)

val Item.core: ItemCore get() = ItemCore(rule, dotPosition)

// Represents a state (set of items)
typealias State = Set<Item>

typealias StateCollection = List<State>