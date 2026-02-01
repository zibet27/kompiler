package parser

import lexer.Token
import lexer.type
import lexer.unexpected
import parser.generator.Action
import parser.generator.GrammarRule
import parser.generator.ParsingTable

interface SemanticActions<V> {
    fun onShift(token: Token): V
    fun onReduce(rule: GrammarRule, children: List<V>): V
}

/**
 * Type-aware LR(1) parser that tracks declared type names and converts IDENT tokens to TYPENAME
 * when they refer to types (object declarations and typealias declarations).
 */
abstract class TypeAwareParser<V>(
    private val table: ParsingTable,
    private val actions: SemanticActions<V>
) {
    protected val typeNames = mutableSetOf<String>()

    fun parse(tokens: Sequence<Token>): V {
        val stateStack = ArrayDeque<Int>()
        val valueStack = ArrayDeque<V>()
        stateStack.addLast(0)

        val tokenIterator = tokens.iterator()
        var currentToken = transformToken(tokenIterator.next())

        while (true) {
            val currentState = stateStack.last()
            when (val action = table.actionTable[currentState][currentToken.type]) {
                is Action.Shift -> {
                    stateStack.addLast(action.state)
                    valueStack.addLast(actions.onShift(currentToken))
                    currentToken = transformToken(tokenIterator.next())
                }

                is Action.Reduce -> {
                    val numToPop = action.rule.rhs.size
                    val children = (1..numToPop).map { valueStack.removeLast() }.asReversed()
                    repeat(numToPop) { stateStack.removeLast() }

                    // Track type names from reductions
                    trackTypeName(action.rule, children)

                    val nextStateForGoto = stateStack.last()
                    val nextState = table.gotoTable[nextStateForGoto][action.rule.lhs]
                        ?: error("No GOTO entry from state $nextStateForGoto with ${action.rule.lhs}")

                    val v = actions.onReduce(action.rule, children)

                    stateStack.addLast(nextState)
                    valueStack.addLast(v)
                }

                is Action.Accept -> return valueStack.single()
                null -> currentToken.unexpected()
            }
        }
    }

    abstract fun transformToken(t: Token): Token

    abstract fun trackTypeName(rule: GrammarRule, children: List<V>)
}