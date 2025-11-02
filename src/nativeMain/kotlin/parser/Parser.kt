package parser

import lexer.Token
import lexer.type
import parser.generator.Action
import parser.generator.GrammarRule
import parser.generator.ParsingTable

interface SemanticActions<V> {
    fun onShift(token: Token): V
    fun onReduce(rule: GrammarRule, children: List<V>): V
}

// LR(1) parser
class Parser<V>(
    private val table: ParsingTable,
    private val actions: SemanticActions<V>
) {

    /**
     * Generic LR parse that constructs a semantic value directly via user-provided actions, without
     * building an intermediate parse tree. The returned value is whatever [actions] produce for the start symbol.
     */
    fun parse(tokens: Sequence<Token>): V {
        val stateStack = ArrayDeque<Int>()
        val valueStack = ArrayDeque<V>()
        stateStack.addLast(0)

        val tokenIterator = tokens.iterator()
        var currentToken = tokenIterator.next()

        while (true) {
            val currentState = stateStack.last()
            when (val action = table.actionTable[currentState][currentToken.type]) {
                is Action.Shift -> {
                    stateStack.addLast(action.state)
                    valueStack.addLast(actions.onShift(currentToken))
                    currentToken = tokenIterator.next()
                }

                is Action.Reduce -> {
                    val numToPop = action.rule.rhs.size
                    val children = (1..numToPop).map { valueStack.removeLast() }.asReversed()
                    repeat(numToPop) { stateStack.removeLast() }

                    val nextStateForGoto = stateStack.last()
                    val nextState = table.gotoTable[nextStateForGoto][action.rule.lhs]
                        ?: error("No GOTO entry from state $nextStateForGoto with ${action.rule.lhs}")

                    val v = actions.onReduce(action.rule, children)

                    stateStack.addLast(nextState)
                    valueStack.addLast(v)
                }

                is Action.Accept -> return valueStack.single()
                null -> error("Syntax Error: Unexpected token $currentToken.")
            }
        }
    }
}