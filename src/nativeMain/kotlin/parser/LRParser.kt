package parser

import lexer.LexerFactory
import lexer.LexerImpl
import lexer.Token
import lexer.type

// A node in the parse tree (for demonstration)
sealed class ParseNode {
    data class Inner(val symbol: NonTerminal, val children: List<ParseNode>) : ParseNode()
    data class Leaf(val token: Token) : ParseNode()
}

class Parser(
    private val table: ParsingTable
) {

    fun parse(tokens: Sequence<Token>): ParseNode {
        val stateStack = ArrayDeque<Int>()
        val nodeStack = ArrayDeque<ParseNode>()
        stateStack.addLast(0)

        val tokenIterator = (tokens).iterator()
        var currentToken = tokenIterator.next()

        while (true) {
            val currentState = stateStack.last()
            when (val action = table.actionTable[currentState][currentToken.type]) {
                is Action.Shift -> {
                    stateStack.addLast(action.state)
                    nodeStack.addLast(ParseNode.Leaf(currentToken))
                    currentToken = tokenIterator.next()
                }
                is Action.Reduce -> {
                    val numToPop = action.rule.rhs.size
                    val children = (1..numToPop).map { nodeStack.removeLast() }.reversed()
                    repeat(numToPop) { stateStack.removeLast() }

                    val nextStateForGoto = stateStack.last()
                    val nextState = table.gotoTable[nextStateForGoto][action.rule.lhs]
                        ?: error("No GOTO entry from state $nextStateForGoto with ${action.rule.lhs}")

                    stateStack.addLast(nextState)
                    nodeStack.addLast(ParseNode.Inner(action.rule.lhs, children))
                    println("GOTO from state $nextStateForGoto with ${action.rule.lhs} to state $nextState")
                }
                is Action.Accept -> return nodeStack.single()
                null -> error("Syntax Error: Unexpected token $currentToken in state $currentState.")
            }
        }
    }
}