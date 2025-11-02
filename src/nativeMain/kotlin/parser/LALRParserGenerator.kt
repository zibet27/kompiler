package parser

import lexer.TokenType
import kotlin.collections.set

sealed class Action {
    data class Shift(val state: Int) : Action()
    data class Reduce(val rule: GrammarRule) : Action()
    data object Accept : Action()
}

data class ParsingTable(
    val actionTable: List<Map<TokenType, Action>>,
    val gotoTable: List<Map<NonTerminal, Int>>,
    val grammar: Grammar
)

typealias StateCollection = List<State>

class LALRParserGenerator(private val grammar: Grammar) {
    private val firstSets: Map<Symbol, Set<TokenType?>> = buildMap {
        // Initialize with terminals
        grammar.terminals.forEach { t -> set(Terminal(t), setOf(t)) }
        grammar.nonTerminals.forEach { nt -> set(nt, mutableSetOf()) }
    }

    init {
        var changed = true
        while (changed) {
            changed = false
            for (rule in grammar.augmentedRules) {
                val lhsFirst = firstSets[rule.lhs] as MutableSet
                val oldSize = lhsFirst.size
                val rhsFirst = computeFirstOfSequence(symbols = rule.rhs)
                lhsFirst.addAll(rhsFirst)
                if (lhsFirst.size > oldSize) {
                    changed = true
                }
            }
        }
        println("DONE")
    }

    val parsingTable: ParsingTable by lazy {
        println("Building parsing table...")
        val (lr1States, lr1Transitions) = buildLR1CanonicalCollection()
        println("Finished building parsing table")
        val (lalrStates, lr1ToLalrMap) = lr1States.mergeToLALR()
        println("Finished merging LR(1) states ${lr1States.size} -> ${lalrStates.size}")
        buildLALRTables(lr1States, lalrStates, lr1Transitions, lr1ToLalrMap)
    }

    private fun computeFirstOfSequence(symbols: List<Symbol>): Set<TokenType?> = buildSet {
        for (symbol in symbols) {
            val symbolFirst = firstSets[symbol]!!
            for (symbolAfterDot in symbolFirst) {
                symbolAfterDot?.let { add(it) } // Add all non-epsilon terminals
            }
            if (null !in symbolFirst) { // If the symbol cannot be epsilon
                return@buildSet
            }
        }
        add(null) // Add epsilon
    }

    private fun State.closure(): State {
        val closureSet = this.toMutableSet()
        val worklist = this.toMutableList()

        while (worklist.isNotEmpty()) {
            val item = worklist.removeFirst()
            val symbolAfterDot = item.symbolAfterDot

            val rhs = item.rule.rhs
            if (symbolAfterDot is NonTerminal) {
                val beta = rhs.subList(item.dotPosition + 1, rhs.size)
                // Lookahead is FIRST of the rest of the rule (β) plus the original lookahead (a)
                val lookaheads = computeFirstOfSequence(beta + Terminal(item.terminalSymbol))

                for (rule in grammar.getRulesFor(symbolAfterDot)) {
                    for (terminal in lookaheads) {
                        // If the FIRST set contained epsilon, use the original lookahead
                        val newLookahead = terminal ?: item.terminalSymbol
                        val newItem = Item(rule, 0, newLookahead)
                        if (closureSet.add(newItem)) {
                            worklist.add(newItem)
                        }
                    }
                }
            }
        }
        return closureSet
    }

    private fun State.goTo(symbol: Symbol): State {
        val movedItems = filter { it.symbolAfterDot == symbol }.map { it.advance() }
        return movedItems.toSet().closure()
    }

    private fun buildLR1CanonicalCollection(): Pair<List<State>, Map<Pair<Int, Symbol>, Int>> {
        val initialItem = Item(grammar.augmentedStartRule, dotPosition = 0, terminalSymbol = TokenType.EOF)
        val initialState = setOf(initialItem).closure()

        val states = mutableListOf(initialState)
        val transitions = mutableMapOf<Pair<Int, Symbol>, Int>()
        val worklist = mutableListOf(0) // indices of states to process

        while (worklist.isNotEmpty()) {
            val currentIndex = worklist.removeFirst()
            val currentState = states[currentIndex]

            val symbolsToTransitionOn = currentState.mapNotNull { it.symbolAfterDot }.toSet()

            for (symbol in symbolsToTransitionOn) {
                val nextState = currentState.goTo(symbol)
                if (nextState.isEmpty()) continue

                val existingStateIndex = states.indexOf(nextState)
                val nextStateIndex = if (existingStateIndex != -1) {
                    existingStateIndex
                } else {
                    states.add(nextState)
                    worklist.add(states.size - 1)
                    states.size - 1
                }
                transitions[currentIndex to symbol] = nextStateIndex
            }
        }
        return Pair(states, transitions)
    }

    private fun StateCollection.mergeToLALR(): Pair<StateCollection, Map<Int, Int>> {
        val coreToStateGroup = indices.groupBy {
            this[it].map { item -> item.core }.toSet()
        }

        val lalrStates = mutableListOf<State>()
        val lr1ToLalrMap = mutableMapOf<Int, Int>()

        for (group in coreToStateGroup.values) {
            val newLalrStateIndex = lalrStates.size
            // Merge all items from the LR(1) states in this group
            val mergedState = group.flatMap { stateIndex -> this[stateIndex] }.toSet()
            lalrStates.add(mergedState)
            // Map all old LR(1) state indices to the new LALR(1) state index
            for (oldIndex in group) {
                lr1ToLalrMap[oldIndex] = newLalrStateIndex
            }
        }

        return Pair(lalrStates, lr1ToLalrMap)
    }

    private fun buildLALRTables(
        lr1States: StateCollection,
        lalrStates: StateCollection,
        lr1Transitions: Map<Pair<Int, Symbol>, Int>,
        lr1ToLalrMap: Map<Int, Int>
    ): ParsingTable {
        val numStates = lalrStates.size
        val actionTable = Array(numStates) { mutableMapOf<TokenType, Action>() }
        val gotoTable = Array(numStates) { mutableMapOf<NonTerminal, Int>() }

        for (lr1StateIndex in lr1States.indices) {
            val lalrStateIndex = lr1ToLalrMap[lr1StateIndex]!!
            val stateItems = lr1States[lr1StateIndex]

            // Handle Reduces and Accepts actions
            for (item in stateItems) {
                if (item.symbolAfterDot == null) { // This is a reduction item: [A -> α ., t]
                    val action = when {
                        item.rule.isAccepting -> Action.Accept
                        else -> Action.Reduce(item.rule)
                    }

                    val existingAction = actionTable[lalrStateIndex][item.terminalSymbol]
                    require(existingAction == null || existingAction == action) {
                        buildString {
                            appendLine("!! CONFLICT in LALR state $lalrStateIndex on token ${item.terminalSymbol} !!")
                            appendLine("   Existing: $existingAction")
                            appendLine("   New:      $action")
                            appendLine("   (Caused by merging LR(1) states)")
                        }
                    }
                    actionTable[lalrStateIndex][item.terminalSymbol] = action
                }
            }

            // Handle Shift and Goto actions using original LR(1) transitions
            val transitionsFromThisState = lr1Transitions.filter { it.key.first == lr1StateIndex }
            for ((key, targetLr1State) in transitionsFromThisState) {
                val (_, symbol) = key
                val targetLalrState = lr1ToLalrMap[targetLr1State]!!

                when (symbol) {
                    is Terminal -> {
                        val action = Action.Shift(targetLalrState)
                        val existingAction = actionTable[lalrStateIndex][symbol.t]
                        require(existingAction == null || existingAction == action) {
                            buildString {
                                appendLine("!! SHIFT/REDUCE CONFLICT in LALR state $lalrStateIndex on token ${symbol.t} !!")
                                appendLine("   Existing: $existingAction")
                                appendLine("   New:      $action")
                            }
                        }
                        actionTable[lalrStateIndex][symbol.t] = action
                    }

                    is NonTerminal -> {
                        gotoTable[lalrStateIndex][symbol] = targetLalrState
                    }
                }
            }
        }

        return ParsingTable(
            actionTable.map { it },
            gotoTable.map { it },
            grammar
        )
    }
}