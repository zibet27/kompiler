package parser.generator

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import lexer.TokenType

// Symbols in grammar: terminals are TokenType; non-terminals are NonTerminal.
@Serializable
sealed class Symbol

@Serializable
data class Terminal(val t: TokenType) : Symbol() {
    override fun toString(): String = t.name
}

@Serializable(with = NonTerminalSerializer::class)
data class NonTerminal(val name: String) : Symbol()

val String.nt get() = NonTerminal(name = this)
val TokenType.t get() = Terminal(t = this)

// Represents a grammar rule: A → B C D
@Serializable
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

    val augmentedStartRule: GrammarRule
        get() = augmentedRules.first()

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

    fun hash(): String {
        return listOf(
            terminals.joinToString(separator = ","),
            nonTerminals.joinToString(separator = ","),
            augmentedRules.size
        ).joinToString(
            separator = ";",
            prefix = "[",
            postfix = "]"
        )
    }
}

object NonTerminalSerializer : KSerializer<NonTerminal> {
    override fun deserialize(decoder: Decoder): NonTerminal {
        return decoder.decodeString().nt
    }

    override fun serialize(encoder: Encoder, value: NonTerminal) {
        encoder.encodeString(value.name)
    }

    override val descriptor: SerialDescriptor = PrimitiveSerialDescriptor("NonTerminal", PrimitiveKind.STRING)
}
