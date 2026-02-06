package wasm

import llvm.IRBasicBlock
import llvm.IRValue

/**
 * Intermediate representation for the structured control flow.
 */
sealed class StructuredCFG {
    /** A sequence of structured control flow nodes */
    data class Sequence(val nodes: List<StructuredCFG>) : StructuredCFG()

    /** Emit a basic block's instructions (non-terminator) */
    data class Block(val block: IRBasicBlock) : StructuredCFG()

    /** Return instruction */
    data class Return(val value: IRValue?) : StructuredCFG()

    /** Unconditional branch to a label */
    data class Branch(val depth: Int) : StructuredCFG()

    /** Conditional branch */
    data class BranchIf(val condition: IRValue, val depth: Int) : StructuredCFG()

    /** WebAssembly BLOCK construct */
    data class WasmBlock(val body: StructuredCFG) : StructuredCFG()

    /** WebAssembly LOOP construct */
    data class WasmLoop(val body: StructuredCFG) : StructuredCFG()

    /** WebAssembly IF construct (no else) */
    data class WasmIf(
        val condition: IRValue,
        val inverted: Boolean,
        val thenBody: StructuredCFG
    ) : StructuredCFG()

    /** WebAssembly IF/ELSE construct */
    data class WasmIfElse(
        val condition: IRValue,
        val thenBody: StructuredCFG,
        val elseBody: StructuredCFG
    ) : StructuredCFG()

    companion object {
        fun sequence(vararg nodes: StructuredCFG): StructuredCFG = when {
            nodes.isEmpty() -> Sequence(emptyList())
            nodes.size == 1 -> nodes[0]
            else -> Sequence(nodes.toList())
        }

        fun sequence(nodes: List<StructuredCFG>): StructuredCFG = when {
            nodes.isEmpty() -> Sequence(emptyList())
            nodes.size == 1 -> nodes[0]
            else -> Sequence(nodes)
        }
    }
}
