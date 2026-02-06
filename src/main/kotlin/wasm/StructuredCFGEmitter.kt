package wasm

/**
 * Emits WebAssembly bytecode from StructuredCFG.
 */
class StructuredCFGEmitter(
    private val emitter: WasmEmitter,
    private val instructionEmitter: InstructionEmitter,
    private val phiResolver: PhiResolver
) {
    fun emit(cfg: StructuredCFG) {
        when (cfg) {
            is StructuredCFG.Sequence -> cfg.nodes.forEach { emit(it) }
            is StructuredCFG.Block -> emitBlock(cfg)
            is StructuredCFG.Return -> emitReturn(cfg)
            is StructuredCFG.Branch -> emitBranch(cfg)
            is StructuredCFG.BranchIf -> emitBranchIf(cfg)
            is StructuredCFG.WasmBlock -> emitWasmBlock(cfg)
            is StructuredCFG.WasmLoop -> emitWasmLoop(cfg)
            is StructuredCFG.WasmIf -> emitWasmIf(cfg)
            is StructuredCFG.WasmIfElse -> emitWasmIfElse(cfg)
        }
    }

    private fun emitBlock(cfg: StructuredCFG.Block) {
        val block = cfg.block
        for (inst in block.instructions) {
            if (!inst.isTerminator()) {
                instructionEmitter.emitInstruction(inst)
            }
        }
        phiResolver.getPhiCopies(block).forEach { instructionEmitter.emitPhiCopy(it) }
    }

    private fun emitReturn(cfg: StructuredCFG.Return) {
        cfg.value?.let { instructionEmitter.emitValue(it) }
        emitter.writeByte(WasmOp.RETURN)
    }

    private fun emitBranch(cfg: StructuredCFG.Branch) {
        emitter.writeByte(WasmOp.BR)
        emitter.writeU32Leb(cfg.depth)
    }

    private fun emitBranchIf(cfg: StructuredCFG.BranchIf) {
        instructionEmitter.emitValue(cfg.condition)
        emitter.writeByte(WasmOp.BR_IF)
        emitter.writeU32Leb(cfg.depth)
    }

    private fun emitWasmBlock(cfg: StructuredCFG.WasmBlock) {
        emitter.writeByte(WasmOp.BLOCK)
        emitter.writeByte(0x40) // void
        emit(cfg.body)
        emitter.writeByte(WasmOp.END)
    }

    private fun emitWasmLoop(cfg: StructuredCFG.WasmLoop) {
        emitter.writeByte(WasmOp.LOOP)
        emitter.writeByte(0x40) // void
        emit(cfg.body)
        emitter.writeByte(WasmOp.END)
    }

    private fun emitWasmIf(cfg: StructuredCFG.WasmIf) {
        instructionEmitter.emitValue(cfg.condition)
        if (cfg.inverted) {
            emitter.writeByte(WasmOp.I32_EQZ)
        }
        emitter.writeByte(WasmOp.IF)
        emitter.writeByte(0x40) // void
        emit(cfg.thenBody)
        emitter.writeByte(WasmOp.END)
    }

    private fun emitWasmIfElse(cfg: StructuredCFG.WasmIfElse) {
        instructionEmitter.emitValue(cfg.condition)
        emitter.writeByte(WasmOp.IF)
        emitter.writeByte(0x40) // void
        emit(cfg.thenBody)
        emitter.writeByte(WasmOp.ELSE)
        emit(cfg.elseBody)
        emitter.writeByte(WasmOp.END)
    }

    private fun llvm.IRInstruction.isTerminator(): Boolean = when (this) {
        is llvm.IRInstruction.Ret,
        is llvm.IRInstruction.Br,
        is llvm.IRInstruction.CondBr -> true
        else -> false
    }
}
