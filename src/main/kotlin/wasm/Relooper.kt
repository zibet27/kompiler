package wasm

import llvm.*

/**
 * Production Relooper - Converts arbitrary CFG to structured WebAssembly control flow.
 *
 * Algorithm:
 * 1. Detect natural loops using back-edges from DFS traversal
 * 2. Compute loop bodies (all blocks that can reach back-edge and are dominated by header)
 * 3. Emit loops with BLOCK (for break) + LOOP (for continue) structure
 * 4. Handle if/else regions with proper merge point detection
 * 5. Track control flow labels for correct br depth calculation
 *
 * Key insight: WebAssembly br targets are:
 * - BLOCK label: branches to END of block (break)
 * - LOOP label: branches to START of loop (continue)
 * - Depth 0 = innermost, depth 1 = next outer, etc.
 */
class Relooper(
    private val function: IRFunction,
    private val emitter: WasmEmitter,
    private val instructionEmitter: InstructionEmitter,
    private val phiResolver: PhiResolver
) {
    // CFG data
    private val successors = mutableMapOf<IRBasicBlock, List<IRBasicBlock>>()
    private val predecessors = mutableMapOf<IRBasicBlock, MutableList<IRBasicBlock>>()

    // Track which blocks we've emitted
    private val emitted = mutableSetOf<IRBasicBlock>()

    // Loop analysis data
    private data class LoopInfo(
        val header: IRBasicBlock,
        val body: Set<IRBasicBlock>,
        val exitTargets: Set<IRBasicBlock> // blocks outside loop that are branch targets from inside
    )

    private val loopInfoMap = mutableMapOf<IRBasicBlock, LoopInfo>()

    // Control flow label stack for br instruction depth calculation
    // Each entry represents a control structure: (targetBlock, isLoop, loopInfo?)
    // - For LOOP: targetBlock=header, isLoop=true
    // - For BLOCK wrapping loop: targetBlock=null, isLoop=false, loopInfo=loop
    // - For BLOCK in if/else: targetBlock=mergeBlock, isLoop=false
    private data class Label(
        val targetBlock: IRBasicBlock?,
        val isLoop: Boolean,
        val loopInfo: LoopInfo? = null
    )
    private val labelStack = mutableListOf<Label>()

    // Track the block that will be emitted next (for fall-through optimization)
    private var nextBlock: IRBasicBlock? = null

    fun reloop() {
        if (function.basicBlocks.isEmpty()) {
            emitter.writeByte(WasmOp.END)
            return
        }

        buildCFG()
        analyzeLoops()

        // Emit from entry block
        emit(function.basicBlocks.first(), null)

        emitter.writeByte(WasmOp.END)
    }

    private fun buildCFG() {
        for (block in function.basicBlocks) {
            val succs = when (val term = block.terminator) {
                is IRInstruction.Br -> listOf(term.dest)
                is IRInstruction.CondBr -> listOf(term.thenBlock, term.elseBlock)
                else -> emptyList()
            }
            successors[block] = succs

            // Build predecessors
            for (succ in succs) {
                predecessors.getOrPut(succ) { mutableListOf() }.add(block)
            }
        }
    }

    /**
     * Analyze loops using DFS to find back-edges.
     * A back-edge is an edge from a block to an ancestor in the DFS tree.
     */
    private fun analyzeLoops() {
        val visited = mutableSetOf<IRBasicBlock>()
        val dfsStack = mutableSetOf<IRBasicBlock>()
        val backEdges = mutableListOf<Pair<IRBasicBlock, IRBasicBlock>>()

        fun dfs(block: IRBasicBlock) {
            visited.add(block)
            dfsStack.add(block)

            for (succ in successors[block] ?: emptyList()) {
                if (succ in dfsStack) {
                    // Back-edge found: block -> succ
                    backEdges.add(block to succ)
                } else if (succ !in visited) {
                    dfs(succ)
                }
            }

            dfsStack.remove(block)
        }

        if (function.basicBlocks.isNotEmpty()) {
            dfs(function.basicBlocks.first())
        }

        // For each back-edge, compute the loop body
        for ((source, header) in backEdges) {
            val body = computeLoopBody(header, source)
            val exitTargets = findLoopExitTargets(body)
            loopInfoMap[header] = LoopInfo(header, body, exitTargets)
        }
    }

    /**
     * Compute loop body: all blocks that can reach the back-edge source
     * and are reachable from the header.
     */
    private fun computeLoopBody(header: IRBasicBlock, backEdgeSource: IRBasicBlock): Set<IRBasicBlock> {
        val body = mutableSetOf<IRBasicBlock>()
        val worklist = mutableListOf(backEdgeSource)
        body.add(header)
        body.add(backEdgeSource)

        while (worklist.isNotEmpty()) {
            val block = worklist.removeAt(worklist.size - 1)
            // Add predecessors
            for (pred in predecessors[block] ?: emptyList()) {
                if (pred !in body && pred != header) {
                    body.add(pred)
                    worklist.add(pred)
                }
            }
        }

        return body
    }

    /**
     * Find all blocks outside the loop that are targeted by branches from inside the loop.
     */
    private fun findLoopExitTargets(body: Set<IRBasicBlock>): Set<IRBasicBlock> {
        val exitTargets = mutableSetOf<IRBasicBlock>()
        for (block in body) {
            for (succ in successors[block] ?: emptyList()) {
                if (succ !in body) {
                    exitTargets.add(succ)
                }
            }
        }
        return exitTargets
    }

    /**
     * Emit a block and all blocks reachable from it.
     * @param currentLoop the innermost loop we're currently inside (null if not in a loop)
     */
    private fun emit(block: IRBasicBlock, currentLoop: LoopInfo?) {
        if (block in emitted) return

        // Check if this block starts a new loop
        val loopInfo = loopInfoMap[block]
        if (loopInfo != null) {
            emitLoop(loopInfo)
            return
        }

        emitted.add(block)

        // Emit block body
        emitBlockInstructions(block)

        // Handle terminator
        when (val term = block.terminator) {
            is IRInstruction.Ret -> {
                if (term.value != null) {
                    instructionEmitter.emitValue(term.value)
                }
                emitter.writeByte(WasmOp.RETURN)
            }
            is IRInstruction.Br -> {
                handleBranch(term.dest, currentLoop)
            }
            is IRInstruction.CondBr -> {
                emitIfElse(term.thenBlock, term.elseBlock, term.condition, currentLoop)
            }
            null -> {}
            else -> error("Unexpected terminator: $term")
        }
    }

    /**
     * Emit a loop with proper BLOCK+LOOP structure.
     */
    private fun emitLoop(loopInfo: LoopInfo) {
        // Emit BLOCK (for break)
        emitter.writeByte(WasmOp.BLOCK)
        emitter.writeByte(0x40) // void
        labelStack.add(Label(targetBlock = null, isLoop = false, loopInfo = loopInfo))

        // Emit LOOP (for continue)
        emitter.writeByte(WasmOp.LOOP)
        emitter.writeByte(0x40) // void
        labelStack.add(Label(targetBlock = loopInfo.header, isLoop = true, loopInfo = loopInfo))

        // Emit all blocks in loop body
        emitLoopBody(loopInfo)

        // End loop structures
        emitter.writeByte(WasmOp.END) // end loop
        labelStack.removeLast()

        emitter.writeByte(WasmOp.END) // end block
        labelStack.removeLast()

        // Emit exit blocks (blocks reachable after the loop)
        for (exitBlock in loopInfo.exitTargets) {
            if (exitBlock !in emitted) {
                emit(exitBlock, null)
            }
        }
    }

    /**
     * Emit all blocks within a loop body.
     * Uses structured emission starting from the header.
     */
    private fun emitLoopBody(loopInfo: LoopInfo) {
        // Start by emitting from the header
        emitInLoop(loopInfo.header, loopInfo)
    }

    /**
     * Emit a block within a loop context.
     * This ensures proper structured control flow within the loop.
     */
    private fun emitInLoop(block: IRBasicBlock, loopInfo: LoopInfo) {
        if (block in emitted) return
        if (block !in loopInfo.body) return

        emitted.add(block)

        // Emit block instructions
        emitBlockInstructions(block)

        // Handle terminator
        when (val term = block.terminator) {
            is IRInstruction.Ret -> {
                if (term.value != null) {
                    instructionEmitter.emitValue(term.value)
                }
                emitter.writeByte(WasmOp.RETURN)
            }
            is IRInstruction.Br -> {
                handleBranchInLoop(term.dest, loopInfo)
            }
            is IRInstruction.CondBr -> {
                emitIfElseInLoopStructured(term.thenBlock, term.elseBlock, term.condition, loopInfo)
            }
            null -> {}
            else -> error("Unexpected terminator: $term")
        }
    }

    /**
     * Handle branch within a loop with proper continue/break detection.
     */
    private fun handleBranchInLoop(dest: IRBasicBlock, loopInfo: LoopInfo) {
        // Check if branching to next block (fall through)
        if (dest == nextBlock) {
            return
        }

        // Check if this is the loop header (continue)
        if (dest == loopInfo.header && dest in emitted) {
            val depth = findBranchDepth(loopInfo.header, isLoopTarget = true)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
            return
        }

        // Check if dest leads directly to the header (it's an increment block)
        val destSuccessors = successors[dest] ?: emptyList()
        if (dest in loopInfo.body && destSuccessors.size == 1 && destSuccessors[0] == loopInfo.header) {
            // This is a continue to increment block
            if (dest !in emitted) {
                // Emit the increment block inline, then it will branch back
                emitInLoop(dest, loopInfo)
            } else {
                // Already emitted somewhere else - we still need to execute increment instructions!
                // Emit the increment block's instructions (without marking as emitted again)
                // then emit continue to header
                emitBlockInstructions(dest)
                val depth = findBranchDepth(loopInfo.header, isLoopTarget = true)
                emitter.writeByte(WasmOp.BR)
                emitter.writeU32Leb(depth)
            }
            return
        }

        // Check if breaking out of loop
        if (dest in loopInfo.exitTargets) {
            val depth = findLoopBreakDepth(loopInfo)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
            return
        }

        // Check if need to break out to a merge point
        val labelIndex = findLabelIndex(dest)
        if (labelIndex != null) {
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(labelIndex)
            return
        }

        // Otherwise, emit the destination block inline
        if (dest in loopInfo.body && dest !in emitted) {
            emitInLoop(dest, loopInfo)
        } else if (dest in loopInfo.body && dest in emitted) {
            // Block is already emitted - check if it leads back to header
            @Suppress("NAME_SHADOWING")
            val destSuccessors = successors[dest] ?: emptyList()
            if (destSuccessors.size == 1 && destSuccessors[0] == loopInfo.header) {
                // This block leads back to header, so it's like a continue
                // Emit the block's instructions and branch back to header
                emitBlockInstructions(dest)
                val depth = findBranchDepth(loopInfo.header, isLoopTarget = true)
                emitter.writeByte(WasmOp.BR)
                emitter.writeU32Leb(depth)
            }
        }
    }

    /**
     * Emit structured if/else within a loop.
     */
    private fun emitIfElseInLoopStructured(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        loopInfo: LoopInfo
    ) {
        if (thenBlock in emitted && elseBlock in emitted) {
            return
        }

        // Check for empty branches
        val thenIsEmpty = isEmptyBlock(thenBlock)
        val elseIsEmpty = isEmptyBlock(elseBlock)
        val mergeBlock = findMergeBlock(thenBlock, elseBlock, loopInfo.body)
        val mergeHasControlFlow = mergeBlock != null && mergeBlock.terminator is IRInstruction.CondBr

        // Simplified IF for empty else branch
        if (elseIsEmpty && elseBlock !in emitted && mergeBlock != null && !mergeHasControlFlow) {
            val elseSucc = successors[elseBlock]?.firstOrNull()
            if (elseSucc == mergeBlock && mergeBlock in loopInfo.body) {
                instructionEmitter.emitValue(condition)
                emitter.writeByte(WasmOp.IF)
                emitter.writeByte(0x40) // void

                // Add IF to label stack (even though it's void, it affects branch depths)
                labelStack.add(Label(targetBlock = null, isLoop = false))

                emitted.add(elseBlock)
                val savedNext = nextBlock
                nextBlock = mergeBlock

                if (thenBlock !in emitted && thenBlock in loopInfo.body) {
                    emitInLoop(thenBlock, loopInfo)
                }

                nextBlock = savedNext
                labelStack.removeLast() // Remove IF label
                emitter.writeByte(WasmOp.END)

                if (mergeBlock !in emitted) {
                    emitInLoop(mergeBlock, loopInfo)
                }
                return
            }
        }

        // Simplified IF for empty then branch
        if (thenIsEmpty && thenBlock !in emitted && mergeBlock != null && !mergeHasControlFlow) {
            val thenSucc = successors[thenBlock]?.firstOrNull()
            if (thenSucc == mergeBlock && mergeBlock in loopInfo.body) {
                instructionEmitter.emitValue(condition)
                emitter.writeByte(WasmOp.I32_EQZ)
                emitter.writeByte(WasmOp.IF)
                emitter.writeByte(0x40) // void

                // Add IF to label stack
                labelStack.add(Label(targetBlock = null, isLoop = false))

                emitted.add(thenBlock)
                val savedNext = nextBlock
                nextBlock = mergeBlock

                if (elseBlock !in emitted && elseBlock in loopInfo.body) {
                    emitInLoop(elseBlock, loopInfo)
                }

                nextBlock = savedNext
                labelStack.removeLast() // Remove IF label
                emitter.writeByte(WasmOp.END)

                if (mergeBlock !in emitted) {
                    emitInLoop(mergeBlock, loopInfo)
                }
                return
            }
        }

        // Special case: CONTINUE vs BREAK (do-while pattern)
        // One branch goes back to loop header (continue), other exits loop (break)
        val thenIsContinue = thenBlock == loopInfo.header && thenBlock in emitted
        val elseIsContinue = elseBlock == loopInfo.header && elseBlock in emitted
        val thenIsBreak = thenBlock in loopInfo.exitTargets
        val elseIsBreak = elseBlock in loopInfo.exitTargets

        if (thenIsContinue && elseIsBreak) {
            // if (condition) continue else break
            instructionEmitter.emitValue(condition)
            val continueDepth = findBranchDepth(loopInfo.header, isLoopTarget = true)
            emitter.writeByte(WasmOp.BR_IF)
            emitter.writeU32Leb(continueDepth)
            // Fall through means break
            val breakDepth = findLoopBreakDepth(loopInfo)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(breakDepth)
            return
        }

        if (elseIsContinue && thenIsBreak) {
            // if (condition) break else continue
            instructionEmitter.emitValue(condition)
            val breakDepth = findLoopBreakDepth(loopInfo)
            emitter.writeByte(WasmOp.BR_IF)
            emitter.writeU32Leb(breakDepth)
            // Fall through means continue
            val continueDepth = findBranchDepth(loopInfo.header, isLoopTarget = true)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(continueDepth)
            return
        }

        // Full BLOCK + IF/ELSE
        emitter.writeByte(WasmOp.BLOCK)
        emitter.writeByte(0x40) // void
        labelStack.add(Label(targetBlock = mergeBlock, isLoop = false))

        instructionEmitter.emitValue(condition)
        emitter.writeByte(WasmOp.IF)
        emitter.writeByte(0x40) // void

        // Add IF to label stack
        labelStack.add(Label(targetBlock = null, isLoop = false))

        // Handle then branch
        if (thenBlock in loopInfo.exitTargets) {
            // Break out of loop
            val depth = findLoopBreakDepth(loopInfo)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
        } else if (thenBlock == loopInfo.header && thenBlock in emitted) {
            // Continue to loop header
            val depth = findBranchDepth(loopInfo.header, isLoopTarget = true)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
        } else if (thenBlock !in emitted && thenBlock != mergeBlock && thenBlock in loopInfo.body) {
            emitInLoop(thenBlock, loopInfo)
        } else if (thenBlock == mergeBlock) {
            emitted.add(thenBlock)
        }

        emitter.writeByte(WasmOp.ELSE)

        // Handle else branch
        if (elseBlock in loopInfo.exitTargets) {
            // Break out of loop
            val depth = findLoopBreakDepth(loopInfo)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
        } else if (elseBlock == loopInfo.header && elseBlock in emitted) {
            // Continue to loop header
            val depth = findBranchDepth(loopInfo.header, isLoopTarget = true)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
        } else if (elseBlock !in emitted && elseBlock != mergeBlock && elseBlock in loopInfo.body) {
            emitInLoop(elseBlock, loopInfo)
        } else if (elseBlock == mergeBlock) {
            emitted.add(elseBlock)
        }

        labelStack.removeLast() // Remove IF label
        emitter.writeByte(WasmOp.END) // end if
        emitter.writeByte(WasmOp.END) // end block
        labelStack.removeLast() // Remove BLOCK label

        // Emit merge block if it's in loop and hasn't been emitted
        if (mergeBlock != null && mergeBlock in loopInfo.body) {
            if (mergeBlock in emitted && mergeBlock == thenBlock || mergeBlock == elseBlock) {
                emitted.remove(mergeBlock)
                emitInLoop(mergeBlock, loopInfo)
            } else if (mergeBlock !in emitted) {
                emitInLoop(mergeBlock, loopInfo)
            }
        }
    }

    /**
     * Handle a branch instruction with proper depth calculation.
     */
    private fun handleBranch(dest: IRBasicBlock, currentLoop: LoopInfo?) {
        // Check if this branches to the block that will be emitted next
        // In that case, we can just fall through (no explicit branch needed)
        if (dest == nextBlock) {
            return // Fall through
        }

        // Check if this is a back-edge (continue to loop header)
        if (loopInfoMap.containsKey(dest) && dest in emitted) {
            // This is a continue - branch to loop start
            val depth = findBranchDepth(dest, isLoopTarget = true)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
            return
        }

        // Check if we're branching to a block that will cause a loop continue
        // This happens when we branch to a block that eventually branches back to the loop header
        if (currentLoop != null && dest in currentLoop.body) {
            // Check if this block leads directly back to the header (it's a continue target)
            val destSuccessors = successors[dest] ?: emptyList()
            if (destSuccessors.size == 1 && destSuccessors[0] == currentLoop.header) {
                // This is a continue - branch to loop start
                // But we need to emit the destination block's instructions first if they haven't been
                if (dest in emitted) {
                    // Already emitted, just branch back
                    val depth = findBranchDepth(currentLoop.header, isLoopTarget = true)
                    emitter.writeByte(WasmOp.BR)
                    emitter.writeU32Leb(depth)
                    return
                }
                // If not emitted, we'll emit it and it will handle the continue
            }
        }

        // Check if we're breaking out of a loop
        if (currentLoop != null && dest in currentLoop.exitTargets) {
            // Break to loop exit (target the BLOCK wrapping the LOOP)
            val depth = findLoopBreakDepth(currentLoop)
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(depth)
            return
        }

        // Check if we need to break out to reach the destination
        // (destination is on the label stack, meaning it's a merge point we need to jump to)
        val labelIndex = findLabelIndex(dest)
        if (labelIndex != null) {
            // Need to branch to break out
            emitter.writeByte(WasmOp.BR)
            emitter.writeU32Leb(labelIndex)
            return
        }

        // Fall-through: continue to next block (will be emitted sequentially)
        if (dest !in emitted) {
            emit(dest, currentLoop)
        }
    }

    /**
     * Find the label stack index for a target block (for br instruction).
     * Returns null if the block is not a merge point on the label stack.
     */
    private fun findLabelIndex(target: IRBasicBlock): Int? {
        for (i in labelStack.indices.reversed()) {
            val label = labelStack[i]
            if (label.targetBlock == target && !label.isLoop) {
                return labelStack.size - 1 - i
            }
        }
        return null
    }

    /**
     * Find br depth to branch to a specific block/loop.
     */
    private fun findBranchDepth(target: IRBasicBlock, isLoopTarget: Boolean): Int {
        for (i in labelStack.indices.reversed()) {
            val label = labelStack[i]
            if (label.targetBlock == target && label.isLoop == isLoopTarget) {
                return labelStack.size - 1 - i
            }
        }
        error("Label not found for target: ${target.name}, isLoop=$isLoopTarget")
    }

    /**
     * Find br depth to break out of a loop (target its enclosing BLOCK).
     */
    private fun findLoopBreakDepth(loopInfo: LoopInfo): Int {
        // Find the BLOCK that wraps this loop (the one before the LOOP label)
        for (i in labelStack.indices.reversed()) {
            val label = labelStack[i]
            if (label.loopInfo == loopInfo && !label.isLoop) {
                return labelStack.size - 1 - i
            }
        }
        error("Loop break target not found for loop: ${loopInfo.header.name}")
    }


    /**
     * Emit if/else construct outside any loop.
     */
    private fun emitIfElse(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        currentLoop: LoopInfo?
    ) {
        if (thenBlock in emitted && elseBlock in emitted) {
            return
        }

        // Check if one branch is empty (just falls through to merge)
        val thenIsEmpty = isEmptyBlock(thenBlock)
        val elseIsEmpty = isEmptyBlock(elseBlock)

        // Find merge block
        val mergeBlock = findMergeBlock(thenBlock, elseBlock, null)

        // Check if the merge block itself has complex control flow
        // If so, we can't use the simplified IF - we need full BLOCK+IF/ELSE
        val mergeHasControlFlow = mergeBlock != null && mergeBlock.terminator is IRInstruction.CondBr

        // Special case: if one branch is empty and just goes to merge, we can simplify
        // BUT only if the merge block doesn't have further control flow
        if (thenIsEmpty && thenBlock !in emitted && mergeBlock != null && !mergeHasControlFlow) {
            val thenSucc = successors[thenBlock]?.firstOrNull()
            if (thenSucc == mergeBlock) {
                // Then branch is empty, emit: if (condition) {} else { elseBlock }
                // Which is: if (!condition) { elseBlock }
                // Invert condition
                instructionEmitter.emitValue(condition)
                emitter.writeByte(WasmOp.I32_EQZ) // invert condition

                emitter.writeByte(WasmOp.IF)
                emitter.writeByte(0x40) // void

                // Add IF to label stack
                labelStack.add(Label(targetBlock = null, isLoop = false))

                emitted.add(thenBlock) // Mark as emitted

                // Set next block so branches to it can fall through
                val savedNextBlock = nextBlock
                nextBlock = mergeBlock

                if (elseBlock !in emitted) {
                    emit(elseBlock, currentLoop)
                }

                nextBlock = savedNextBlock

                labelStack.removeLast() // Remove IF label
                emitter.writeByte(WasmOp.END) // end if

                // Emit merge block
                if (mergeBlock !in emitted) {
                    emit(mergeBlock, currentLoop)
                }
                return
            }
        }

        if (elseIsEmpty && elseBlock !in emitted && mergeBlock != null && !mergeHasControlFlow) {
            val elseSucc = successors[elseBlock]?.firstOrNull()
            if (elseSucc == mergeBlock) {
                // Else branch is empty, emit: if (condition) { thenBlock }
                instructionEmitter.emitValue(condition)

                emitter.writeByte(WasmOp.IF)
                emitter.writeByte(0x40) // void

                // Add IF to label stack
                labelStack.add(Label(targetBlock = null, isLoop = false))

                emitted.add(elseBlock) // Mark as emitted

                // Set next block so branches to it can fall through
                val savedNextBlock = nextBlock
                nextBlock = mergeBlock

                if (thenBlock !in emitted) {
                    emit(thenBlock, currentLoop)
                }

                nextBlock = savedNextBlock

                labelStack.removeLast() // Remove IF label
                emitter.writeByte(WasmOp.END) // end if

                // Emit merge block
                if (mergeBlock !in emitted) {
                    emit(mergeBlock, currentLoop)
                }
                return
            }
        }

        // General case: both branches have content
        // Emit wrapped block
        emitter.writeByte(WasmOp.BLOCK)
        emitter.writeByte(0x40) // void
        labelStack.add(Label(targetBlock = mergeBlock, isLoop = false))

        // Emit condition
        instructionEmitter.emitValue(condition)

        // Emit IF
        emitter.writeByte(WasmOp.IF)
        emitter.writeByte(0x40) // void

        // Add IF to label stack
        labelStack.add(Label(targetBlock = null, isLoop = false))

        // Emit then branch (but not if it's the merge block itself)
        if (thenBlock !in emitted && thenBlock != mergeBlock) {
            emit(thenBlock, currentLoop)
        } else if (thenBlock == mergeBlock) {
            emitted.add(thenBlock) // Mark as "will be emitted outside"
        }

        // Emit ELSE
        emitter.writeByte(WasmOp.ELSE)

        // Emit else branch (but not if it's the merge block itself)
        if (elseBlock !in emitted && elseBlock != mergeBlock) {
            emit(elseBlock, currentLoop)
        } else if (elseBlock == mergeBlock) {
            emitted.add(elseBlock) // Mark as "will be emitted outside"
        }

        labelStack.removeLast() // Remove IF label
        emitter.writeByte(WasmOp.END) // end if
        emitter.writeByte(WasmOp.END) // end block
        labelStack.removeLast() // Remove BLOCK label

        // Emit merge block
        if (mergeBlock != null && mergeBlock in emitted) {
            // Merge block was marked for emission, now actually emit it
            emitted.remove(mergeBlock) // Remove from emitted so we can emit it
            emit(mergeBlock, currentLoop)
        } else if (mergeBlock != null && mergeBlock !in emitted) {
            emit(mergeBlock, currentLoop)
        }
    }

    /**
     * Check if a block is empty (contains no instructions except terminator).
     */
    private fun isEmptyBlock(block: IRBasicBlock): Boolean {
        // A block is empty if it has no instructions except the terminator and no phi copies
        val hasInstructions = block.instructions.any {
            it !is IRInstruction.Ret && it !is IRInstruction.Br && it !is IRInstruction.CondBr
        }
        val hasPhiCopies = phiResolver.getPhiCopies(block).isNotEmpty()
        return !hasInstructions && !hasPhiCopies
    }

    /**
     * Find the IMMEDIATE common merge point of two branches using BFS.
     * This finds the closest common successor, not just any common block in program order.
     * This is crucial for nested if-else-if structures.
     */
    private fun findMergeBlock(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        loopBody: Set<IRBasicBlock>?
    ): IRBasicBlock? {
        // Use BFS from both branches simultaneously to find the closest common block
        val thenVisited = mutableSetOf<IRBasicBlock>()
        val elseVisited = mutableSetOf<IRBasicBlock>()
        val thenQueue = ArrayDeque<IRBasicBlock>()
        val elseQueue = ArrayDeque<IRBasicBlock>()

        thenQueue.add(thenBlock)
        thenVisited.add(thenBlock)
        elseQueue.add(elseBlock)
        elseVisited.add(elseBlock)

        // BFS level by level from both sides
        while (thenQueue.isNotEmpty() || elseQueue.isNotEmpty()) {
            // Process one level of then-side BFS
            repeat(thenQueue.size) {
                val block = thenQueue.removeFirst()
                for (succ in successors[block] ?: emptyList()) {
                    if (loopBody != null && succ !in loopBody) continue
                    if (succ in elseVisited && succ != thenBlock && succ != elseBlock) {
                        return succ // Found closest common successor
                    }
                    if (succ !in thenVisited) {
                        thenVisited.add(succ)
                        thenQueue.add(succ)
                    }
                }
            }

            // Process one level of else-side BFS
            repeat(elseQueue.size) {
                val block = elseQueue.removeFirst()
                for (succ in successors[block] ?: emptyList()) {
                    if (loopBody != null && succ !in loopBody) continue
                    if (succ in thenVisited && succ != thenBlock && succ != elseBlock) {
                        return succ // Found closest common successor
                    }
                    if (succ !in elseVisited) {
                        elseVisited.add(succ)
                        elseQueue.add(succ)
                    }
                }
            }
        }

        return null
    }

    /**
     * Emit block body (instructions + phi copies).
     */
    private fun emitBlockInstructions(block: IRBasicBlock) {
        for (inst in block.instructions) {
            when (inst) {
                is IRInstruction.Ret,
                is IRInstruction.Br,
                is IRInstruction.CondBr -> continue
                else -> instructionEmitter.emitInstruction(inst)
            }
        }

        for (phiCopy in phiResolver.getPhiCopies(block)) {
            instructionEmitter.emitPhiCopy(phiCopy)
        }
    }
}
