package wasm

import llvm.*

/**
 * Relooper - Converts arbitrary CFG to structured WebAssembly control flow.
 */
class Relooper(
    private val function: IRFunction,
    private val phiResolver: PhiResolver
) {
    private val successors = mutableMapOf<IRBasicBlock, List<IRBasicBlock>>()
    private val predecessors = mutableMapOf<IRBasicBlock, MutableList<IRBasicBlock>>()
    private val processed = mutableSetOf<IRBasicBlock>()

    private data class LoopInfo(
        val header: IRBasicBlock,
        val body: Set<IRBasicBlock>,
        val exitTargets: Set<IRBasicBlock>
    )

    private val loopInfoMap = mutableMapOf<IRBasicBlock, LoopInfo>()

    private data class Label(
        val targetBlock: IRBasicBlock?,
        val isLoop: Boolean,
        val loopInfo: LoopInfo? = null
    )

    private val labelStack = mutableListOf<Label>()
    private var nextBlock: IRBasicBlock? = null

    private fun IRBasicBlock.successors(): List<IRBasicBlock> = successors[this] ?: emptyList()

    private fun IRBasicBlock.leadsToHeader(loopInfo: LoopInfo): Boolean =
        successors().let { it.size == 1 && it.first() == loopInfo.header }

    fun reloop(): StructuredCFG {
        if (function.basicBlocks.isEmpty()) {
            return StructuredCFG.Sequence(nodes = emptyList())
        }

        buildCFG()
        analyzeLoops()
        return process(function.basicBlocks.first(), null)
    }

    private fun buildCFG() {
        for (block in function.basicBlocks) {
            val s = when (val term = block.terminator) {
                is IRInstruction.Br -> listOf(term.dest)
                is IRInstruction.CondBr -> listOf(term.thenBlock, term.elseBlock)
                else -> emptyList()
            }
            successors[block] = s
            s.forEach { predecessors.getOrPut(it) { mutableListOf() }.add(block) }
        }
    }

    private fun analyzeLoops() {
        val visited = mutableSetOf<IRBasicBlock>()
        val dfsStack = mutableSetOf<IRBasicBlock>()
        val backEdges = mutableListOf<Pair<IRBasicBlock, IRBasicBlock>>()

        fun dfs(block: IRBasicBlock) {
            visited.add(block)
            dfsStack.add(block)
            for (succ in block.successors()) {
                when (succ) {
                    in dfsStack -> backEdges.add(block to succ)
                    !in visited -> dfs(succ)
                }
            }
            dfsStack.remove(block)
        }

        function.basicBlocks.firstOrNull()?.let { dfs(it) }

        for ((source, header) in backEdges) {
            val body = computeLoopBody(header, source)
            loopInfoMap[header] = LoopInfo(header, body, body.findExitTargets())
        }
    }

    private fun computeLoopBody(header: IRBasicBlock, backEdgeSource: IRBasicBlock): Set<IRBasicBlock> {
        val body = mutableSetOf(header, backEdgeSource)
        val worklist = mutableListOf(backEdgeSource)

        while (worklist.isNotEmpty()) {
            for (pred in predecessors[worklist.removeLast()] ?: emptyList()) {
                if (pred !in body && pred != header) {
                    body.add(pred)
                    worklist.add(pred)
                }
            }
        }
        return body
    }

    private fun Set<IRBasicBlock>.findExitTargets(): Set<IRBasicBlock> =
        flatMapTo(mutableSetOf()) { block -> block.successors().filter { it !in this } }

    // Label stack helpers
    private inline fun <T> withLabel(label: Label, block: () -> T): T {
        labelStack.add(label)
        return try {
            block()
        } finally {
            labelStack.removeLast()
        }
    }

    private inline fun <T> withBlockLabel(targetBlock: IRBasicBlock?, loopInfo: LoopInfo? = null, block: () -> T): T =
        withLabel(Label(targetBlock, isLoop = false, loopInfo = loopInfo), block)

    private inline fun <T> withLoopLabel(header: IRBasicBlock, loopInfo: LoopInfo, block: () -> T): T =
        withLabel(Label(header, isLoop = true, loopInfo = loopInfo), block)

    private inline fun <T> withNextBlock(block: IRBasicBlock?, action: () -> T): T {
        val saved = nextBlock
        nextBlock = block
        return try {
            action()
        } finally {
            nextBlock = saved
        }
    }

    private fun process(block: IRBasicBlock, currentLoop: LoopInfo?): StructuredCFG {
        if (block in processed) return StructuredCFG.Sequence(emptyList())

        loopInfoMap[block]?.let { return processLoop(it) }

        processed.add(block)
        val nodes = mutableListOf<StructuredCFG>()
        nodes.add(StructuredCFG.Block(block))
        nodes.add(processTerminator(block.terminator, currentLoop, inLoop = false))
        return StructuredCFG.sequence(nodes)
    }

    private fun processTerminator(term: IRInstruction?, currentLoop: LoopInfo?, inLoop: Boolean): StructuredCFG {
        return when (term) {
            is IRInstruction.Ret -> StructuredCFG.Return(term.value)
            is IRInstruction.Br -> if (inLoop && currentLoop != null) {
                processBranchInLoop(term.dest, currentLoop)
            } else {
                processBranch(term.dest, currentLoop)
            }

            is IRInstruction.CondBr -> if (inLoop && currentLoop != null) {
                processIfElseInLoop(term.thenBlock, term.elseBlock, term.condition, currentLoop)
            } else {
                processIfElse(term.thenBlock, term.elseBlock, term.condition, currentLoop)
            }

            null -> StructuredCFG.Sequence(emptyList())
            else -> error("Unexpected terminator: $term")
        }
    }

    private fun processLoop(loopInfo: LoopInfo): StructuredCFG {
        val nodes = mutableListOf<StructuredCFG>()

        val loopBody = withBlockLabel(null, loopInfo) {
            withLoopLabel(loopInfo.header, loopInfo) {
                processInLoop(loopInfo.header, loopInfo)
            }
        }

        nodes.add(StructuredCFG.WasmBlock(StructuredCFG.WasmLoop(loopBody)))

        loopInfo.exitTargets.filter { it !in processed }.forEach {
            nodes.add(process(it, null))
        }

        return StructuredCFG.sequence(nodes)
    }

    private fun processInLoop(block: IRBasicBlock, loopInfo: LoopInfo): StructuredCFG {
        if (block in processed || block !in loopInfo.body) return StructuredCFG.Sequence(emptyList())

        processed.add(block)
        val nodes = mutableListOf<StructuredCFG>()
        nodes.add(StructuredCFG.Block(block))
        nodes.add(processTerminator(block.terminator, loopInfo, inLoop = true))
        return StructuredCFG.sequence(nodes)
    }

    private fun processBranchInLoop(dest: IRBasicBlock, loopInfo: LoopInfo): StructuredCFG {
        if (dest == nextBlock) return StructuredCFG.Sequence(emptyList())

        // Continue to loop header
        if (dest == loopInfo.header && dest in processed) {
            return StructuredCFG.Branch(findBranchDepth(loopInfo.header))
        }

        // Increment block leading to header
        if (dest in loopInfo.body && dest.leadsToHeader(loopInfo)) {
            return if (dest !in processed) {
                processInLoop(dest, loopInfo)
            } else {
                StructuredCFG.sequence(
                    StructuredCFG.Block(dest),
                    StructuredCFG.Branch(findBranchDepth(loopInfo.header))
                )
            }
        }

        // Break out of loop
        if (dest in loopInfo.exitTargets) {
            return StructuredCFG.Branch(findLoopBreakDepth(loopInfo))
        }

        // Branch to merge point
        findLabelIndex(dest)?.let { return StructuredCFG.Branch(it) }

        // Process destination inline
        if (dest in loopInfo.body && dest !in processed) {
            return processInLoop(dest, loopInfo)
        } else if (dest in loopInfo.body && dest in processed && dest.leadsToHeader(loopInfo)) {
            return StructuredCFG.sequence(
                StructuredCFG.Block(dest),
                StructuredCFG.Branch(findBranchDepth(loopInfo.header))
            )
        }

        return StructuredCFG.Sequence(emptyList())
    }

    private fun processIfElseInLoop(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        loopInfo: LoopInfo
    ): StructuredCFG {
        if (thenBlock in processed && elseBlock in processed) return StructuredCFG.Sequence(emptyList())

        val mergeBlock = findMergeBlock(thenBlock, elseBlock, loopInfo.body)
        val mergeHasControlFlow = mergeBlock?.terminator is IRInstruction.CondBr

        tryProcessSimplifiedIfInLoop(thenBlock, elseBlock, condition, loopInfo, mergeBlock, mergeHasControlFlow)
            ?.let { return it }

        tryProcessContinueBreakPattern(thenBlock, elseBlock, condition, loopInfo)?.let { return it }

        // Full BLOCK + IF/ELSE
        return processFullIfElseInLoop(thenBlock, elseBlock, condition, loopInfo, mergeBlock)
    }

    private fun tryProcessSimplifiedIfInLoop(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        loopInfo: LoopInfo,
        mergeBlock: IRBasicBlock?,
        mergeHasControlFlow: Boolean
    ): StructuredCFG? {
        if (mergeBlock == null || mergeHasControlFlow || mergeBlock !in loopInfo.body) return null

        val thenIsEmpty = isEmptyBlock(thenBlock)
        val elseIsEmpty = isEmptyBlock(elseBlock)

        // Empty else branch
        if (elseIsEmpty && elseBlock !in processed && elseBlock.successors().firstOrNull() == mergeBlock) {
            return processSimplifiedIf(condition, invert = false, emptyBlock = elseBlock, mergeBlock = mergeBlock) {
                if (thenBlock !in processed && thenBlock in loopInfo.body) processInLoop(thenBlock, loopInfo)
                else StructuredCFG.Sequence(emptyList())
            }.let { ifNode ->
                val nodes = mutableListOf(ifNode)
                if (mergeBlock !in processed) nodes.add(processInLoop(mergeBlock, loopInfo))
                StructuredCFG.sequence(nodes)
            }
        }

        // Empty then branch
        if (thenIsEmpty && thenBlock !in processed && thenBlock.successors().firstOrNull() == mergeBlock) {
            return processSimplifiedIf(condition, invert = true, emptyBlock = thenBlock, mergeBlock = mergeBlock) {
                if (elseBlock !in processed && elseBlock in loopInfo.body) processInLoop(elseBlock, loopInfo)
                else StructuredCFG.Sequence(emptyList())
            }.let { ifNode ->
                val nodes = mutableListOf(ifNode)
                if (mergeBlock !in processed) nodes.add(processInLoop(mergeBlock, loopInfo))
                StructuredCFG.sequence(nodes)
            }
        }

        return null
    }

    private fun tryProcessContinueBreakPattern(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        loopInfo: LoopInfo
    ): StructuredCFG? {
        val thenIsContinue = thenBlock == loopInfo.header && thenBlock in processed
        val elseIsContinue = elseBlock == loopInfo.header && elseBlock in processed
        val thenIsBreak = thenBlock in loopInfo.exitTargets
        val elseIsBreak = elseBlock in loopInfo.exitTargets

        if (thenIsContinue && elseIsBreak) {
            return StructuredCFG.sequence(
                StructuredCFG.BranchIf(condition, findBranchDepth(loopInfo.header)),
                StructuredCFG.Branch(findLoopBreakDepth(loopInfo))
            )
        }

        if (elseIsContinue && thenIsBreak) {
            return StructuredCFG.sequence(
                StructuredCFG.BranchIf(condition, findLoopBreakDepth(loopInfo)),
                StructuredCFG.Branch(findBranchDepth(loopInfo.header))
            )
        }

        return null
    }

    private fun processFullIfElseInLoop(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        loopInfo: LoopInfo,
        mergeBlock: IRBasicBlock?
    ): StructuredCFG {
        val ifElse = withBlockLabel(mergeBlock) {
            withBlockLabel(null) {
                val thenBody = processBranchInLoopForIf(thenBlock, loopInfo, mergeBlock)
                val elseBody = processBranchInLoopForIf(elseBlock, loopInfo, mergeBlock)
                StructuredCFG.WasmIfElse(condition, thenBody, elseBody)
            }
        }

        val nodes = mutableListOf<StructuredCFG>(StructuredCFG.WasmBlock(ifElse))
        nodes.add(processMergeBlockInLoop(mergeBlock, thenBlock, elseBlock, loopInfo))
        return StructuredCFG.sequence(nodes)
    }

    private fun processBranchInLoopForIf(
        block: IRBasicBlock,
        loopInfo: LoopInfo,
        mergeBlock: IRBasicBlock?
    ): StructuredCFG = when (block) {
        in loopInfo.exitTargets -> StructuredCFG.Branch(findLoopBreakDepth(loopInfo))
        loopInfo.header if block in processed -> StructuredCFG.Branch(findBranchDepth(loopInfo.header))
        !in processed if block != mergeBlock && block in loopInfo.body -> processInLoop(block, loopInfo)
        mergeBlock -> StructuredCFG.Sequence(emptyList()).also { processed.add(block) }
        else -> StructuredCFG.Sequence(emptyList())
    }

    private fun processMergeBlockInLoop(
        mergeBlock: IRBasicBlock?,
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        loopInfo: LoopInfo
    ): StructuredCFG {
        if (mergeBlock == null || mergeBlock !in loopInfo.body) return StructuredCFG.Sequence(emptyList())

        if (mergeBlock in processed && (mergeBlock == thenBlock || mergeBlock == elseBlock)) {
            processed.remove(mergeBlock)
            return processInLoop(mergeBlock, loopInfo)
        } else if (mergeBlock !in processed) {
            return processInLoop(mergeBlock, loopInfo)
        }
        return StructuredCFG.Sequence(emptyList())
    }

    private fun processBranch(dest: IRBasicBlock, currentLoop: LoopInfo?): StructuredCFG {
        if (dest == nextBlock) {
            return StructuredCFG.Sequence(emptyList())
        }
        // Back-edge to loop header
        if (loopInfoMap.containsKey(dest) && dest in processed) {
            return StructuredCFG.Branch(findBranchDepth(dest))
        }
        // Continue through the increment block
        if (currentLoop != null && dest in currentLoop.body && dest.leadsToHeader(currentLoop) && dest in processed) {
            return StructuredCFG.Branch(findBranchDepth(currentLoop.header))
        }
        // Break out of loop
        if (currentLoop != null && dest in currentLoop.exitTargets) {
            return StructuredCFG.Branch(findLoopBreakDepth(currentLoop))
        }

        // Branch to merge point
        findLabelIndex(dest)?.let { return StructuredCFG.Branch(it) }

        // Process destination inline
        if (dest !in processed) {
            return process(dest, currentLoop)
        }
        return StructuredCFG.Sequence(emptyList())
    }

    private fun processIfElse(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        currentLoop: LoopInfo?
    ): StructuredCFG {
        if (thenBlock in processed && elseBlock in processed) return StructuredCFG.Sequence(emptyList())

        val mergeBlock = findMergeBlock(thenBlock, elseBlock, null)
        val mergeHasControlFlow = mergeBlock?.terminator is IRInstruction.CondBr

        // Try simplified IF for an empty branch
        tryProcessSimplifiedIf(thenBlock, elseBlock, condition, currentLoop, mergeBlock, mergeHasControlFlow)
            ?.let { return it }

        // Full BLOCK + IF/ELSE
        return processFullIfElse(thenBlock, elseBlock, condition, currentLoop, mergeBlock)
    }

    private fun tryProcessSimplifiedIf(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        currentLoop: LoopInfo?,
        mergeBlock: IRBasicBlock?,
        mergeHasControlFlow: Boolean
    ): StructuredCFG? {
        if (mergeBlock == null || mergeHasControlFlow) return null

        val thenIsEmpty = isEmptyBlock(thenBlock)
        val elseIsEmpty = isEmptyBlock(elseBlock)

        // Empty then branch
        if (thenIsEmpty && thenBlock !in processed && thenBlock.successors().firstOrNull() == mergeBlock) {
            return processSimplifiedIf(condition, invert = true, emptyBlock = thenBlock, mergeBlock = mergeBlock) {
                if (elseBlock !in processed) process(elseBlock, currentLoop) else StructuredCFG.Sequence(emptyList())
            }.let { ifNode ->
                val nodes = mutableListOf(ifNode)
                if (mergeBlock !in processed) nodes.add(process(mergeBlock, currentLoop))
                StructuredCFG.sequence(nodes)
            }
        }

        // Empty else branch
        if (elseIsEmpty && elseBlock !in processed && elseBlock.successors().firstOrNull() == mergeBlock) {
            return processSimplifiedIf(condition, invert = false, emptyBlock = elseBlock, mergeBlock = mergeBlock) {
                if (thenBlock !in processed) process(thenBlock, currentLoop) else StructuredCFG.Sequence(emptyList())
            }.let { ifNode ->
                val nodes = mutableListOf(ifNode)
                if (mergeBlock !in processed) nodes.add(process(mergeBlock, currentLoop))
                StructuredCFG.sequence(nodes)
            }
        }

        return null
    }

    private inline fun processSimplifiedIf(
        condition: IRValue,
        invert: Boolean,
        emptyBlock: IRBasicBlock,
        mergeBlock: IRBasicBlock,
        processContent: () -> StructuredCFG
    ): StructuredCFG {
        processed.add(emptyBlock)
        val content = withNextBlock(mergeBlock) { withBlockLabel(null) { processContent() } }
        return StructuredCFG.WasmIf(condition, invert, content)
    }

    private fun processFullIfElse(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        condition: IRValue,
        currentLoop: LoopInfo?,
        mergeBlock: IRBasicBlock?
    ): StructuredCFG {
        val ifElse = withBlockLabel(mergeBlock) {
            withBlockLabel(null) {
                val thenBody = processBranchForIf(thenBlock, mergeBlock, currentLoop)
                val elseBody = processBranchForIf(elseBlock, mergeBlock, currentLoop)
                StructuredCFG.WasmIfElse(condition, thenBody, elseBody)
            }
        }

        val nodes = mutableListOf<StructuredCFG>(StructuredCFG.WasmBlock(ifElse))
        nodes.add(processMergeBlock(mergeBlock, currentLoop))
        return StructuredCFG.sequence(nodes)
    }

    private fun processBranchForIf(
        block: IRBasicBlock,
        mergeBlock: IRBasicBlock?,
        currentLoop: LoopInfo?
    ): StructuredCFG = when (block) {
        !in processed if block != mergeBlock -> process(block, currentLoop)
        mergeBlock -> StructuredCFG.Sequence(emptyList()).also { processed.add(block) }
        else -> StructuredCFG.Sequence(emptyList())
    }

    private fun processMergeBlock(mergeBlock: IRBasicBlock?, currentLoop: LoopInfo?): StructuredCFG {
        if (mergeBlock == null) return StructuredCFG.Sequence(emptyList())

        if (mergeBlock in processed) {
            processed.remove(mergeBlock)
            return process(mergeBlock, currentLoop)
        }
        return process(mergeBlock, currentLoop)
    }

    private fun findLabelIndex(target: IRBasicBlock): Int? {
        for (i in labelStack.indices.reversed()) {
            val label = labelStack[i]
            if (label.targetBlock == target && !label.isLoop) {
                return labelStack.size - 1 - i
            }
        }
        return null
    }

    private fun findBranchDepth(target: IRBasicBlock): Int {
        for (i in labelStack.indices.reversed()) {
            val label = labelStack[i]
            if (label.targetBlock == target && label.isLoop) {
                return labelStack.size - 1 - i
            }
        }
        error("Label not found for target: ${target.name}")
    }

    private fun findLoopBreakDepth(loopInfo: LoopInfo): Int {
        for (i in labelStack.indices.reversed()) {
            val label = labelStack[i]
            if (label.loopInfo == loopInfo && !label.isLoop) {
                return labelStack.size - 1 - i
            }
        }
        error("Loop break target not found for loop: ${loopInfo.header.name}")
    }

    private fun isEmptyBlock(block: IRBasicBlock): Boolean {
        val hasInstructions = block.instructions.any {
            it !is IRInstruction.Ret && it !is IRInstruction.Br && it !is IRInstruction.CondBr
        }
        return !hasInstructions && phiResolver.getPhiCopies(block).isEmpty()
    }

    private fun findMergeBlock(
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock,
        loopBody: Set<IRBasicBlock>?
    ): IRBasicBlock? {
        val thenVisited = mutableSetOf(thenBlock)
        val elseVisited = mutableSetOf(elseBlock)
        val thenQueue = ArrayDeque<IRBasicBlock>().apply { add(thenBlock) }
        val elseQueue = ArrayDeque<IRBasicBlock>().apply { add(elseBlock) }

        while (thenQueue.isNotEmpty() || elseQueue.isNotEmpty()) {
            processBfsLevel(thenQueue, thenVisited, elseVisited, loopBody, thenBlock, elseBlock)?.let { return it }
            processBfsLevel(elseQueue, elseVisited, thenVisited, loopBody, thenBlock, elseBlock)?.let { return it }
        }
        return null
    }

    private fun processBfsLevel(
        queue: ArrayDeque<IRBasicBlock>,
        visited: MutableSet<IRBasicBlock>,
        otherVisited: Set<IRBasicBlock>,
        loopBody: Set<IRBasicBlock>?,
        thenBlock: IRBasicBlock,
        elseBlock: IRBasicBlock
    ): IRBasicBlock? {
        repeat(queue.size) {
            val block = queue.removeFirst()
            for (succ in block.successors()) {
                if (loopBody != null && succ !in loopBody) continue
                if (succ in otherVisited && succ != thenBlock && succ != elseBlock) return succ
                if (succ !in visited) {
                    visited.add(succ)
                    queue.add(succ)
                }
            }
        }
        return null
    }
}
