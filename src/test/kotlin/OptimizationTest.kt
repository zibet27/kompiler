import llvm.*
import llvm.opt.analysis.*
import llvm.opt.pass.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue
import kotlin.test.assertFalse
import kotlin.test.assertSame

class OptimizationTest {

    // ===========================================
    // CFG Analysis Tests
    // ===========================================

    @Test
    fun testCFGAnalysisSimple() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Create function: if (cond) { a } else { b } merge
        val fn = IRFunction("test", i32, listOf(IRArgument(context.i1, "cond")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        val thenBlock = IRBasicBlock("then")
        val elseBlock = IRBasicBlock("else")
        val merge = IRBasicBlock("merge")

        fn.basicBlocks.addAll(listOf(entry, thenBlock, elseBlock, merge))

        builder.positionAtEnd(entry)
        builder.buildCondBr(fn.parameters[0], thenBlock, elseBlock)

        builder.positionAtEnd(thenBlock)
        builder.buildBr(merge)

        builder.positionAtEnd(elseBlock)
        builder.buildBr(merge)

        builder.positionAtEnd(merge)
        builder.buildRet(IRIntConstant(0, i32))

        val cfg = CFGInfo.compute(fn)

        // Check successors
        assertEquals(setOf(thenBlock, elseBlock), cfg.successors(entry))
        assertEquals(setOf(merge), cfg.successors(thenBlock))
        assertEquals(setOf(merge), cfg.successors(elseBlock))
        assertEquals(emptySet(), cfg.successors(merge))

        // Check predecessors
        assertEquals(emptySet(), cfg.predecessors(entry))
        assertEquals(setOf(entry), cfg.predecessors(thenBlock))
        assertEquals(setOf(entry), cfg.predecessors(elseBlock))
        assertEquals(setOf(thenBlock, elseBlock), cfg.predecessors(merge))
    }

    @Test
    fun testCFGReachability() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        val fn = IRFunction("test", i32, listOf())
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        val reachable = IRBasicBlock("reachable")
        val unreachable = IRBasicBlock("unreachable")

        fn.basicBlocks.addAll(listOf(entry, reachable, unreachable))

        builder.positionAtEnd(entry)
        builder.buildBr(reachable)

        builder.positionAtEnd(reachable)
        builder.buildRet(IRIntConstant(0, i32))

        builder.positionAtEnd(unreachable)
        builder.buildRet(IRIntConstant(1, i32))

        val cfg = CFGInfo.compute(fn)

        assertTrue(cfg.isReachable(entry))
        assertTrue(cfg.isReachable(reachable))
        assertFalse(cfg.isReachable(unreachable))
    }

    // ===========================================
    // Dominance Analysis Tests
    // ===========================================

    @Test
    fun testDominanceSimple() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Linear CFG: entry -> a -> b -> exit
        val fn = IRFunction("test", i32, listOf())
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        val a = IRBasicBlock("a")
        val b = IRBasicBlock("b")
        val exit = IRBasicBlock("exit")

        fn.basicBlocks.addAll(listOf(entry, a, b, exit))

        builder.positionAtEnd(entry)
        builder.buildBr(a)

        builder.positionAtEnd(a)
        builder.buildBr(b)

        builder.positionAtEnd(b)
        builder.buildBr(exit)

        builder.positionAtEnd(exit)
        builder.buildRet(IRIntConstant(0, i32))

        val domInfo = DominanceInfo.compute(fn)

        // Entry dominates everything
        assertTrue(domInfo.dominates(entry, entry))
        assertTrue(domInfo.dominates(entry, a))
        assertTrue(domInfo.dominates(entry, b))
        assertTrue(domInfo.dominates(entry, exit))

        // Each block dominates its successors in linear CFG
        assertTrue(domInfo.dominates(a, b))
        assertTrue(domInfo.dominates(b, exit))

        // But not the other way around
        assertFalse(domInfo.dominates(exit, entry))
        assertFalse(domInfo.dominates(b, a))
    }

    @Test
    fun testDominanceFrontier() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Diamond CFG: entry -> (then, else) -> merge
        val fn = IRFunction("test", i32, listOf(IRArgument(context.i1, "cond")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        val thenBlock = IRBasicBlock("then")
        val elseBlock = IRBasicBlock("else")
        val merge = IRBasicBlock("merge")

        fn.basicBlocks.addAll(listOf(entry, thenBlock, elseBlock, merge))

        builder.positionAtEnd(entry)
        builder.buildCondBr(fn.parameters[0], thenBlock, elseBlock)

        builder.positionAtEnd(thenBlock)
        builder.buildBr(merge)

        builder.positionAtEnd(elseBlock)
        builder.buildBr(merge)

        builder.positionAtEnd(merge)
        builder.buildRet(IRIntConstant(0, i32))

        val domInfo = DominanceInfo.compute(fn)

        // Merge is in dominance frontier of both branches
        assertEquals(setOf(merge), domInfo.dominanceFrontier(thenBlock))
        assertEquals(setOf(merge), domInfo.dominanceFrontier(elseBlock))
        assertEquals(emptySet(), domInfo.dominanceFrontier(entry))
        assertEquals(emptySet(), domInfo.dominanceFrontier(merge))
    }

    // ===========================================
    // Mem2Reg Pass Tests
    // ===========================================

    @Test
    fun testMem2RegSimple() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Simple case: alloca, store, load, return
        val fn = IRFunction("test", i32, listOf(IRArgument(i32, "x")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        fn.basicBlocks.add(entry)

        builder.positionAtEnd(entry)
        val alloca = builder.buildAlloca(i32, "tmp")
        builder.buildStore(fn.parameters[0], alloca)
        val loaded = builder.buildLoad(i32, alloca, "val")
        builder.buildRet(loaded)

        val pass = Mem2RegPass()
        val modified = pass.runOnFunction(fn)

        assertTrue(modified, "Mem2Reg should modify the function")

        // Check that alloca is removed
        val allocas = entry.instructions.filterIsInstance<IRInstruction.Alloca>()
        assertEquals(0, allocas.size, "Alloca should be removed")

        // Check that loads are removed
        val loads = entry.instructions.filterIsInstance<IRInstruction.Load>()
        assertEquals(0, loads.size, "Load should be removed")

        // Check that stores are removed
        val stores = entry.instructions.filterIsInstance<IRInstruction.Store>()
        assertEquals(0, stores.size, "Store should be removed")
    }

    @Test
    fun testMem2RegWithPhi() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // If-else with different values stored
        val fn = IRFunction("test", i32, listOf(IRArgument(context.i1, "cond")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        val thenBlock = IRBasicBlock("then")
        val elseBlock = IRBasicBlock("else")
        val merge = IRBasicBlock("merge")

        fn.basicBlocks.addAll(listOf(entry, thenBlock, elseBlock, merge))

        builder.positionAtEnd(entry)
        val alloca = builder.buildAlloca(i32, "tmp")
        builder.buildCondBr(fn.parameters[0], thenBlock, elseBlock)

        builder.positionAtEnd(thenBlock)
        builder.buildStore(IRIntConstant(1, i32), alloca)
        builder.buildBr(merge)

        builder.positionAtEnd(elseBlock)
        builder.buildStore(IRIntConstant(2, i32), alloca)
        builder.buildBr(merge)

        builder.positionAtEnd(merge)
        val loaded = builder.buildLoad(i32, alloca, "val")
        builder.buildRet(loaded)

        val pass = Mem2RegPass()
        val modified = pass.runOnFunction(fn)

        assertTrue(modified, "Mem2Reg should modify the function")

        // Check that a PHI node was inserted at merge
        val phis = merge.instructions.filterIsInstance<IRInstruction.Phi>()
        assertEquals(1, phis.size, "Should have one PHI node")

        val phi = phis[0]
        assertEquals(2, phi.incoming.size, "PHI should have 2 incoming values")
    }

    @Test
    fun testMem2RegNonPromotable() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Alloca with address escaping (passed to a call)
        val printf = IRFunction("printf", context.void, listOf(IRArgument(IRType.Pointer(context.i8), "fmt")), isExternal = true)
        module.functions.add(printf)

        val fn = IRFunction("test", i32, listOf())
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        fn.basicBlocks.add(entry)

        builder.positionAtEnd(entry)
        val alloca = builder.buildAlloca(i32, "tmp")
        builder.buildStore(IRIntConstant(42, i32), alloca)
        // Pass address to call - makes it non-promotable
        val cast = builder.buildBitCast(alloca, IRType.Pointer(context.i8), "cast")
        builder.buildCall(printf, listOf(cast))
        val loaded = builder.buildLoad(i32, alloca, "val")
        builder.buildRet(loaded)

        val pass = Mem2RegPass()
        val modified = pass.runOnFunction(fn)

        // Should not be modified because alloca escapes
        assertFalse(modified, "Mem2Reg should not modify non-promotable alloca")

        // Alloca should still be there
        val allocas = entry.instructions.filterIsInstance<IRInstruction.Alloca>()
        assertEquals(1, allocas.size, "Alloca should remain")
    }

    // ===========================================
    // Inlining Pass Tests
    // ===========================================

    @Test
    fun testInliningSimple() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Create callee: add1(x) = x + 1
        val add1 = IRFunction("add1", i32, listOf(IRArgument(i32, "x")))
        module.functions.add(add1)

        val add1Entry = IRBasicBlock("entry")
        add1.basicBlocks.add(add1Entry)

        builder.positionAtEnd(add1Entry)
        val result = builder.buildAdd(add1.parameters[0], IRIntConstant(1, i32), "res")
        builder.buildRet(result)

        // Create caller: main() = add1(5)
        val main = IRFunction("main", i32, listOf())
        module.functions.add(main)

        val mainEntry = IRBasicBlock("entry")
        main.basicBlocks.add(mainEntry)

        builder.positionAtEnd(mainEntry)
        val callResult = builder.buildCall(add1, listOf(IRIntConstant(5, i32)), "call")
        builder.buildRet(callResult)

        val pass = InliningPass(sizeThreshold = 100)
        val modified = pass.runOnModule(module)

        assertTrue(modified, "Inlining should modify the module")

        // Check that main now has more than one block (due to inlining)
        assertTrue(main.basicBlocks.size > 1, "Main should have multiple blocks after inlining")

        // Check that there's no longer a call to add1 in the original entry
        val calls = mainEntry.instructions.filterIsInstance<IRInstruction.Call>()
            .filter { it.function === add1 || it.function.ref() == add1.ref() }
        assertEquals(0, calls.size, "Call to add1 should be inlined")
    }

    @Test
    fun testInliningRecursiveNotInlined() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Create recursive function: fact(n) = n <= 1 ? 1 : n * fact(n-1)
        val fact = IRFunction("fact", i32, listOf(IRArgument(i32, "n")))
        module.functions.add(fact)

        val entry = IRBasicBlock("entry")
        val recurse = IRBasicBlock("recurse")
        val done = IRBasicBlock("done")

        fact.basicBlocks.addAll(listOf(entry, recurse, done))

        builder.positionAtEnd(entry)
        val cond = builder.buildICmp(IRInstruction.ICmp.Predicate.SLE, fact.parameters[0], IRIntConstant(1, i32), "cond")
        builder.buildCondBr(cond, done, recurse)

        builder.positionAtEnd(recurse)
        val nMinus1 = builder.buildSub(fact.parameters[0], IRIntConstant(1, i32), "n_minus_1")
        val recCall = builder.buildCall(fact, listOf(nMinus1), "rec")
        val mulResult = builder.buildMul(fact.parameters[0], recCall, "mul")
        builder.buildBr(done)

        builder.positionAtEnd(done)
        val phi = builder.buildPhi(i32, "result")
        phi.addIncoming(IRIntConstant(1, i32), entry)
        phi.addIncoming(mulResult, recurse)
        builder.buildRet(phi)

        // Create caller that calls fact
        val main = IRFunction("main", i32, listOf())
        module.functions.add(main)

        val mainEntry = IRBasicBlock("entry")
        main.basicBlocks.add(mainEntry)

        builder.positionAtEnd(mainEntry)
        val callResult = builder.buildCall(fact, listOf(IRIntConstant(5, i32)), "call")
        builder.buildRet(callResult)

        val pass = InliningPass(sizeThreshold = 100)
        val modified = pass.runOnModule(module)

        // Should not inline recursive function
        assertFalse(modified, "Recursive function should not be inlined")
    }

    @Test
    fun testInliningLargeFunctionNotInlined() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Create a large function (lots of instructions)
        val bigFn = IRFunction("big", i32, listOf(IRArgument(i32, "x")))
        module.functions.add(bigFn)

        val entry = IRBasicBlock("entry")
        bigFn.basicBlocks.add(entry)

        builder.positionAtEnd(entry)
        var value: IRValue = bigFn.parameters[0]
        // Add 60 instructions (above default threshold of 50)
        repeat(60) {
            value = builder.buildAdd(value, IRIntConstant(1, i32), "add$it")
        }
        builder.buildRet(value)

        // Create caller
        val main = IRFunction("main", i32, listOf())
        module.functions.add(main)

        val mainEntry = IRBasicBlock("entry")
        main.basicBlocks.add(mainEntry)

        builder.positionAtEnd(mainEntry)
        val callResult = builder.buildCall(bigFn, listOf(IRIntConstant(0, i32)), "call")
        builder.buildRet(callResult)

        val pass = InliningPass(sizeThreshold = 50)
        val modified = pass.runOnModule(module)

        // Should not inline large function
        assertFalse(modified, "Large function should not be inlined")

        // Call should still be present
        val calls = mainEntry.instructions.filterIsInstance<IRInstruction.Call>()
        assertEquals(1, calls.size, "Call should remain")
    }

    // ===========================================
    // PassManager Tests
    // ===========================================

    @Test
    fun testPassManagerChaining() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Create callee that uses alloca
        val addOne = IRFunction("addOne", i32, listOf(IRArgument(i32, "x")))
        module.functions.add(addOne)

        val addOneEntry = IRBasicBlock("entry")
        addOne.basicBlocks.add(addOneEntry)

        builder.positionAtEnd(addOneEntry)
        val alloca = builder.buildAlloca(i32, "tmp")
        builder.buildStore(addOne.parameters[0], alloca)
        val loaded = builder.buildLoad(i32, alloca, "val")
        val result = builder.buildAdd(loaded, IRIntConstant(1, i32), "res")
        builder.buildRet(result)

        // Create caller
        val main = IRFunction("main", i32, listOf())
        module.functions.add(main)

        val mainEntry = IRBasicBlock("entry")
        main.basicBlocks.add(mainEntry)

        builder.positionAtEnd(mainEntry)
        val callResult = builder.buildCall(addOne, listOf(IRIntConstant(5, i32)), "call")
        builder.buildRet(callResult)

        // Run both passes
        val passManager = PassManager()
        passManager.add(Mem2RegPass())
        passManager.add(InliningPass(sizeThreshold = 100))

        val modified = passManager.runOnModule(module)

        assertTrue(modified, "PassManager should modify the module")

        // Check that mem2reg ran on addOne
        val addOneAllocas = addOneEntry.instructions.filterIsInstance<IRInstruction.Alloca>()
        assertEquals(0, addOneAllocas.size, "Mem2Reg should have removed allocas from addOne")
    }

    @Test
    fun testPassManagerFixedPoint() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Simple function
        val fn = IRFunction("test", i32, listOf(IRArgument(i32, "x")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        fn.basicBlocks.add(entry)

        builder.positionAtEnd(entry)
        val alloca = builder.buildAlloca(i32, "tmp")
        builder.buildStore(fn.parameters[0], alloca)
        val loaded = builder.buildLoad(i32, alloca, "val")
        builder.buildRet(loaded)

        val passManager = PassManager()
        passManager.add(Mem2RegPass())

        // Run to fixed point - should converge after one iteration
        val modified = passManager.runToFixedPoint(module)

        assertTrue(modified, "Should modify at least once")

        // Verify allocas are gone
        val allocas = entry.instructions.filterIsInstance<IRInstruction.Alloca>()
        assertEquals(0, allocas.size)
    }

    // ===========================================
    // Use-Def Analysis Tests
    // ===========================================

    @Test
    fun testUseDefAnalysis() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        val fn = IRFunction("test", i32, listOf(IRArgument(i32, "x"), IRArgument(i32, "y")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        fn.basicBlocks.add(entry)

        builder.positionAtEnd(entry)
        val sum = builder.buildAdd(fn.parameters[0], fn.parameters[1], "sum")
        val doubled = builder.buildMul(sum, IRIntConstant(2, i32), "doubled")
        builder.buildRet(doubled)

        val useDefInfo = UseDefInfo.compute(fn)

        // Check uses of parameter x
        val xUses = useDefInfo.getUses(fn.parameters[0])
        assertEquals(1, xUses.size, "x should have one use")
        assertSame(xUses[0].instruction, sum, "x should be used in sum")

        // Check uses of sum
        val sumUses = useDefInfo.getUses(sum)
        assertEquals(1, sumUses.size, "sum should have one use")
        assertSame(sumUses[0].instruction, doubled, "sum should be used in doubled")

        // Check definition blocks
        assertEquals(entry, useDefInfo.getDefBlock(sum))
        assertEquals(entry, useDefInfo.getDefBlock(doubled))
        assertEquals(null, useDefInfo.getDefBlock(fn.parameters[0])) // Args have no def block
    }

    // ===========================================
    // Integration Tests
    // ===========================================

    @Test
    fun testFullOptimizationPipeline() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)
        val i32 = context.i32

        // Small helper function
        val square = IRFunction("square", i32, listOf(IRArgument(i32, "x")))
        module.functions.add(square)

        val squareEntry = IRBasicBlock("entry")
        square.basicBlocks.add(squareEntry)

        builder.positionAtEnd(squareEntry)
        val sqResult = builder.buildMul(square.parameters[0], square.parameters[0], "sq")
        builder.buildRet(sqResult)

        // Main function that calls square
        val main = IRFunction("main", i32, listOf())
        module.functions.add(main)

        val mainEntry = IRBasicBlock("entry")
        main.basicBlocks.add(mainEntry)

        builder.positionAtEnd(mainEntry)
        val alloca = builder.buildAlloca(i32, "tmp")
        builder.buildStore(IRIntConstant(5, i32), alloca)
        val loaded = builder.buildLoad(i32, alloca, "val")
        val callResult = builder.buildCall(square, listOf(loaded), "result")
        builder.buildRet(callResult)

        // Full optimization pipeline
        val passManager = PassManager(PassConfig(enableLogging = false))
        passManager.add(Mem2RegPass())
        passManager.add(InliningPass(sizeThreshold = 100))

        val modified = passManager.runOnModule(module)
        assertTrue(modified)

        // Verify some optimization effects:
        // 1. Main's entry should have no allocas after mem2reg
        val mainAllocas = mainEntry.instructions.filterIsInstance<IRInstruction.Alloca>()
        assertEquals(0, mainAllocas.size, "Main should have no allocas after optimization")
    }
}
