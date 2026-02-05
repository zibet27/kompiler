import llvm.IRBasicBlock
import llvm.IRFunction
import llvm.IRInstruction
import llvm.IRIntConstant
import llvm.IRModule
import llvm.IRType
import wasm.WasmBackend
import kotlin.test.Test
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

/**
 * Unit tests for the Relooper - testing structured control flow generation
 */
class RelooperTest {

    @Test
    fun `test simple sequence - two blocks with unconditional branch`() {
        // Test: entry -> block2
        val function = IRFunction("test", IRType.Void, emptyList())

        val entry = IRBasicBlock("entry")
        val block2 = IRBasicBlock("block2")

        entry.instructions.add(IRInstruction.Br(block2))
        block2.instructions.add(IRInstruction.Ret(null))

        function.basicBlocks.add(entry)
        function.basicBlocks.add(block2)

        // Should compile without errors
        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test simple if-else - conditional branch to two blocks`() {
        // Test: entry -> (cond) ? then : else -> merge
        val function = IRFunction("test", IRType.Void, emptyList())

        val entry = IRBasicBlock("entry")
        val thenBlock = IRBasicBlock("then")
        val elseBlock = IRBasicBlock("else")
        val merge = IRBasicBlock("merge")

        val condition = IRIntConstant(1, IRType.Int(1))
        entry.instructions.add(IRInstruction.CondBr(condition, thenBlock, elseBlock))
        thenBlock.instructions.add(IRInstruction.Br(merge))
        elseBlock.instructions.add(IRInstruction.Br(merge))
        merge.instructions.add(IRInstruction.Ret(null))

        function.basicBlocks.add(entry)
        function.basicBlocks.add(thenBlock)
        function.basicBlocks.add(elseBlock)
        function.basicBlocks.add(merge)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test simple loop - back edge from body to header`() {
        // Test: header -> (cond) ? body : exit; body -> header
        val function = IRFunction("test", IRType.Void, emptyList())

        val header = IRBasicBlock("header")
        val body = IRBasicBlock("body")
        val exit = IRBasicBlock("exit")

        val condition = IRIntConstant(1, IRType.Int(1))
        header.instructions.add(IRInstruction.CondBr(condition, body, exit))
        body.instructions.add(IRInstruction.Br(header)) // back edge
        exit.instructions.add(IRInstruction.Ret(null))

        function.basicBlocks.add(header)
        function.basicBlocks.add(body)
        function.basicBlocks.add(exit)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test loop with break - exit from loop body`() {
        // Test: header -> (cond1) ? body : exit; body -> (cond2) ? exit : header
        val function = IRFunction("test", IRType.Void, emptyList())

        val header = IRBasicBlock("header")
        val body = IRBasicBlock("body")
        val exit = IRBasicBlock("exit")

        val cond1 = IRIntConstant(1, IRType.Int(1))
        val cond2 = IRIntConstant(0, IRType.Int(1))

        header.instructions.add(IRInstruction.CondBr(cond1, body, exit))
        body.instructions.add(IRInstruction.CondBr(cond2, exit, header))
        exit.instructions.add(IRInstruction.Ret(null))

        function.basicBlocks.add(header)
        function.basicBlocks.add(body)
        function.basicBlocks.add(exit)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test nested if - if inside another if`() {
        // Test: entry -> (c1) ? outer_then : outer_else
        //        outer_then -> (c2) ? inner_then : inner_else -> merge
        val function = IRFunction("test", IRType.Void, emptyList())

        val entry = IRBasicBlock("entry")
        val outerThen = IRBasicBlock("outer_then")
        val outerElse = IRBasicBlock("outer_else")
        val innerThen = IRBasicBlock("inner_then")
        val innerElse = IRBasicBlock("inner_else")
        val merge = IRBasicBlock("merge")

        val cond1 = IRIntConstant(1, IRType.Int(1))
        val cond2 = IRIntConstant(0, IRType.Int(1))

        entry.instructions.add(IRInstruction.CondBr(cond1, outerThen, outerElse))
        outerThen.instructions.add(IRInstruction.CondBr(cond2, innerThen, innerElse))
        innerThen.instructions.add(IRInstruction.Br(merge))
        innerElse.instructions.add(IRInstruction.Br(merge))
        outerElse.instructions.add(IRInstruction.Br(merge))
        merge.instructions.add(IRInstruction.Ret(null))

        function.basicBlocks.add(entry)
        function.basicBlocks.add(outerThen)
        function.basicBlocks.add(outerElse)
        function.basicBlocks.add(innerThen)
        function.basicBlocks.add(innerElse)
        function.basicBlocks.add(merge)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test switch-like pattern - multiple branches to same merge`() {
        // Test: entry -> (c1) ? b1 : (c2) ? b2 : b3 -> merge
        val function = IRFunction("test", IRType.Void, emptyList())

        val entry = IRBasicBlock("entry")
        val check2 = IRBasicBlock("check2")
        val b1 = IRBasicBlock("b1")
        val b2 = IRBasicBlock("b2")
        val b3 = IRBasicBlock("b3")
        val merge = IRBasicBlock("merge")

        val cond1 = IRIntConstant(1, IRType.Int(1))
        val cond2 = IRIntConstant(0, IRType.Int(1))

        entry.instructions.add(IRInstruction.CondBr(cond1, b1, check2))
        check2.instructions.add(IRInstruction.CondBr(cond2, b2, b3))
        b1.instructions.add(IRInstruction.Br(merge))
        b2.instructions.add(IRInstruction.Br(merge))
        b3.instructions.add(IRInstruction.Br(merge))
        merge.instructions.add(IRInstruction.Ret(null))

        function.basicBlocks.add(entry)
        function.basicBlocks.add(check2)
        function.basicBlocks.add(b1)
        function.basicBlocks.add(b2)
        function.basicBlocks.add(b3)
        function.basicBlocks.add(merge)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test function with return value - i32 return`() {
        // Test: simple function that returns a constant
        val function = IRFunction("test", IRType.Int(32), emptyList())

        val entry = IRBasicBlock("entry")
        val retValue = IRIntConstant(42, IRType.Int(32))
        entry.instructions.add(IRInstruction.Ret(retValue))

        function.basicBlocks.add(entry)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    @Test
    fun `test loop with return value - return after loop`() {
        // Test: loop that exits, then returns a value
        // header -> (cond) ? body : exit; body -> header; exit -> ret 0
        val function = IRFunction("test", IRType.Int(32), emptyList())

        val header = IRBasicBlock("header")
        val body = IRBasicBlock("body")
        val exit = IRBasicBlock("exit")

        val condition = IRIntConstant(0, IRType.Int(1)) // false -> exit immediately
        header.instructions.add(IRInstruction.CondBr(condition, body, exit))
        body.instructions.add(IRInstruction.Br(header))

        val retValue = IRIntConstant(0, IRType.Int(32))
        exit.instructions.add(IRInstruction.Ret(retValue))

        function.basicBlocks.add(header)
        function.basicBlocks.add(body)
        function.basicBlocks.add(exit)

        val wasm = compileFunction(function)
        assertNotNull(wasm)
        assertTrue(wasm.isNotEmpty())
    }

    /**
     * Helper function to compile a function to WebAssembly bytecode
     */
    private fun compileFunction(function: IRFunction): ByteArray {
        val module = IRModule("test")
        module.functions.add(function)

        val backend = WasmBackend()
        return backend.compile(module)
    }
}