import llvm.IRArgument
import llvm.IRBasicBlock
import llvm.IRBuilder
import llvm.IRContext
import llvm.IRFunction
import llvm.IRGlobalVariable
import llvm.IRInstruction
import llvm.IRIntConstant
import llvm.IRPrinter
import llvm.IRType
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class LLVMTest {
    @Test
    fun testBasicTypes() {
        assertEquals("void", IRType.Void.toString())
        assertEquals("i1", IRType.Int(1).toString())
        assertEquals("i8", IRType.Int(8).toString())
        assertEquals("i32", IRType.Int(32).toString())
        assertEquals("i64", IRType.Int(64).toString())
        assertEquals("float", IRType.Float.toString())
        assertEquals("double", IRType.Double.toString())
        assertEquals("ptr", IRType.Pointer(IRType.Int(32)).toString())
        assertEquals("[10 x i32]", IRType.Array(10, IRType.Int(32)).toString())
    }

    @Test
    fun testFunctionType() {
        val i32 = IRType.Int(32)
        val fnType = IRType.Function(i32, listOf(i32, i32))
        assertEquals("i32 (i32, i32)", fnType.toString())

        val varArgFnType = IRType.Function(i32, listOf(i32), isVarArg = true)
        assertEquals("i32 (i32, ...)", varArgFnType.toString())
    }

    @Test
    fun testStructType() {
        val i32 = IRType.Int(32)
        val i8 = IRType.Int(8)
        val structType = IRType.Struct(null, listOf(i32, i8, i32))
        assertEquals("{ i32, i8, i32 }", structType.toString())

        val namedStructType = IRType.Struct("MyStruct", listOf(i32, i32))
        assertEquals("%MyStruct", namedStructType.toString())
    }

    @Test
    fun testIRBuilderBasic() {
        val context = IRContext()
        val module = context.createModule("test")
        val builder = IRBuilder(context, module)

        val i32 = IRType.Int(32)
        val fn = IRFunction("add", i32, listOf(IRArgument(i32, "a"), IRArgument(i32, "b")))
        module.functions.add(fn)

        val entry = IRBasicBlock("entry")
        fn.basicBlocks.add(entry)
        builder.positionAtEnd(entry)

        val sum = builder.buildAdd(fn.parameters[0], fn.parameters[1], "sum")
        builder.buildRet(sum)

        val printer = IRPrinter()
        val ir = printer.print(module)

        assertTrue(ir.contains("define i32 @add(i32 %a, i32 %b)"))
        assertTrue(ir.contains("entry:"))
        assertTrue(ir.contains("%sum.0 = add i32 %a, %b"))
        assertTrue(ir.contains("ret i32 %sum.0"))
    }

    @Test
    fun testComplexIR() {
        val context = IRContext()
        val module = context.createModule("complex")
        val builder = IRBuilder(context, module)

        val i32 = context.i32
        val mainFn = IRFunction("main", i32, listOf())
        module.functions.add(mainFn)

        val entry = IRBasicBlock("entry")
        val thenBlock = IRBasicBlock("then")
        val elseBlock = IRBasicBlock("else")
        val mergeBlock = IRBasicBlock("merge")

        mainFn.basicBlocks.addAll(listOf(entry, thenBlock, elseBlock, mergeBlock))

        builder.positionAtEnd(entry)
        val cond =
            builder.buildICmp(IRInstruction.ICmp.Predicate.SGT, IRIntConstant(10, i32), IRIntConstant(5, i32), "cond")
        builder.buildCondBr(cond, thenBlock, elseBlock)

        builder.positionAtEnd(thenBlock)
        val valThen = IRIntConstant(1, i32)
        builder.buildBr(mergeBlock)

        builder.positionAtEnd(elseBlock)
        val valElse = IRIntConstant(2, i32)
        builder.buildBr(mergeBlock)

        builder.positionAtEnd(mergeBlock)
        val phi = builder.buildPhi(i32, "result")
        phi.addIncoming(valThen, thenBlock)
        phi.addIncoming(valElse, elseBlock)
        builder.buildRet(phi)

        val ir = IRPrinter().print(module)

        assertTrue(ir.contains("br i1 %cond.0, label %then, label %else"))
        assertTrue(ir.contains("phi i32 [ 1, %then ], [ 2, %else ]"))
    }

    @Test
    fun testGlobalVariables() {
        val context = IRContext()
        val module = context.createModule("test")
        val i32 = context.i32

        val global = IRGlobalVariable("my_global", i32, IRIntConstant(42, i32))
        module.globals.add(global)

        val printer = IRPrinter()
        val ir = printer.print(module)

        assertTrue(ir.contains("@my_global = global i32 42"))
    }
}