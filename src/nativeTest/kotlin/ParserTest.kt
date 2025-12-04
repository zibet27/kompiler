package test

import ast.AlienFunDecl
import ast.Assign
import ast.AssignInit
import ast.Binary
import ast.BinaryOp
import ast.BlockExpr
import ast.BuiltinType
import ast.Call
import ast.Cast
import ast.CharLit
import ast.DoWhileExpr
import ast.ExprStmt
import ast.F64Lit
import ast.ForExpr
import ast.FunDecl
import ast.FunDef
import ast.GlobalVarDecl
import ast.Ident
import ast.IfExpr
import ast.Index
import ast.IntLit
import ast.LocalVarDecl
import ast.Member
import ast.NamedType
import ast.ObjectDecl
import ast.ObjectDef
import ast.PointerType
import ast.PostfixDec
import ast.PostfixInc
import ast.Program
import ast.SkipStmt
import ast.StopStmt
import ast.StringLit
import ast.SwitchExpr
import ast.TypeAlias
import ast.Unary
import ast.UnaryOp
import ast.WhileExpr
import ast.WithInit
import kotlin.test.*
import parser.KodeParser

class ParserTest {
    private val parser = KodeParser()

    private fun parse(source: String): Program = parser.parse(source)

    @Test
    fun emptyProgram() {
        val prog = parse("")
        assertEquals(0, prog.decls.size)
    }

    @Test
    fun simpleFunctionDeclaration() {
        val prog = parse("fun add{ a: i32, b: i32 }: i32;")
        assertEquals(1, prog.decls.size)
        val decl = prog.decls[0] as FunDecl
        assertEquals("add", decl.name)
        assertEquals(2, decl.params.size)
        assertTrue(decl.returnType is BuiltinType)
    }

    @Test
    fun functionDefinition() {
        val prog = parse("fun add{ a: i32, b: i32 }: i32 ( a + b; );")
        assertEquals(1, prog.decls.size)
        val def = prog.decls[0] as FunDef
        assertEquals("add", def.name)
        assertEquals(2, def.params.size)
        assertTrue(def.body.items.isNotEmpty())
    }

    @Test
    fun alienFunctionDeclaration() {
        val prog = parse("alien fun print_int{ x: i32 };")
        assertEquals(1, prog.decls.size)
        val alien = prog.decls[0] as AlienFunDecl
        assertEquals("print_int", alien.name)
        assertEquals(1, alien.params.size)
    }

    @Test
    fun objectDeclaration() {
        val prog = parse("object Point;")
        assertEquals(1, prog.decls.size)
        val obj = prog.decls[0] as ObjectDecl
        assertEquals("Point", obj.name)
    }

    @Test
    fun objectDefinition() {
        val prog = parse("object Point ( x: i32; y: i32; );")
        assertEquals(1, prog.decls.size)
        val obj = prog.decls[0] as ObjectDef
        assertEquals("Point", obj.name)
        assertEquals(2, obj.fields.size)
        assertEquals("x", obj.fields[0].name)
        assertEquals("y", obj.fields[1].name)
    }

    @Test
    fun typeAlias() {
        val prog = parse("typealias BinaryOp = { i32, i32 } -> i32;")
        assertEquals(1, prog.decls.size)
        val alias = prog.decls[0] as TypeAlias
        assertEquals("BinaryOp", alias.name)
        assertEquals(2, alias.paramTypes.size)
    }

    @Test
    fun globalVariableDeclaration() {
        val prog = parse("i32 global_var = 10;")
        assertEquals(1, prog.decls.size)
        val global = prog.decls[0] as GlobalVarDecl
        assertTrue(global.type is BuiltinType)
        assertEquals(1, global.declarators.size)
        assertEquals("global_var", global.declarators[0].name)
    }

    @Test
    fun globalArrayDeclaration() {
        val prog = parse("u8 global_array[10] with ' ';")
        assertEquals(1, prog.decls.size)
        val global = prog.decls[0] as GlobalVarDecl
        val declarator = global.declarators[0]
        assertEquals("global_array", declarator.name)
        assertEquals(1, declarator.arrayDims.size)
        assertTrue(declarator.init is WithInit)
    }

    @Test
    fun multipleGlobalVariables() {
        val prog = parse("i32 x = 1, y = 2, z = 3;")
        assertEquals(1, prog.decls.size)
        val global = prog.decls[0] as GlobalVarDecl
        assertEquals(3, global.declarators.size)
    }

    // === Types ===

    @Test
    fun builtinTypes() {
        parse("i32 a;")
        parse("u8 b;")
        parse("f64 c;")
        parse("void ptr d;")
    }

    @Test
    fun pointerTypes() {
        val prog = parse("i32 ptr p; i32 ptr ptr pp;")
        assertEquals(2, prog.decls.size)
        val p1 = (prog.decls[0] as GlobalVarDecl).type as PointerType
        assertEquals(1, p1.levels)
        val p2 = (prog.decls[1] as GlobalVarDecl).type as PointerType
        assertEquals(2, p2.levels)
    }

    @Test
    fun namedTypes() {
        val prog = parse("object Point; Point p;")
        val decl = prog.decls[1] as GlobalVarDecl
        assertTrue(decl.type is NamedType)
        assertEquals("Point", decl.type.name)
    }

    @Test
    fun functionTypes() {
        val prog = parse("typealias Callback = { i32, i32 } -> void;")
        val alias = prog.decls[0] as TypeAlias
        assertEquals(2, alias.paramTypes.size)
    }

    // === Expressions ===

    @Test
    fun integerLiteral() {
        val prog = parse("fun main{}: i32 ( 42; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        val lit = stmt.expr as IntLit
        assertEquals(42, lit.value)
    }

    @Test
    fun floatLiteral() {
        val prog = parse("fun main{}: i32 ( 3.14; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        assertTrue(stmt.expr is F64Lit)
    }

    @Test
    fun charLiteral() {
        val prog = parse("fun main{}: i32 ( 'a'; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        assertTrue(stmt.expr is CharLit)
    }

    @Test
    fun stringLiteral() {
        val prog = parse("fun main{}: i32 ( \"hello\"; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        assertTrue(stmt.expr is StringLit)
    }

    @Test
    fun identifier() {
        val prog = parse("fun main{}: i32 ( i32 x = 5; x; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        val ident = stmt.expr as Ident
        assertEquals("x", ident.name)
    }

    @Test
    fun arithmeticOperators() {
        parse("fun main{}: i32 ( i32 a = 1 + 2; );")
        parse("fun main{}: i32 ( i32 a = 3 - 1; );")
        parse("fun main{}: i32 ( i32 a = 2 * 3; );")
        parse("fun main{}: i32 ( i32 a = 6 / 2; );")
        parse("fun main{}: i32 ( i32 a = 7 % 3; );")
    }

    @Test
    fun bitwiseOperators() {
        parse("fun main{}: i32 ( i32 a = 5 & 3; );")
        parse("fun main{}: i32 ( i32 a = 5 | 3; );")
        parse("fun main{}: i32 ( i32 a = 5 ^ 3; );")
        parse("fun main{}: i32 ( i32 a = ~5; );")
        parse("fun main{}: i32 ( i32 a = 5 << 1; );")
        parse("fun main{}: i32 ( i32 a = 5 >> 1; );")
    }

    @Test
    fun logicalOperators() {
        parse("fun main{}: i32 ( i32 a = { 1 > 0 } && { 2 > 0 }; );")
        parse("fun main{}: i32 ( i32 a = { 1 > 0 } || { 0 > 2 }; );")
        parse("fun main{}: i32 ( i32 a = !{ 1 == 2 }; );")
    }

    @Test
    fun comparisonOperators() {
        parse("fun main{}: i32 ( i32 a = { 1 == 2 }; );")
        parse("fun main{}: i32 ( i32 a = { 1 != 2 }; );")
        parse("fun main{}: i32 ( i32 a = { 1 < 2 }; );")
        parse("fun main{}: i32 ( i32 a = { 1 > 2 }; );")
        parse("fun main{}: i32 ( i32 a = { 1 <= 2 }; );")
        parse("fun main{}: i32 ( i32 a = { 1 >= 2 }; );")
    }

    @Test
    fun unaryOperators() {
        parse("fun main{}: i32 ( i32 a = +5; );")
        parse("fun main{}: i32 ( i32 a = -5; );")
        parse("fun main{}: i32 ( i32 a = !0; );")
        parse("fun main{}: i32 ( i32 a = ~5; );")
    }

    @Test
    fun prefixIncrementDecrement() {
        val prog = parse("fun main{}: i32 ( i32 a = 5; i32 b = ++a; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[1] as LocalVarDecl
        val init = stmt.declarators[0].init as AssignInit
        assertTrue(init.expr is Unary)
        assertEquals(UnaryOp.PreInc, init.expr.op)
    }

    @Test
    fun postfixIncrementDecrement() {
        val prog = parse("fun main{}: i32 ( i32 a = 5; a++; a--; );")
        val def = prog.decls[0] as FunDef
        val stmt1 = def.body.items[1] as ExprStmt
        val stmt2 = def.body.items[2] as ExprStmt
        assertTrue(stmt1.expr is PostfixInc)
        assertTrue(stmt2.expr is PostfixDec)
    }

    @Test
    fun pointerOperations() {
        val prog = parse("fun main{}: i32 ( i32 a = 5; i32 ptr p = &a; i32 b = *p; );")
        val def = prog.decls[0] as FunDef
        val init1 = (def.body.items[1] as LocalVarDecl).declarators[0].init as AssignInit
        assertTrue(init1.expr is Unary)
        assertEquals(UnaryOp.AddressOf, init1.expr.op)
        val init2 = (def.body.items[2] as LocalVarDecl).declarators[0].init as AssignInit
        assertTrue(init2.expr is Unary)
        assertEquals(UnaryOp.Deref, init2.expr.op)
    }

    @Test
    fun functionCall() {
        val prog = parse("fun add{ a: i32, b: i32 }: i32 ( a + b; ); fun main{}: i32 ( add{ 1, 2 }; );")
        val def = prog.decls[1] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        val call = stmt.expr as Call
        assertEquals(2, call.args.size)
    }

    @Test
    fun arrayIndexing() {
        val prog = parse("fun main{}: i32 ( i32 arr[5]; arr[0] = 1; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        val assign = stmt.expr as Assign
        assertTrue(assign.target is Index)
    }

    @Test
    fun memberAccess() {
        val prog = parse("object Point ( x: i32; ); fun main{}: i32 ( Point p; p.x = 5; );")
        val def = prog.decls[1] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        val assign = stmt.expr as Assign
        val member = assign.target as Member
        assertFalse(member.viaArrow)
        assertEquals("x", member.name)
    }

    @Test
    fun arrowMemberAccess() {
        val prog = parse("object Point ( x: i32; ); fun main{}: i32 ( Point ptr p; p->x = 5; );")
        val def = prog.decls[1] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        val assign = stmt.expr as Assign
        val member = assign.target as Member
        assertTrue(member.viaArrow)
        assertEquals("x", member.name)
    }

    @Test
    fun assignment() {
        val prog = parse("fun main{}: i32 ( i32 a = 5; a = 10; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        assertTrue(stmt.expr is Assign)
    }

    @Test
    fun casting() {
        val prog = parse("fun main{}: i32 ( f64 d = 3.14; i32 i = d ~> i32; );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[1] as LocalVarDecl
        val init = decl.declarators[0].init as AssignInit
        val cast = init.expr as Cast
        assertEquals("d", cast.ident)
        assertTrue(cast.type is BuiltinType)
    }

    @Test
    fun groupedExpression() {
        val prog = parse("fun main{}: i32 ( i32 a = { 1 + 2 } * 3; );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        val init = decl.declarators[0].init as AssignInit
        assertTrue(init.expr is Binary)
    }

    @Test
    fun complexExpression() {
        parse("fun main{}: i32 ( i32 result = {{5 + 3} * 2} / {4 - 1} + 7; );")
    }

    // === Control Flow ===

    @Test
    fun ifExpression() {
        val prog = parse("fun main{}: i32 ( if { 1 > 0 } ( 42; ); );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        val ifExpr = stmt.expr as IfExpr
        assertTrue(ifExpr.cond is Binary)
        assertTrue(ifExpr.thenBlock.items.isNotEmpty())
        assertTrue(ifExpr.elseBlock.items.isEmpty())
    }

    @Test
    fun ifElseExpression() {
        val prog = parse("fun main{}: i32 ( i32 x = if { 1 > 0 } ( 42; ) else ( 0; ); );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        val init = decl.declarators[0].init as AssignInit
        val ifExpr = init.expr as IfExpr
        assertTrue(ifExpr.elseBlock.items.isNotEmpty())
    }

    @Test
    fun ifElseIfExpression() {
        parse("fun main{}: i32 ( if { 1 > 2 } ( 1; ) else if { 2 > 3 } ( 2; ) else ( 3; ); );")
    }

    @Test
    fun whileLoop() {
        val prog = parse("fun main{}: i32 ( i32 i = 0; while { i < 10 } ( i++; ); );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        val whileExpr = stmt.expr as WhileExpr
        assertTrue(whileExpr.cond is Binary)
        assertTrue(whileExpr.body.items.isNotEmpty())
    }

    @Test
    fun doWhileLoop() {
        val prog = parse("fun main{}: i32 ( i32 i = 0; do ( i++; ) while { i < 10 }; );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[1] as ExprStmt
        val doWhileExpr = stmt.expr as DoWhileExpr
        assertTrue(doWhileExpr.cond is Binary)
        assertTrue(doWhileExpr.body.items.isNotEmpty())
    }

    @Test
    fun forLoop() {
        val prog = parse("fun main{}: i32 ( for { i32 i = 0; i < 10; i++ } ( i; ); );")
        val def = prog.decls[0] as FunDef
        val stmt = def.body.items[0] as ExprStmt
        val forExpr = stmt.expr as ForExpr
        assertTrue(forExpr.init is LocalVarDecl)
        assertTrue(forExpr.cond is Binary)
        assertTrue(forExpr.incr is PostfixInc)
    }

    @Test
    fun forLoopWithExprInit() {
        parse("fun main{}: i32 ( i32 i; for { i = 0; i < 10; i++ } ( i; ); );")
    }

    @Test
    fun switchExpression() {
        val prog = parse("fun main{}: i32 ( i32 x = switch { 3 } ( 1 -> 10; 2 -> 20; 3 -> 30; else -> 0; ); );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        val init = decl.declarators[0].init as AssignInit
        val switchExpr = init.expr as SwitchExpr
        assertEquals(3, switchExpr.cases.size)
        assertNotNull(switchExpr.defaultCase)
    }

    @Test
    fun switchWithoutDefault() {
        val prog = parse("fun main{}: i32 ( i32 x = switch { 3 } ( 1 -> 10; 2 -> 20; ); );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        val init = decl.declarators[0].init as AssignInit
        val switchExpr = init.expr as SwitchExpr
        assertEquals(2, switchExpr.cases.size)
    }

    // === Statements ===

    @Test
    fun skipStatement() {
        val prog = parse("fun main{}: i32 ( skip; );")
        val def = prog.decls[0] as FunDef
        assertTrue(def.body.items[0] is SkipStmt)
    }

    @Test
    fun stopStatement() {
        val prog = parse("fun main{}: i32 ( stop; );")
        val def = prog.decls[0] as FunDef
        assertTrue(def.body.items[0] is StopStmt)
    }

    @Test
    fun blockExpression() {
        val prog = parse("fun main{}: i32 ( i32 x = ( i32 a = 5; a + 10; ); );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        val init = decl.declarators[0].init as AssignInit
        assertTrue(init.expr is BlockExpr)
    }

    @Test
    fun nestedBlocks() {
        parse("fun main{}: i32 ( i32 x = 10; ( i32 y = 20; ( i32 z = 30; ); ); );")
    }

    @Test
    fun localVariableDeclaration() {
        val prog = parse("fun main{}: i32 ( i32 a; );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        assertEquals("a", decl.declarators[0].name)
    }

    @Test
    fun localVariableWithInitialization() {
        val prog = parse("fun main{}: i32 ( i32 a = 5; );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        assertNotNull(decl.declarators[0].init)
    }

    @Test
    fun localArrayDeclaration() {
        val prog = parse("fun main{}: i32 ( i32 arr[10] with 0; );")
        val def = prog.decls[0] as FunDef
        val decl = def.body.items[0] as LocalVarDecl
        val declarator = decl.declarators[0]
        assertEquals(1, declarator.arrayDims.size)
        assertTrue(declarator.init is WithInit)
    }

    @Test
    fun helloWorld() {
        parse("alien fun print_string{ s: u8 ptr }; fun main{}: i32 ( print_string{ \"Hello, World!\" }; 0; );")
    }

    @Test
    fun variablesExample() {
        parse("""
            alien fun print_int{ x: i32 };
            i32 global_var = 10;
            fun main{}: i32 ( i32 a; i32 b = 5; a = b * 2; print_int{ a }; 0; );
        """.trimIndent())
    }

    @Test
    fun functionPointers() {
        val prog = parse("""
            typealias BinaryOperation = { i32, i32 } -> i32;
            fun add{ a: i32, b: i32 }: i32 ( a + b; );
            fun main{}: i32 ( BinaryOperation op = add; i32 result = op{ 5, 3 }; 0; );
        """.trimIndent())

        assertEquals(3, prog.decls.size)

        // Check typealias
        val alias = prog.decls[0] as TypeAlias
        assertEquals("BinaryOperation", alias.name)
        assertEquals(2, alias.paramTypes.size)
        assertTrue(alias.returnType is BuiltinType)

        // Check function definition
        val addFun = prog.decls[1] as FunDef
        assertEquals("add", addFun.name)
        assertEquals(2, addFun.params.size)

        // Check the main function with function pointer usage
        val mainFun = prog.decls[2] as FunDef
        val opDecl = mainFun.body.items[0] as LocalVarDecl
        assertEquals("BinaryOperation", (opDecl.type as NamedType).name)

        // Check function call through a pointer
        val resultDecl = mainFun.body.items[1] as LocalVarDecl
        val resultInit = resultDecl.declarators[0].init as AssignInit
        assertTrue(resultInit.expr is Call)
    }

    @Test
    fun structsExample() {
        val prog = parse("""
            object Point ( x: i32; y: i32; );
            fun main{}: i32 ( Point p; p.x = 10; p.y = 20; 0; );
        """.trimIndent())

        assertEquals(2, prog.decls.size)

        // Check object definition
        val pointObj = prog.decls[0] as ObjectDef
        assertEquals("Point", pointObj.name)
        assertEquals(2, pointObj.fields.size)
        assertEquals("x", pointObj.fields[0].name)
        assertEquals("y", pointObj.fields[1].name)

        // Check struct usage in the main
        val mainFun = prog.decls[1] as FunDef
        val pDecl = mainFun.body.items[0] as LocalVarDecl
        assertTrue(pDecl.type is NamedType)
        assertEquals("Point", pDecl.type.name)

        // Check member assignments
        val xAssign = (mainFun.body.items[1] as ExprStmt).expr as Assign
        assertTrue(xAssign.target is Member)
        assertEquals("x", xAssign.target.name)

        val yAssign = (mainFun.body.items[2] as ExprStmt).expr as Assign
        assertTrue(yAssign.target is Member)
        assertEquals("y", yAssign.target.name)
    }

    @Test
    fun pointersExample() {
        val prog = parse("""
            fun swap{ a: i32 ptr, b: i32 ptr }: void ( i32 temp = *a; *a = *b; *b = temp; );
            fun main{}: i32 ( i32 x = 5; i32 y = 10; swap{ &x, &y }; 0; );
        """.trimIndent())

        assertEquals(2, prog.decls.size)

        // Check swap function signature
        val swapFun = prog.decls[0] as FunDef
        assertEquals("swap", swapFun.name)
        assertEquals(2, swapFun.params.size)
        assertTrue(swapFun.params[0].type is PointerType)
        assertTrue(swapFun.returnType is BuiltinType)
        assertEquals(BuiltinType.Kind.Void, swapFun.returnType.kind)

        // Check pointer dereference operations in the swap body
        val tempDecl = swapFun.body.items[0] as LocalVarDecl
        val tempInit = tempDecl.declarators[0].init as AssignInit
        assertTrue(tempInit.expr is Unary)
        assertEquals(UnaryOp.Deref, tempInit.expr.op)

        // Check the main function with address-of operations
        val mainFun = prog.decls[1] as FunDef
        val swapCall = (mainFun.body.items[2] as ExprStmt).expr as Call
        assertEquals(2, swapCall.args.size)
        assertTrue(swapCall.args[0] is Unary)
        assertEquals(UnaryOp.AddressOf, (swapCall.args[0] as Unary).op)
        assertTrue(swapCall.args[1] is Unary)
        assertEquals(UnaryOp.AddressOf, (swapCall.args[1] as Unary).op)
    }

    @Test
    fun operatorPrecedence() {
        // Test: 2 + 3 * 4 should parse as 2 + (3 * 4) = 14
        val prog1 = parse("fun main{}: i32 ( i32 result = 2 + 3 * 4; );")
        val def1 = prog1.decls[0] as FunDef
        val decl1 = def1.body.items[0] as LocalVarDecl
        val expr1 = (decl1.declarators[0].init as AssignInit).expr as Binary
        assertEquals(BinaryOp.Add, expr1.op)
        assertTrue(expr1.right is Binary)
        assertEquals(BinaryOp.Mul, expr1.right.op)

        // Test: { 2 + 3 } * 4 should parse as (2 + 3) * 4 = 20
        val prog2 = parse("fun main{}: i32 ( i32 result = { 2 + 3 } * 4; );")
        val def2 = prog2.decls[0] as FunDef
        val decl2 = def2.body.items[0] as LocalVarDecl
        val expr2 = (decl2.declarators[0].init as AssignInit).expr as Binary
        assertEquals(BinaryOp.Mul, expr2.op)
        assertTrue(expr2.left is Binary)
        assertEquals(BinaryOp.Add, expr2.left.op)

        // Test: 2 * 3 + 4 * 5 should parse as (2 * 3) + (4 * 5) = 26
        val prog3 = parse("fun main{}: i32 ( i32 result = 2 * 3 + 4 * 5; );")
        val def3 = prog3.decls[0] as FunDef
        val decl3 = def3.body.items[0] as LocalVarDecl
        val expr3 = (decl3.declarators[0].init as AssignInit).expr as Binary
        assertEquals(BinaryOp.Add, expr3.op)
        assertTrue(expr3.left is Binary)
        assertTrue(expr3.right is Binary)
        assertEquals(BinaryOp.Mul, expr3.left.op)
        assertEquals(BinaryOp.Mul, expr3.right.op)
    }

    @Test
    fun complexNestedExpressions() {
        val prog = parse("""
            fun add{ a: i32, b: i32 }: i32 ( a + b; );
            fun main{}: i32 (
                i32 arr[5];
                i32 result = add{ 5 * 2, 10 / 2 } + { 15 % 7 };
                arr[ add{ 1, 2 } ] = 42;
                0;
            );
        """.trimIndent())

        assertEquals(2, prog.decls.size)

        val mainFun = prog.decls[1] as FunDef
        assertEquals(4, mainFun.body.items.size)

        // Check array declaration
        val arrDecl = mainFun.body.items[0] as LocalVarDecl
        assertEquals("arr", arrDecl.declarators[0].name)
        assertEquals(1, arrDecl.declarators[0].arrayDims.size)

        // Check a complex expression: add{ 5 * 2, 10 / 2 } + { 15 % 7 }
        val resultDecl = mainFun.body.items[1] as LocalVarDecl
        val resultExpr = (resultDecl.declarators[0].init as AssignInit).expr as Binary
        assertEquals(BinaryOp.Add, resultExpr.op)

        // Left side: function call with arithmetic args
        val callExpr = resultExpr.left as Call
        assertEquals(2, callExpr.args.size)
        assertTrue(callExpr.args[0] is Binary)
        assertEquals(BinaryOp.Mul, (callExpr.args[0] as Binary).op)
        assertTrue(callExpr.args[1] is Binary)
        assertEquals(BinaryOp.Div, (callExpr.args[1] as Binary).op)

        // Right side: modulo in grouped expression
        val groupedExpr = resultExpr.right as Binary
        assertEquals(BinaryOp.Mod, groupedExpr.op)

        // Check array indexing with function call: arr [ add{ 1, 2 } ] = 42
        val arrAssign = (mainFun.body.items[2] as ExprStmt).expr as Assign
        val indexExpr = arrAssign.target as Index
        assertTrue(indexExpr.target is Ident)
        assertEquals("arr", indexExpr.target.name)
        assertTrue(indexExpr.index is Call)
        assertEquals(2, indexExpr.index.args.size)
    }
}
