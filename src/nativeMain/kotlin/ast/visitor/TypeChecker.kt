package ast.visitor

import ast.*
import lexer.Span

data class Diagnostic(val message: String, val span: Span) {
    override fun toString(): String = "Type error. $message at $span"
}

class Scope(val parent: Scope? = null) {
    val vars = mutableMapOf<String, Type>()

    operator fun get(name: String): Type? = vars[name] ?: parent?.get(name)
}

class TypeChecker(
    private val program: Program
) : AstVisitor<Type> {

    val diagnostics: MutableList<Diagnostic> = mutableListOf()

    // Environments
    private val functions = mutableMapOf<String, Type.Fn>()
    private val globals = mutableMapOf<String, Type>()
    private val types = mutableMapOf<String, Type>() // named types: objects, aliases, etc.

    private var scope: Scope = Scope()
    private var currentRet: Type = Type.Void

    private fun report(span: Span, msg: String) {
        diagnostics += Diagnostic(msg, span)
    }

    private fun TypeRef.resolve(): Type = when (this) {
        is BuiltinType -> when (this.kind) {
            BuiltinType.Kind.I32 -> Type.I32
            BuiltinType.Kind.U8 -> Type.U8
            BuiltinType.Kind.F64 -> Type.F64
            BuiltinType.Kind.Void -> Type.Void
        }

        is NamedType -> types[name] ?: globals[name] ?: functions[name] ?: Type.Unknown
        is PointerType -> Type.Ptr(base.resolve(), levels)
        is FuncType -> Type.Fn(paramTypes.map { it.resolve() }, returnType.resolve())
    }

    private fun unify(a: Type, b: Type, span: Span): Type = when {
        a.canImplicitlyCastTo(b) -> b
        b.canImplicitlyCastTo(a) -> a
        else -> {
            report(span, "Type mismatch: $a vs $b")
            Type.Unknown
        }
    }

    private fun blockResultType(block: Block): Type {
        // The value of a block is the value of its last expression statement, if any; otherwise void.
        var lastType: Type = Type.Void
        withScope {
            block.items.forEach {
                lastType = it.accept(this@TypeChecker)
            }
        }
        return lastType
    }

    private inline fun <T> withScope(block: () -> T): T {
        val parent = scope
        scope = Scope(parent)
        return try {
            block()
        } finally {
            scope = parent
        }
    }

    init {
        for (d in program.decls) when (d) {
            is FunDecl -> functions[d.name] = Type.Fn(d.params.map { it.type.resolve() }, d.returnType.resolve())
            is FunDef -> functions[d.name] = Type.Fn(d.params.map { it.type.resolve() }, d.returnType.resolve())
            is GlobalVarDecl -> d.declarators.forEach { globals[it.name] = d.type.resolve() }
            is ObjectDef -> {
                // collect object type with resolved fields
                val fieldMap = d.fields.associate { it.name to it.type.resolve() }
                types[d.name] = Type.Obj(d.name, fieldMap, isComplete = true)
            }

            is ObjectDecl -> {
                // forward declaration: placeholder incomplete object
                if (!types.containsKey(d.name)) {
                    types[d.name] = Type.Obj(d.name, emptyMap(), isComplete = false)
                }
            }

            is TypeAlias -> types[d.name] = Type.Fn(
                params = d.paramTypes.map { it.resolve() }, ret = d.returnType.resolve()
            )

            is AlienFunDecl -> functions[d.name] = Type.Fn(
                params = d.params.map { it.type.resolve() }, ret = d.returnType.resolve()
            )
        }
    }

    fun check(): List<Diagnostic> {
        program.accept(this)
        return diagnostics
    }

    // Visitor impl
    override fun visit(node: Program): Type {
        node.decls.forEach { it.accept(this) }
        return Type.Void
    }

    override fun visit(node: FunDecl): Type = functions[node.name] ?: Type.Unknown

    override fun visit(node: FunDef): Type {
        val fn = functions[node.name] ?: Type.Fn(node.params.map { it.type.resolve() }, node.returnType.resolve())
        withScope {
            node.params.forEach { scope.vars[it.name] = it.type.resolve() }
            val prevRet = currentRet
            currentRet = fn.ret
            node.body.accept(this)
            currentRet = prevRet
        }
        return fn
    }

    override fun visit(node: AlienFunDecl): Type = Type.Void

    override fun visit(node: ObjectDecl): Type = Type.Void
    override fun visit(node: ObjectDef): Type {
        val existing = types[node.name]
        val fieldMap = node.fields.associate { it.name to it.type.resolve() }
        val def = Type.Obj(node.name, fieldMap, isComplete = true)
        if (existing is Type.Obj && existing.isComplete && existing != def) {
            report(node.span, "Conflicting redefinition of object ${node.name}")
        }
        types[node.name] = def
        return Type.Void
    }

    override fun visit(node: TypeAlias): Type = Type.Void

    override fun visit(node: GlobalVarDecl): Type {
        val t = node.type.resolve()
        node.declarators.forEach { scope.vars[it.name] = t }
        return Type.Void
    }

    override fun visit(node: FieldDecl): Type = node.type.resolve()
    override fun visit(node: Param): Type = node.type.resolve()
    override fun visit(node: Declarator): Type = node.init.accept(this)

    override fun visit(node: WithInit): Type = node.expr.accept(this)
    override fun visit(node: AssignInit): Type = node.expr.accept(this)

    override fun visit(node: BuiltinType): Type = node.resolve()
    override fun visit(node: NamedType): Type = node.resolve()
    override fun visit(node: PointerType): Type = node.resolve()
    override fun visit(node: FuncType): Type = node.resolve()

    override fun visit(node: Block): Type = blockResultType(node)

    override fun visit(node: LocalVarDecl): Type {
        val baseType = node.type.resolve()
        node.declarators.forEach { dec ->
            scope.vars[dec.name] = when (dec.arrayDims.size) {
                0 -> baseType
                else -> Type.Ptr(baseType, levels = dec.arrayDims.size)
            }
            val srcType = dec.init.accept(this)
            if (!srcType.canImplicitlyCastTo(other = baseType)) {
                report(dec.init.span, "Cannot initialize '${dec.name}': $srcType is not assignable to $baseType")
            }
        }
        return Type.Void
    }

    override fun visit(node: ExprStmt): Type = node.expr.accept(this)
    override fun visit(node: SkipStmt): Type = Type.Void
    override fun visit(node: StopStmt): Type = Type.Void

    override fun visit(node: IntLit): Type = Type.I32
    override fun visit(node: FloatLit): Type = Type.F64
    override fun visit(node: CharLit): Type = Type.U8
    override fun visit(node: StringLit): Type = Type.Ptr(Type.U8, 1)

    override fun visit(node: Ident): Type = scope[node.name] ?: globals[node.name] ?: functions[node.name] ?: run {
        report(node.span, "Unresolved identifier '${node.name}'")
        Type.Unknown
    }

    override fun visit(node: Unary): Type {
        val t = node.expr.accept(this)
        return when (node.op) {
            UnaryOp.Not, UnaryOp.BitNot, UnaryOp.Plus, UnaryOp.Minus, UnaryOp.PreInc, UnaryOp.PreDec -> when (t) {
                is Type.F64, is Type.I32, is Type.U8 -> t
                is Type.Unknown -> t
                else -> report(node.span, "Unary ${node.op} not applicable to $t").let { Type.Unknown }
            }

            UnaryOp.Deref -> when (t) {
                is Type.Ptr -> when (t.levels) {
                    1 -> t.base
                    else -> Type.Ptr(t.base, levels = 1)
                }

                else -> report(node.span, "Cannot dereference $t").let { Type.Unknown }
            }

            UnaryOp.AddressOf -> when (t) {
                is Type.Ptr -> Type.Ptr(t.base, levels = t.levels + 1)
                else -> Type.Ptr(base = t, levels = 1)
            }
        }
    }

    override fun visit(node: Binary): Type {
        val lt = node.left.accept(this)
        val rt = node.right.accept(this)
        if (lt != rt || lt !in listOf(Type.I32, Type.U8, Type.F64)) {
            report(node.span, "Incompatible numeric types: $lt and $rt")
            return Type.Unknown
        }
        return when (node.op) {
            BinaryOp.Add, BinaryOp.Sub, BinaryOp.Mul, BinaryOp.Div, BinaryOp.Mod, BinaryOp.Shl, BinaryOp.Shr, BinaryOp.BitAnd, BinaryOp.BitOr, BinaryOp.BitXor -> lt
            BinaryOp.Eq, BinaryOp.Ne, BinaryOp.Lt, BinaryOp.Gt, BinaryOp.Le, BinaryOp.Ge -> Type.I32
            BinaryOp.OrOr, BinaryOp.AndAnd -> Type.I32
        }
    }

    override fun visit(node: Call): Type = when (val calleeT = node.callee.accept(this)) {
        is Type.Fn -> {
            if (calleeT.params.size != node.args.size) {
                report(node.span, "Argument count mismatch: expected ${calleeT.params.size}, got ${node.args.size}")
            } else {
                node.args.zip(calleeT.params).forEach { (argExpr, paramT) ->
                    val argType = argExpr.accept(this)
                    if (!argType.canImplicitlyCastTo(paramT)) {
                        report(argExpr.span, "Argument type $argType is not assignable to $paramT")
                    }
                }
            }
            calleeT.ret
        }

        else -> report(node.callee.span, "Attempt to call non-function: $calleeT").let { Type.Unknown }
    }

    override fun visit(node: Index): Type {
        val targetT = node.target.accept(this)
        val indexType = node.index.accept(this)
        if (indexType != Type.I32) {
            report(node.index.span, "Index must be of type i32; got $indexType")
        }
        return when (targetT) {
            is Type.Ptr -> targetT.base
            else -> report(node.span, "Indexing is only supported on pointers; got $targetT").let { Type.Unknown }
        }
    }

    override fun visit(node: Member): Type {
        val targetT = node.target.accept(this)
        val objT = when {
            !node.viaArrow && targetT is Type.Obj -> targetT
            node.viaArrow && targetT is Type.Ptr && targetT.base is Type.Obj && targetT.levels == 1 -> targetT.base
            else -> {
                report(node.span, "Can't access member `${node.name}` of $targetT")
                return Type.Unknown
            }
        }
        return objT.fields[node.name] ?: run {
            report(node.span, "Object ${objT.name} has no member '${node.name}'")
            Type.Unknown
        }
    }

    override fun visit(node: Assign): Type {
        val tt = node.target.accept(this)
        val vt = node.value.accept(this)
        if (tt == Type.Unknown || vt == Type.Unknown || tt != vt) {
            report(node.span, "Cannot assign $vt to $tt")
        }
        return tt
    }

    override fun visit(node: BlockExpr): Type = blockResultType(node.block)

    override fun visit(node: IfExpr): Type {
        node.cond.accept(this)
        val tThen = blockResultType(node.thenBlock)
        val tElse = blockResultType(node.elseBlock)
        return unify(tThen, tElse, node.span)
    }

    override fun visit(node: WhileExpr): Type {
        node.cond.accept(this)
        node.body.accept(this)
        return Type.Void
    }

    override fun visit(node: DoWhileExpr): Type {
        node.body.accept(this)
        node.cond.accept(this)
        return Type.Void
    }

    override fun visit(node: ForExpr): Type = withScope {
        node.init.accept(this)
        node.cond.accept(this)
        node.incr.accept(this)
        node.body.accept(this)
        Type.Void
    }

    override fun visit(node: SwitchExpr): Type {
        node.expr.accept(this)
        var acc: Type? = null
        for (c in node.cases) {
            val ct = c.accept(this)
            acc = if (acc == null) ct else unify(acc, ct, c.span)
        }
        node.defaultCase?.let {
            val dt = it.accept(this)
            acc = if (acc == null) dt else unify(acc, dt, it.span)
        }
        return acc ?: Type.Void
    }

    override fun visit(node: SwitchCase): Type = node.result.accept(this)
    override fun visit(node: Cast): Type = node.type.resolve()
    override fun visit(node: PostfixInc): Type = node.target.accept(this)
    override fun visit(node: PostfixDec): Type = node.target.accept(this)
}

