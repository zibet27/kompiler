package type

import ast.*
import ast.visitor.AstVisitor
import ast.visitor.accept
import lexer.Span

data class Diagnostic(val message: String, val span: Span) {
    override fun toString(): String = "Type error. $message at $span"
}

class Scope(val parent: Scope? = null) {
    val vars = mutableMapOf<String, KodeType>()

    operator fun get(name: String): KodeType? = vars[name] ?: parent?.get(name)
}

class TypeChecker(
    private val program: Program
) : AstVisitor<KodeType> {

    val diagnostics: MutableList<Diagnostic> = mutableListOf()

    // Environments
    private val functions = mutableMapOf<String, KodeType.Fn>()
    private val globals = mutableMapOf<String, KodeType>()
    private val types = mutableMapOf<String, KodeType>() // named types: objects, aliases, etc.

    private var scope: Scope = Scope()
    private var currentRet: KodeType = KodeType.Void

    private fun report(span: Span, msg: String) {
        diagnostics += Diagnostic(msg, span)
    }

    private fun TypeRef.resolve(): KodeType = when (this) {
        is BuiltinType -> when (kind) {
            BuiltinType.Kind.I32 -> KodeType.I32
            BuiltinType.Kind.U8 -> KodeType.U8
            BuiltinType.Kind.F64 -> KodeType.F64
            BuiltinType.Kind.Void -> KodeType.Void
        }

        is NamedType -> types[name] ?: globals[name] ?: functions[name] ?: KodeType.Unknown
        is PointerType -> KodeType.Ptr(base.resolve(), levels)
        is FuncType -> {
            val kParamTypes = paramTypes.map { it.resolve() }
            val kReturnType = returnType.resolve()
            KodeType.Fn(kParamTypes, kReturnType)
        }
    }

    private fun unify(a: KodeType, b: KodeType, span: Span): KodeType = when {
        a.canImplicitlyCastTo(b) -> b
        b.canImplicitlyCastTo(a) -> a
        else -> {
            report(span, "Type mismatch: $a vs $b")
            KodeType.Unknown
        }
    }

    private fun blockResultType(block: Block): KodeType {
        // The value of a block is the value of its last expression statement, if any; otherwise void.
        var lastType: KodeType = KodeType.Void
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
        for (d in program.declarations) when (d) {
            is FunDecl -> functions[d.name] =
                KodeType.Fn(d.params.map { it.type.resolve() }, d.returnType.resolve()).also { d.kodeType = it }

            is FunDef -> functions[d.name] =
                KodeType.Fn(d.params.map { it.type.resolve() }, d.returnType.resolve()).also { d.kodeType = it }

            is GlobalVarDecl -> d.declarators.forEach { decl ->
                val kodeType = d.type.resolve().withDimensions(decl.arrayDims.map { d -> d.value })
                globals[decl.name] = kodeType
                decl.kodeType = kodeType
            }

            is ObjectDef -> {
                // collect object type with resolved fields
                val fieldList = d.fields.map { it.name to it.type.resolve() }
                types[d.name] = KodeType.Obj(d.name, fieldList, isComplete = true)
            }

            is ObjectDecl -> {
                if (types.containsKey(d.name)) {
                    report(d.span, "Duplicate object declaration: ${d.name}")
                    continue
                }
                types[d.name] = KodeType.Obj(d.name, emptyList(), isComplete = false)
            }

            is TypeAlias -> types[d.name] = KodeType.Fn(
                params = d.paramTypes.map { it.resolve() }, ret = d.returnType.resolve()
            )

            is AlienFunDecl -> functions[d.name] = KodeType.Fn(
                params = d.params.map { it.type.resolve() }, ret = d.returnType.resolve()
            ).also { d.kodeType = it }
        }
    }

    fun check(): List<Diagnostic> {
        program.accept(this)
        return diagnostics
    }

    // Visitor impl
    override fun visit(node: Program): KodeType {
        node.declarations.forEach { it.accept(this) }
        return KodeType.Void
    }

    override fun visit(node: FunDecl): KodeType = functions[node.name] ?: KodeType.Unknown

    override fun visit(node: FunDef): KodeType {
        val fn = functions[node.name] ?: KodeType.Fn(node.params.map { it.type.resolve() }, node.returnType.resolve())
        withScope {
            node.params.forEach { scope.vars[it.name] = it.type.resolve() }
            val prevRet = currentRet
            currentRet = fn.ret
            node.body.accept(this)
            currentRet = prevRet
        }
        return fn
    }

    override fun visit(node: AlienFunDecl): KodeType = KodeType.Void

    override fun visit(node: ObjectDecl): KodeType = KodeType.Void
    override fun visit(node: ObjectDef): KodeType {
        val existing = types[node.name]
        if (existing != null && existing !is KodeType.Obj) {
            report(node.span, "Cannot redefine non-object type ${node.name}")
            return KodeType.Unknown
        }
        val fields = node.fields.map { it.name to it.type.resolve() }
        val def = KodeType.Obj(node.name, fields, isComplete = true)
        if (existing is KodeType.Obj && existing.isComplete && existing != def) {
            report(node.span, "Conflicting redefinition of object ${node.name}")
        }
        types[node.name] = def
        node.kodeType = def
        return KodeType.Void
    }

    override fun visit(node: TypeAlias): KodeType = KodeType.Void

    override fun visit(node: GlobalVarDecl): KodeType {
        val baseType = node.type.resolve()
        node.declarators.forEach { dec ->
            val kodeType = baseType.withDimensions(dec.arrayDims.map { d -> d.value })
            globals[dec.name] = kodeType
            dec.kodeType = kodeType
            val srcType = dec.init.accept(this)
            if (!srcType.canImplicitlyCastTo(baseType)) {
                report(dec.init.span, "Cannot initialize '${dec.name}': $srcType is not assignable to $baseType")
            }
        }
        return KodeType.Void
    }

    override fun visit(node: FieldDecl): KodeType = error("Unreachable.")

    override fun visit(node: Param): KodeType {
        return node.type.resolve().also { node.kodeType = it }
    }

    override fun visit(node: Declarator): KodeType {
        return node.init.accept(this).also { node.kodeType = it }
    }

    override fun visit(node: WithInit): KodeType = node.expr.accept(this)
    override fun visit(node: AssignInit): KodeType = node.expr.accept(this)

    override fun visit(node: BuiltinType): KodeType = node.resolve()
    override fun visit(node: NamedType): KodeType = node.resolve()
    override fun visit(node: PointerType): KodeType = node.resolve()
    override fun visit(node: FuncType): KodeType = error("Unreachable.")

    override fun visit(node: Block): KodeType = blockResultType(node)

    override fun visit(node: LocalVarDecl): KodeType {
        val baseType = node.type.resolve()
        node.declarators.forEach { dec ->
            val kodeType = baseType.withDimensions(dec.arrayDims.map { d -> d.value })
            scope.vars[dec.name] = kodeType
            dec.kodeType = kodeType
            val srcType = dec.init.accept(this)
            if (!srcType.canImplicitlyCastTo(other = baseType)) {
                report(dec.init.span, "Cannot initialize '${dec.name}': $srcType is not assignable to $baseType")
            }
        }
        return KodeType.Void
    }

    override fun visit(node: ExprStmt): KodeType = node.expr.accept(this)
    override fun visit(node: SkipStmt): KodeType = KodeType.Void
    override fun visit(node: StopStmt): KodeType = KodeType.Void

    override fun visit(node: IntLit): KodeType = KodeType.I32
    override fun visit(node: F64Lit): KodeType = KodeType.F64
    override fun visit(node: CharLit): KodeType = KodeType.U8
    override fun visit(node: StringLit): KodeType = KodeType.Ptr(KodeType.U8, 1)

    override fun visit(node: Ident): KodeType = scope[node.name] ?: globals[node.name] ?: functions[node.name] ?: run {
        report(node.span, "Unresolved identifier '${node.name}'")
        KodeType.Unknown
    }

    override fun visit(node: Unary): KodeType {
        val t = node.expr.accept(this)
        return when (node.op) {
            UnaryOp.Not, UnaryOp.BitNot, UnaryOp.Plus, UnaryOp.Minus, UnaryOp.PreInc, UnaryOp.PreDec -> when (t) {
                is KodeType.F64, is KodeType.I32, is KodeType.U8 -> t
                is KodeType.Unknown -> t
                else -> report(node.span, "Unary ${node.op} not applicable to $t").let { KodeType.Unknown }
            }

            UnaryOp.Deref -> when (t) {
                is KodeType.Ptr -> t.referenced()
                else -> report(node.span, "Cannot dereference $t").let { KodeType.Unknown }
            }

            UnaryOp.AddressOf -> when (t) {
                is KodeType.Ptr -> KodeType.Ptr(t.base, levels = t.levels + 1)
                else -> KodeType.Ptr(base = t, levels = 1)
            }
        }
    }

    override fun visit(node: Binary): KodeType {
        val lt = node.left.accept(this)
        val rt = node.right.accept(this)
        if (lt != rt || lt !in listOf(KodeType.I32, KodeType.U8, KodeType.F64)) {
            report(node.span, "Incompatible numeric types: $lt and $rt")
            return KodeType.Unknown
        }
        return when (node.op) {
            BinaryOp.Add, BinaryOp.Sub, BinaryOp.Mul, BinaryOp.Div, BinaryOp.Mod, BinaryOp.Shl, BinaryOp.Shr, BinaryOp.BitAnd, BinaryOp.BitOr, BinaryOp.BitXor -> lt
            BinaryOp.Eq, BinaryOp.Ne, BinaryOp.Lt, BinaryOp.Gt, BinaryOp.Le, BinaryOp.Ge -> KodeType.I32
            BinaryOp.OrOr, BinaryOp.AndAnd -> KodeType.I32
        }
    }

    override fun visit(node: Call): KodeType = when (val calleeT = node.callee.accept(this)) {
        is KodeType.Fn -> {
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

        else -> report(node.callee.span, "Attempt to call non-function: $calleeT").let { KodeType.Unknown }
    }

    override fun visit(node: Index): KodeType {
        val targetT = node.target.accept(this)
        val indexType = node.index.accept(this)
        if (indexType != KodeType.I32) {
            report(node.index.span, "Index must be of type i32; got $indexType")
        }
        return when (targetT) {
            is KodeType.Ptr -> targetT.base
            is KodeType.Arr -> targetT.base
            else -> report(node.span, "Indexing is only supported on pointers; got $targetT").let { KodeType.Unknown }
        }
    }

    override fun visit(node: Member): KodeType {
        val targetT = node.target.accept(this)
        val objT = when {
            !node.viaArrow && targetT is KodeType.Obj -> targetT
            node.viaArrow && targetT is KodeType.Ptr && targetT.base is KodeType.Obj && targetT.levels == 1 -> targetT.base
            else -> {
                report(node.span, "Can't access member `${node.name}` of $targetT")
                return KodeType.Unknown
            }
        }
        val fieldWithName = objT.fields.firstOrNull { it.first == node.name }
        if (fieldWithName != null) {
            return fieldWithName.second
        }
        report(node.span, "Object ${objT.name} has no member '${node.name}'")
        return KodeType.Unknown
    }

    override fun visit(node: Assign): KodeType {
        val tt = node.target.accept(this)
        val vt = node.value.accept(this)
        if (tt == KodeType.Unknown || vt == KodeType.Unknown || tt != vt) {
            report(node.span, "Cannot assign $vt to $tt")
        }
        return tt
    }

    override fun visit(node: BlockExpr): KodeType = blockResultType(node.block)

    override fun visit(node: IfExpr): KodeType {
        node.cond.accept(this)
        val tThen = blockResultType(node.thenBlock)
        val tElse = blockResultType(node.elseBlock)
        return unify(tThen, tElse, node.span)
    }

    override fun visit(node: WhileExpr): KodeType {
        node.cond.accept(this)
        node.body.accept(this)
        return KodeType.Void
    }

    override fun visit(node: DoWhileExpr): KodeType {
        node.body.accept(this)
        node.cond.accept(this)
        return KodeType.Void
    }

    override fun visit(node: ForExpr): KodeType = withScope {
        node.init.accept(this)
        node.cond.accept(this)
        node.incr.accept(this)
        node.body.accept(this)
        KodeType.Void
    }

    override fun visit(node: SwitchExpr): KodeType {
        node.expr.accept(this)
        var acc: KodeType? = null
        for (c in node.cases) {
            val ct = c.accept(this)
            acc = if (acc == null) ct else unify(acc, ct, c.span)
        }
        node.defaultCase?.let {
            val dt = it.accept(this)
            acc = if (acc == null) dt else unify(acc, dt, it.span)
        }
        return acc ?: KodeType.Void
    }

    override fun visit(node: SwitchCase): KodeType = node.result.accept(this)
    override fun visit(node: Cast): KodeType {
        val srcKodeType = node.expr.accept(this)
        val destKodeType = node.toType.resolve()
        if (!srcKodeType.canCastTo(destKodeType)) {
            report(node.span, "Cannot cast $srcKodeType to $destKodeType")
        }
        node.kodeType = destKodeType
        return destKodeType
    }

    override fun visit(node: PostfixInc): KodeType = node.target.accept(this)
    override fun visit(node: PostfixDec): KodeType = node.target.accept(this)

    override fun visit(node: StructInit): KodeType {
        val structType = types[node.typeName]
        if (structType == null) {
            report(node.span, "Unknown type: ${node.typeName}")
            return KodeType.Unknown
        }
        if (structType !is KodeType.Obj) {
            report(node.span, "${node.typeName} is not a struct type")
            return KodeType.Unknown
        }

        for (fieldInit in node.fieldInits) {
            val fieldName = fieldInit.name
            val fieldType = structType.fields.firstOrNull { it.first == fieldName }
            if (fieldType == null) {
                report(fieldInit.span, "Unknown field: $fieldName in ${node.typeName}")
            } else {
                val initType = fieldInit.value.accept(this)
                unify(initType, fieldType.second, fieldInit.span)
            }
        }

        return structType
    }

    override fun visit(node: FieldInit): KodeType {
        return node.value.accept(this)
    }
}

