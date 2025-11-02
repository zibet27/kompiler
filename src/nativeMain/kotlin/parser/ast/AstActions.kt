package parser.ast

import lexer.Span
import lexer.Token
import parser.KodeGrammar
import parser.SemanticActions
import parser.generator.GrammarRule
import parser.generator.Terminal

/**
 * Semantic actions that build the AST directly during LR parsing.
 */
object AstActions : SemanticActions<AstValue> {

    override fun onShift(token: Token): AstValue = AstValue.T(token)

    override fun onReduce(rule: GrammarRule, children: List<AstValue>): AstValue {
        val (lhs, rhs) = rule
        return when (lhs) {
            // Program and top-levels
            KodeGrammar.NT.Program -> {
                val decls = tds(children[0]).list
                AstValue.Pgm(Program(decls, span(children)))
            }

            KodeGrammar.NT.TopDeclList -> when (rhs.size) {
                1 -> AstValue.TDs(listOf(td(children[0]).decl))
                2 -> {
                    val left = tds(children[0]).list
                    val right = td(children[1]).decl
                    AstValue.TDs(left + right)
                }

                else -> error("TopDeclList shape")
            }

            KodeGrammar.NT.TopDecl -> children[0] // already TD

            KodeGrammar.NT.FunDecl -> {
                val nameTok = tok(children[1])
                val params = params(children[3]).list
                val ret = ty(children[6]).type
                AstValue.TD(FunDecl(nameTok.lexeme, params, ret, span(children)))
            }

            KodeGrammar.NT.FunDef -> {
                val nameTok = tok(children[1])
                val params = params(children[3]).list
                val ret = ty(children[6]).type
                val items = items(children[8]).list
                val body = Block(items, span(children.subList(7, 10)))
                AstValue.TD(FunDef(nameTok.lexeme, params, ret, body, span(children)))
            }

            KodeGrammar.NT.AlienFunDecl -> {
                val nameTok = tok(children[2])
                val params = params(children[4]).list
                val ret = tyOpt(children[6]).type
                AstValue.TD(AlienFunDecl(nameTok.lexeme, params, ret, span(children)))
            }

            // Types
            KodeGrammar.NT.Type -> when (rhs.size) {
                2 if rhs[0] == KodeGrammar.NT.BaseType -> {
                    val base = ty(children[0]).type
                    val levels = intv(children[1]).value
                    AstValue.Ty(if (levels == 0) base else PointerType(base, levels, span(children)))
                }

                2 if rule.rhs[0] is Terminal -> {
                    val idTok = tok(children[0])
                    val named: TypeRef = NamedType(idTok.lexeme, idTok.span)
                    val levels = intv(children[1]).value
                    AstValue.Ty(if (levels == 0) named else PointerType(named, levels, span(children)))
                }

                else -> children[0]
            }

            KodeGrammar.NT.BaseType -> {
                val tok = tok(children[0])
                val kind = when (tok) {
                    is Token.Void -> BuiltinType.Kind.Void
                    is Token.I32 -> BuiltinType.Kind.I32
                    is Token.F64 -> BuiltinType.Kind.F64
                    is Token.U8 -> BuiltinType.Kind.U8
                    else -> error("Unexpected type: ${tok.lexeme}")
                }
                AstValue.Ty(BuiltinType(kind, tok.span))
            }

            KodeGrammar.NT.TypeSuffix -> when (rule.rhs.size) {
                0 -> AstValue.IntV(0)
                2 -> AstValue.IntV(intv(children[0]).value + 1)
                else -> error("TypeSuffix shape")
            }

            KodeGrammar.NT.FuncType -> {
                val params = tys(children[1]).list
                val ret = ty(children[4]).type
                AstValue.Ty(FuncType(params, ret, span(children)))
            }

            KodeGrammar.NT.ParamTypeListOpt -> when (rule.rhs.size) {
                0 -> AstValue.Tys(emptyList())
                else -> children[0] // Tys
            }

            KodeGrammar.NT.ParamTypeList -> when (rule.rhs.size) {
                1 -> AstValue.Tys(listOf(ty(children[0]).type))
                3 -> AstValue.Tys(tys(children[0]).list + ty(children[2]).type)
                else -> error("ParamTypeList shape")
            }

            // Params
            KodeGrammar.NT.ParamsOpt -> if (rule.rhs.isEmpty()) AstValue.ParamsV(emptyList()) else children[0]
            KodeGrammar.NT.Params -> children[0]
            KodeGrammar.NT.ParamList -> when (rule.rhs.size) {
                1 -> AstValue.ParamsV(listOf(param(children[0]).param))
                3 -> AstValue.ParamsV(params(children[0]).list + param(children[2]).param)
                else -> error("ParamList shape")
            }

            KodeGrammar.NT.Param -> {
                val nameTok = tok(children[0])
                val type = ty(children[2]).type
                AstValue.ParamV(Param(nameTok.lexeme, type, span(children)))
            }

            // Top-level var decls and declarators
            KodeGrammar.NT.GlobalVarDecl -> AstValue.TD(
                GlobalVarDecl(ty(children[0]).type, declarators(children[1]).list, span(children))
            )

            KodeGrammar.NT.DeclaratorList -> when (rule.rhs.size) {
                1 -> AstValue.DeclaratorsV(listOf(declarator(children[0]).decl))
                3 -> AstValue.DeclaratorsV(declarators(children[0]).list + declarator(children[2]).decl)
                else -> error("DeclaratorList shape")
            }

            KodeGrammar.NT.Declarator -> {
                val nameTok = tok(children[0])
                val dims = exprs(children[1]).list
                val init = initV(children[2]).init
                AstValue.DeclaratorV(Declarator(nameTok.lexeme, dims, init, span(children)))
            }

            KodeGrammar.NT.ArraySuffixOpt -> when (rule.rhs.size) {
                0 -> AstValue.ExprsV(emptyList())
                4 -> AstValue.ExprsV(exprs(children[0]).list + expr(children[2]))
                else -> error("ArraySuffixOpt shape")
            }

            KodeGrammar.NT.InitOpt -> when (rule.rhs.size) {
                0 -> AstValue.InitV(null)
                2 -> {
                    val expr = expr(children[1])
                    val t0 = tok(children[0])
                    AstValue.InitV(
                        if (t0.lexeme == "with") WithInit(expr, span(children)) else AssignInit(
                            expr,
                            span(children)
                        )
                    )
                }

                else -> error("InitOpt shape")
            }

            // Function body block
            KodeGrammar.NT.ItemListOpt -> if (rule.rhs.isEmpty()) AstValue.ItemsV(emptyList()) else children[0]
            KodeGrammar.NT.ItemList -> when (rule.rhs.size) {
                1 -> AstValue.ItemsV(listOf(item(children[0]).item))
                2 -> AstValue.ItemsV(items(children[0]).list + item(children[1]).item)
                else -> error("ItemList shape")
            }

            KodeGrammar.NT.Item -> when (rule.rhs.size) {
                2 if rhs[0] == KodeGrammar.NT.LocalVarDecl -> item(
                    children[0]
                )

                2 if rhs[0] == KodeGrammar.NT.Expr -> AstValue.ItemV(
                    ExprStmt(expr(children[0]), span(children))
                )

                2 if children[0] is AstValue.T -> {
                    val it = when (val t = tok(children[0])) {
                        is Token.Skip -> SkipStmt(span(children))
                        is Token.Stop -> StopStmt(span(children))
                        else -> error("Unsupported simple item: ${t.lexeme}")
                    }
                    AstValue.ItemV(it)
                }

                else -> error("Loop statements not yet supported in AstActions")
            }

            KodeGrammar.NT.LocalVarDecl -> AstValue.ItemV(
                LocalVarDecl(
                    ty(children[0]).type,
                    declarators(children[1]).list,
                    span(children)
                )
            )

            // Expressions
            KodeGrammar.NT.Expr -> children[0]
            KodeGrammar.NT.Assign -> if (rule.rhs.size == 3) AstValue.ExprV(
                Assign(
                    expr(children[0]),
                    expr(children[2]),
                    span(children)
                )
            ) else children[0]

            KodeGrammar.NT.LValue -> children[0]
            KodeGrammar.NT.OrExpr -> binLeft(children, BinaryOp.OrOr)
            KodeGrammar.NT.AndExpr -> binLeft(children, BinaryOp.AndAnd)
            KodeGrammar.NT.BitOrExpr -> binLeft(children, BinaryOp.BitOr)
            KodeGrammar.NT.BitXorExpr -> binLeft(children, BinaryOp.BitXor)
            KodeGrammar.NT.BitAndExpr -> binLeft(children, BinaryOp.BitAnd)
            KodeGrammar.NT.Equality -> when (rule.rhs.size) {
                3 -> {
                    val opTok = tok(children[1])
                    val op = if (opTok.lexeme == "==") BinaryOp.Eq else BinaryOp.Ne
                    AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
                }

                else -> children[0]
            }

            KodeGrammar.NT.Rel -> if (rule.rhs.size == 3) {
                val op = when (tok(children[1]).lexeme) {
                    "<" -> BinaryOp.Lt
                    ">" -> BinaryOp.Gt
                    "<=" -> BinaryOp.Le
                    ">=" -> BinaryOp.Ge
                    else -> error("rel-op")
                }
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.Shift -> if (rule.rhs.size == 3) {
                val op = if (tok(children[1]).lexeme == "<<") BinaryOp.Shl else BinaryOp.Shr
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.Add -> if (rule.rhs.size == 3) {
                val op = if (tok(children[1]).lexeme == "+") BinaryOp.Add else BinaryOp.Sub
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.Mul -> if (rule.rhs.size == 3) {
                val op = when (tok(children[1]).lexeme) {
                    "*" -> BinaryOp.Mul
                    "/" -> BinaryOp.Div
                    "%" -> BinaryOp.Mod
                    else -> error("mul-op")
                }
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.Factor -> children[0]
            KodeGrammar.NT.Prefixed -> {
                val op = when (tok(children[0])) {
                    is Token.Bang -> UnaryOp.Not
                    is Token.Tilde -> UnaryOp.BitNot
                    is Token.Plus -> UnaryOp.Plus
                    is Token.Minus -> UnaryOp.Minus
                    is Token.PlusPlus -> UnaryOp.PreInc
                    is Token.MinusMinus -> UnaryOp.PreDec
                    is Token.Star -> UnaryOp.Deref
                    is Token.Amp -> UnaryOp.AddressOf
                    else -> error("prefix-op")
                }
                AstValue.ExprV(Unary(op, expr(children[1]), span(children)))
            }

            KodeGrammar.NT.Postfix -> if (rule.rhs.size == 1) children[0] else when (tok(children[1])) {
                is Token.PlusPlus -> AstValue.ExprV(PostfixInc(expr(children[0]), span(children)))
                is Token.MinusMinus -> AstValue.ExprV(PostfixDec(expr(children[0]), span(children)))
                is Token.Dot -> AstValue.ExprV(
                    Member(
                        expr(children[0]),
                        tok(children[2]).lexeme,
                        viaArrow = false,
                        span(children)
                    )
                )

                is Token.Arrow -> AstValue.ExprV(
                    Member(
                        expr(children[0]),
                        tok(children[2]).lexeme,
                        viaArrow = true,
                        span(children)
                    )
                )

                is Token.LBracket -> AstValue.ExprV(Index(expr(children[0]), expr(children[2]), span(children)))
                is Token.LBrace -> AstValue.ExprV(Call(expr(children[0]), exprs(children[2]).list, span(children)))
                else -> error("postfix tail")
            }

            KodeGrammar.NT.ArgsOpt -> if (rule.rhs.isEmpty()) AstValue.ExprsV(emptyList()) else children[0]
            KodeGrammar.NT.ArgList -> when (rule.rhs.size) {
                1 -> AstValue.ExprsV(listOf(expr(children[0])))
                3 -> AstValue.ExprsV(exprs(children[0]).list + expr(children[2]))
                else -> error("ArgList shape")
            }

            KodeGrammar.NT.Simple -> {
                when (val v = children[0]) {
                    is AstValue.ExprV -> v
                    is AstValue.T -> {
                        val e: Expr = when (val t = v.token) {
                            is Token.Identifier -> Ident(t.lexeme, t.span)
                            is Token.IntLiteral -> IntLit(t.lexeme, t.span)
                            is Token.CharLiteral -> CharLit(t.lexeme, t.span)
                            is Token.StringLiteral -> StringLit(t.lexeme, t.span)
                            else -> error("Unexpected simple token: ${t.lexeme}")
                        }
                        AstValue.ExprV(e)
                    }

                    else -> v
                }
            }

            KodeGrammar.NT.CastExpr -> AstValue.ExprV(
                Cast(
                    tok(children[0]).lexeme,
                    ty(children[2]).type,
                    span(children)
                )
            )

            KodeGrammar.NT.PrimaryNoIdent -> when (val v = children[0]) {
                is AstValue.ExprV -> v
                is AstValue.T -> {
                    val e = when (val t = v.token) {
                        is Token.IntLiteral -> IntLit(t.lexeme, t.span)
                        is Token.CharLiteral -> CharLit(t.lexeme, t.span)
                        is Token.StringLiteral -> StringLit(t.lexeme, t.span)
                        else -> error("Unexpected primary token: ${t.lexeme}")
                    }
                    AstValue.ExprV(e)
                }

                else -> error("PrimaryNoIdent value")
            }

            // Blocks and if-expr
            KodeGrammar.NT.BlockExpr -> AstValue.BlockExprV(
                BlockExpr(
                    Block(items(children[1]).list, span(children)),
                    span(children)
                )
            )

            KodeGrammar.NT.IfExpr -> AstValue.ExprV(
                IfExpr(expr(children[2]), blockExpr(children[4]).block, blockExpr(children[6]).block, span(children))
            )

            // Object/typealias
            KodeGrammar.NT.ObjectDecl -> AstValue.TD(ObjectDecl(tok(children[1]).lexeme, span(children)))
            KodeGrammar.NT.ObjectDef -> AstValue.TD(
                ObjectDef(
                    tok(children[1]).lexeme,
                    fields(children[3]).list,
                    span(children)
                )
            )

            KodeGrammar.NT.FieldDeclsOpt -> if (rule.rhs.isEmpty()) AstValue.FieldsV(emptyList()) else children[0]
            KodeGrammar.NT.FieldDecls -> when (rule.rhs.size) {
                1 -> AstValue.FieldsV(listOf(field(children[0]).field))
                2 -> AstValue.FieldsV(fields(children[0]).list + field(children[1]).field)
                else -> error("FieldDecls shape")
            }

            KodeGrammar.NT.FieldDecl -> AstValue.FieldV(
                FieldDecl(
                    tok(children[0]).lexeme,
                    ty(children[2]).type,
                    span(children)
                )
            )

            KodeGrammar.NT.TypeAlias -> AstValue.TD(
                TypeAlias(tok(children[1]).lexeme, tys(children[4]).list, ty(children[7]).type, span(children))
            )

            KodeGrammar.NT.RetTypeOpt -> if (rule.rhs.isEmpty()) AstValue.TyOpt(null) else AstValue.TyOpt(ty(children[1]).type)

            else -> {
                // Many non-terminals reduce to their single child — propagate wrappers
                if (children.size == 1) children[0] else error("Unhandled reduction for ${rule.lhs} := ${rule.rhs}")
            }
        }
    }

    // Helper: fold left-assoc binary
    private fun binLeft(children: List<AstValue>, op: BinaryOp): AstValue {
        return if (children.size == 3) AstValue.ExprV(
            Binary(
                op,
                expr(children[0]),
                expr(children[2]),
                span(children)
            )
        ) else children[0]
    }

    // Typed extractors
    private fun tok(v: AstValue) = (v as AstValue.T).token
    private fun td(v: AstValue) = (v as AstValue.TD)
    private fun tds(v: AstValue) = (v as AstValue.TDs)
    private fun ty(v: AstValue) = (v as AstValue.Ty)
    private fun tyOpt(v: AstValue) = (v as AstValue.TyOpt)
    private fun tys(v: AstValue) = (v as AstValue.Tys)
    private fun param(v: AstValue) = (v as AstValue.ParamV)
    private fun params(v: AstValue) = (v as AstValue.ParamsV)
    private fun declarator(v: AstValue) = (v as AstValue.DeclaratorV)
    private fun declarators(v: AstValue) = (v as AstValue.DeclaratorsV)
    private fun item(v: AstValue) = (v as AstValue.ItemV)
    private fun items(v: AstValue) = (v as AstValue.ItemsV)
    private fun expr(v: AstValue) = (v as AstValue.ExprV).expr
    private fun exprs(v: AstValue) = (v as AstValue.ExprsV)
    private fun field(v: AstValue) = (v as AstValue.FieldV)
    private fun fields(v: AstValue) = (v as AstValue.FieldsV)
    private fun initV(v: AstValue) = (v as AstValue.InitV)
    private fun blockExpr(v: AstValue) = (v as AstValue.BlockExprV).blockExpr
    private fun intv(v: AstValue) = (v as AstValue.IntV)

    // Compute span from children values
    private fun span(values: List<AstValue>): Span {
        var first: Span? = null
        var last: Span? = null
        fun remember(s: Span) {
            if (first == null || s.start.index < first!!.start.index) first = s
            if (last == null || s.end.index > last!!.end.index) last = s
        }
        for (v in values) when (v) {
            is AstValue.T -> remember(v.token.span)
            is AstValue.Pgm -> v.program.span.let { remember(it) }
            is AstValue.TD -> v.decl.span.let { remember(it) }
            is AstValue.TDs -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.Ty -> v.type.span.let { remember(it) }
            is AstValue.ParamV -> v.param.span.let { remember(it) }
            is AstValue.ParamsV -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.DeclaratorV -> v.decl.span.let { remember(it) }
            is AstValue.DeclaratorsV -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.FieldV -> v.field.span.let { remember(it) }
            is AstValue.FieldsV -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.ItemV -> v.item.span.let { remember(it) }
            is AstValue.ItemsV -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.BlockV -> v.block.span.let { remember(it) }
            is AstValue.BlockExprV -> v.blockExpr.span.let { remember(it) }
            is AstValue.ExprV -> v.expr.span.let { remember(it) }
            is AstValue.ExprsV -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.IntV -> {}
            is AstValue.TyOpt -> v.type?.span?.let { remember(it) }
            is AstValue.Tys -> v.list.firstOrNull()?.span?.let { remember(it) }
            is AstValue.InitV -> v.init?.span?.let { remember(it) }
        }
        return when {
            first != null && last != null -> Span(first.start, last.end)
            else -> error("Can't compute location of $values")
        }
    }
}
