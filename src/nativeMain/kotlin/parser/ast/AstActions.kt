package parser.ast

import ast.*
import lexer.Position
import lexer.Span
import lexer.Token
import lexer.unexpected
import parser.KodeGrammar
import parser.SemanticActions
import parser.generator.GrammarRule
import parser.generator.Terminal

/**
 * Type-safe semantic actions that build the AST directly during LR parsing.
 */
object AstActions : SemanticActions<AstValue> {

    override fun onShift(token: Token): AstValue = AstValue.T(token)

    override fun onReduce(rule: GrammarRule, children: List<AstValue>): AstValue {
        val (lhs, rhs) = rule
        return when (lhs) {
            // Program and top-levels
            KodeGrammar.NT.Program -> AstValue.Pgm(Program(tds(children[0]).list, span(children)))

            KodeGrammar.NT.TopDeclList -> when (rhs.size) {
                0 -> AstValue.TDs(emptyList())
                2 -> AstValue.TDs(tds(children[0]).list + td(children[1]).decl)
                else -> error("TopDeclList shape")
            }

            KodeGrammar.NT.TopDecl -> when (rhs.size) {
                2 -> when (rhs[0]) {
                    KodeGrammar.NT.FunHeader -> {
                        val header = children[0]
                        val body = children[1]
                        // FunHeader: fun IDENT { ParamsOpt } RetTypeOpt
                        val h = header as AstValue.FunHeaderV
                        val bodyItems = if (body is AstValue.T) {
                            null // FunDecl
                        } else {
                            items(body).list // FunDef
                        }
                        if (bodyItems == null) {
                            AstValue.TD(
                                FunDecl(
                                    h.name,
                                    h.params,
                                    h.retType ?: BuiltinType(BuiltinType.Kind.Void, h.span),
                                    span(children)
                                )
                            )
                        } else {
                            AstValue.TD(
                                FunDef(
                                    h.name,
                                    h.params,
                                    h.retType ?: BuiltinType(BuiltinType.Kind.Void, h.span),
                                    Block(bodyItems, span(listOf(body))),
                                    span(children)
                                )
                            )
                        }
                    }

                    KodeGrammar.NT.ObjectHeader -> {
                        val header = children[0] as AstValue.ObjectHeaderV
                        val body = children[1]
                        // ObjectBody: or ( FieldDeclsOpt ) ;
                        val bodyFields = if (body is AstValue.T) {
                            null // ObjectDecl
                        } else {
                            fields(body).list // ObjectDef
                        }
                        if (bodyFields == null) {
                            AstValue.TD(ObjectDecl(header.name, span(children)))
                        } else {
                            AstValue.TD(ObjectDef(header.name, bodyFields, span(children)))
                        }
                    }

                    else -> children[0]
                }

                1 -> children[0]
                else -> error("TopDecl shape")
            }

            KodeGrammar.NT.FunHeader -> {
                val name = tok(children[1]).lexeme
                val params = params(children[3]).list
                val retType = ty(children[5]).type
                AstValue.FunHeaderV(name, params, retType, span(children))
            }

            KodeGrammar.NT.FunBody -> when (rhs.size) {
                1 -> AstValue.T(tok(children[0])) // semicolon
                4 -> AstValue.ItemsV(items(children[1]).list)
                else -> error("FunBody shape")
            }

            KodeGrammar.NT.AlienFunDecl -> AstValue.TD(
                AlienFunDecl(tok(children[2]).lexeme, params(children[4]).list, ty(children[6]).type, span(children))
            )

            KodeGrammar.NT.ObjectHeader -> AstValue.ObjectHeaderV(tok(children[1]).lexeme, span(children))

            KodeGrammar.NT.ObjectBody -> when (rhs.size) {
                1 -> AstValue.T(tok(children[0])) // semicolon
                4 -> fields(children[1])
                else -> error("ObjectBody shape")
            }

            // Types
            KodeGrammar.NT.Type -> when (rhs.size) {
                1 -> children[0] // NonFuncType
                5 -> AstValue.Ty(
                    FuncType(tys(children[1]).list, ty(children[4]).type, span(children))
                )

                else -> error("Type shape")
            }

            KodeGrammar.NT.NonFuncType -> when (rhs.size) {
                1 -> children[0] // PrimaryType
                2 -> {
                    val base = ty(children[0]).type
                    // Accumulate pointer levels: i32 ptr ptr -> PointerType(i32, 2)
                    if (base is PointerType) {
                        AstValue.Ty(PointerType(base.base, base.levels + 1, span(children)))
                    } else {
                        AstValue.Ty(PointerType(base, 1, span(children)))
                    }
                }

                else -> error("NonFuncType shape")
            }

            KodeGrammar.NT.PrimaryType -> when (rhs.size) {
                1 -> when (rhs[0]) {
                    KodeGrammar.NT.BaseType -> children[0]
                    is Terminal -> {
                        when (val typeTok = tok(children[0])) {
                            is Token.TypeName -> AstValue.Ty(NamedType(typeTok.lexeme, typeTok.span))
                            else -> error("PrimaryType expected TYPENAME, got $typeTok")
                        }
                    }

                    else -> error("PrimaryType shape")
                }

                3 -> children[1] // ( Type )
                else -> error("PrimaryType shape")
            }

            KodeGrammar.NT.BaseType -> {
                val t = tok(children[0])
                val kind = when (t) {
                    is Token.Void -> BuiltinType.Kind.Void
                    is Token.I32 -> BuiltinType.Kind.I32
                    is Token.F64 -> BuiltinType.Kind.F64
                    is Token.U8 -> BuiltinType.Kind.U8
                    else -> t.unexpected()
                }
                AstValue.Ty(BuiltinType(kind, t.span))
            }

            KodeGrammar.NT.ParamTypeListOpt -> if (rhs.isEmpty()) AstValue.Tys(emptyList()) else children[0]
            KodeGrammar.NT.ParamTypeList -> when (rhs.size) {
                1 -> AstValue.Tys(listOf(ty(children[0]).type))
                3 -> AstValue.Tys(tys(children[0]).list + ty(children[2]).type)
                else -> error("ParamTypeList shape")
            }

            // Params
            KodeGrammar.NT.ParamsOpt -> if (rhs.isEmpty()) AstValue.ParamsV(emptyList()) else children[0]
            KodeGrammar.NT.Params -> children[0]
            KodeGrammar.NT.ParamList -> when (rhs.size) {
                1 -> AstValue.ParamsV(listOf(param(children[0]).param))
                3 -> AstValue.ParamsV(params(children[0]).list + param(children[2]).param)
                else -> error("ParamList shape")
            }

            KodeGrammar.NT.Param -> AstValue.ParamV(
                Param(tok(children[0]).lexeme, ty(children[2]).type, span(children))
            )

            // Decls/Declarators
            KodeGrammar.NT.GlobalVarDecl -> AstValue.TD(
                GlobalVarDecl(ty(children[0]).type, declarators(children[1]).list, span(children))
            )

            KodeGrammar.NT.DeclaratorList -> when (rhs.size) {
                1 -> AstValue.DeclaratorsV(listOf(declarator(children[0]).decl))
                3 -> AstValue.DeclaratorsV(declarators(children[0]).list + declarator(children[2]).decl)
                else -> error("DeclaratorList shape")
            }

            KodeGrammar.NT.Declarator -> {
                val nameTok = tok(children[0])
                val dims = exprs(children[1]).list
                    .map { it as? IntLit ?: error("Expected int literal for array dimension") }
                val init = initV(children[2]).init
                require(init != null) {
                    "Uninitialized variable $nameTok."
                }
                AstValue.DeclaratorV(Declarator(nameTok.lexeme, init, dims, span(children)))
            }

            KodeGrammar.NT.ArrayDimensionsOpt -> when (rhs.size) {
                0 -> AstValue.ExprsV(emptyList())
                1 -> children[0]
                else -> error("ArrayDimensionsOpt shape")
            }

            KodeGrammar.NT.ArrayDimensions -> when (rhs.size) {
                3 -> AstValue.ExprsV(listOf(expr(children[1])))
                4 -> AstValue.ExprsV(exprs(children[0]).list + expr(children[2]))
                else -> error("ArrayDimensions shape")
            }

            KodeGrammar.NT.InitOpt -> when (rhs.size) {
                0 -> AstValue.InitV(null)
                2 -> {
                    val e = expr(children[1])
                    val t0 = tok(children[0])
                    AstValue.InitV(if (t0 is Token.With) WithInit(e, span(children)) else AssignInit(e, span(children)))
                }

                else -> error("InitOpt shape")
            }

            // Blocks and items
            KodeGrammar.NT.ItemListOpt -> if (rhs.isEmpty()) AstValue.ItemsV(emptyList()) else children[0]
            KodeGrammar.NT.ItemList -> when (rhs.size) {
                1 -> AstValue.ItemsV(listOf(item(children[0]).item))
                2 -> AstValue.ItemsV(items(children[0]).list + item(children[1]).item)
                else -> error("ItemList shape")
            }

            KodeGrammar.NT.Item -> when (rule.rhs.size) {
                2 -> when {
                    rhs[0] == KodeGrammar.NT.LocalVarDecl -> children[0]
                    rhs[0] == KodeGrammar.NT.Expr -> AstValue.ItemV(
                        ExprStmt(expr(children[0]), span(children))
                    )

                    children[0] is AstValue.T -> {
                        when (val t = tok(children[0])) {
                            is Token.Skip -> AstValue.ItemV(SkipStmt(span(children)))
                            is Token.Stop -> AstValue.ItemV(StopStmt(span(children)))
                            else -> t.unexpected()
                        }
                    }

                    else -> error("Unhandled Item reduction: ${rule.rhs}")
                }

                else -> error("Unhandled Item reduction: ${rule.rhs}")
            }

            KodeGrammar.NT.LocalVarDecl -> AstValue.ItemV(
                LocalVarDecl(ty(children[0]).type, declarators(children[1]).list, span(children))
            )

            // Expressions
            KodeGrammar.NT.Expr -> children[0]
            KodeGrammar.NT.AssignExpr -> if (rhs.size == 3) AstValue.ExprV(
                Assign(expr(children[0]), expr(children[2]), span(children))
            ) else children[0]

            KodeGrammar.NT.AssignOp -> children[0]

            KodeGrammar.NT.LogicalOrExpr -> binLeft(children, BinaryOp.OrOr)
            KodeGrammar.NT.LogicalAndExpr -> binLeft(children, BinaryOp.AndAnd)
            KodeGrammar.NT.BitwiseOrExpr -> binLeft(children, BinaryOp.BitOr)
            KodeGrammar.NT.BitwiseXorExpr -> binLeft(children, BinaryOp.BitXor)
            KodeGrammar.NT.BitwiseAndExpr -> binLeft(children, BinaryOp.BitAnd)
            KodeGrammar.NT.EqualityExpr -> when (rhs.size) {
                3 -> {
                    val op = when (val t = tok(children[1])) {
                        is Token.EqualEqual -> BinaryOp.Eq
                        is Token.BangEqual -> BinaryOp.Ne
                        else -> t.unexpected()
                    }
                    AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
                }

                else -> children[0]
            }

            KodeGrammar.NT.RelationalExpr -> if (rhs.size == 3) {
                val op = when (val t = tok(children[1])) {
                    is Token.Less -> BinaryOp.Lt
                    is Token.Greater -> BinaryOp.Gt
                    is Token.LessEqual -> BinaryOp.Le
                    is Token.GreaterEqual -> BinaryOp.Ge
                    else -> t.unexpected()
                }
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.ShiftExpr -> if (rhs.size == 3) {
                val op = when (val t = tok(children[1])) {
                    is Token.ShiftLeft -> BinaryOp.Shl
                    is Token.ShiftRight -> BinaryOp.Shr
                    else -> t.unexpected()
                }
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.AdditiveExpr -> if (rhs.size == 3) {
                val op = when (val t = tok(children[1])) {
                    is Token.Plus -> BinaryOp.Add
                    is Token.Minus -> BinaryOp.Sub
                    else -> t.unexpected()
                }
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.MultiplicativeExpr -> if (rhs.size == 3) {
                val op = when (val t = tok(children[1])) {
                    is Token.Star -> BinaryOp.Mul
                    is Token.Slash -> BinaryOp.Div
                    is Token.Percent -> BinaryOp.Mod
                    else -> t.unexpected()
                }
                AstValue.ExprV(Binary(op, expr(children[0]), expr(children[2]), span(children)))
            } else children[0]

            KodeGrammar.NT.UnaryExpr -> if (rhs.size == 2) {
                val op = when (val t = tok(children[0])) {
                    is Token.Bang -> UnaryOp.Not
                    is Token.Tilde -> UnaryOp.BitNot
                    is Token.Plus -> UnaryOp.Plus
                    is Token.Minus -> UnaryOp.Minus
                    is Token.PlusPlus -> UnaryOp.PreInc
                    is Token.MinusMinus -> UnaryOp.PreDec
                    is Token.Star -> UnaryOp.Deref
                    is Token.Amp -> UnaryOp.AddressOf
                    else -> t.unexpected()
                }
                AstValue.ExprV(Unary(op, expr(children[1]), span(children)))
            } else children[0]

            KodeGrammar.NT.PrefixOp -> children[0]

            KodeGrammar.NT.ArgsOpt -> if (rhs.isEmpty()) AstValue.ExprsV(emptyList()) else children[0]
            KodeGrammar.NT.ArgList -> when (rhs.size) {
                1 -> AstValue.ExprsV(listOf(expr(children[0])))
                3 -> AstValue.ExprsV(exprs(children[0]).list + expr(children[2]))
                else -> children[0].unexpected()
            }

            KodeGrammar.NT.PostfixExpr -> if (rhs.size == 1) children[0] else when (rhs.size) {
                2 -> when (val t = tok(children[1])) {
                    is Token.PlusPlus -> AstValue.ExprV(PostfixInc(expr(children[0]), span(children)))
                    is Token.MinusMinus -> AstValue.ExprV(PostfixDec(expr(children[0]), span(children)))
                    else -> t.unexpected()
                }

                3 -> when (val t = tok(children[1])) {
                    is Token.Dot -> AstValue.ExprV(
                        Member(expr(children[0]), tok(children[2]).lexeme, false, span(children))
                    )

                    is Token.Arrow -> AstValue.ExprV(
                        Member(expr(children[0]), tok(children[2]).lexeme, true, span(children))
                    )

                    is Token.TildeGreater -> AstValue.ExprV(
                        Cast(expr(children[0]), ty(children[2]).type, span(children))
                    )

                    else -> t.unexpected()
                }

                4 -> when (val t = tok(children[1])) {
                    is Token.LBrace -> {
                        val callee = expr(children[0]) as? Ident ?: error(
                            "Identifier expected at ${span(children)}"
                        )
                        val expr = Call(callee, exprs(children[2]).list, span(children))
                        AstValue.ExprV(expr)
                    }

                    is Token.LBracket -> AstValue.ExprV(Index(expr(children[0]), expr(children[2]), span(children)))

                    else -> t.unexpected()
                }

                else -> AstValue.ExprV(Index(expr(children[0]), expr(children[2]), span(children)))
            }

            KodeGrammar.NT.PrimaryExpr -> when (val v = children[0]) {
                is AstValue.ExprV -> v
                is AstValue.T -> {
                    val e: Expr = when (val t = v.token) {
                        is Token.Identifier -> Ident(t.lexeme, t.span)
                        is Token.IntLiteral -> IntLit(t.lexeme.toInt(), t.span)
                        is Token.FloatLiteral -> F64Lit(t.lexeme.toDouble(), t.span)
                        is Token.CharLiteral -> CharLit(t.lexeme.toCharArray().first().code.toUByte(), t.span)
                        is Token.StringLiteral -> StringLit(t.lexeme, t.span)
                        else -> t.unexpected()
                    }
                    AstValue.ExprV(e)
                }

                else -> when (rhs.size) {
                    3 -> children[1] // ( Expr )
                    else -> v
                }
            }

            KodeGrammar.NT.GroupedExpr -> children[1] // { Expr }

            // Blocks and if-expr
            KodeGrammar.NT.BlockExpr -> AstValue.ExprV(
                BlockExpr(Block(items(children[1]).list, span(children)), span(children))
            )

            KodeGrammar.NT.IfExpr -> when (rhs.size) {
                7 -> {
                    val thenBlock = (expr(children[4]) as BlockExpr).block
                    when (val elseChild = children[6]) {
                        is AstValue.ExprV -> when (val e = elseChild.expr) {
                            is BlockExpr -> {
                                // if { cond } block else block
                                AstValue.ExprV(IfExpr(expr(children[2]), thenBlock, e.block, span(children)))
                            }

                            is IfExpr -> {
                                // if { cond } block else if ...
                                val elseBlock = Block(listOf(ExprStmt(e, e.span)), e.span)
                                AstValue.ExprV(IfExpr(expr(children[2]), thenBlock, elseBlock, span(children)))
                            }

                            else -> error("Unexpected else branch: $e")
                        }

                        else -> error("Unexpected else child: $elseChild")
                    }
                }

                5 -> {
                    // if { cond } block (without else)
                    val thenBlock = (expr(children[4]) as BlockExpr).block
                    val elseBlock = Block(emptyList(), Span(Position.Zero, Position.Zero))
                    AstValue.ExprV(IfExpr(expr(children[2]), thenBlock, elseBlock, span(children)))
                }

                else -> error("IfExpr shape")
            }

            KodeGrammar.NT.WhileExpr -> {
                // while { cond } block
                val cond = expr(children[2])
                val body = (expr(children[4]) as BlockExpr).block
                AstValue.ExprV(WhileExpr(cond, body, span(children)))
            }

            KodeGrammar.NT.DoWhileExpr -> {
                // do block while { cond }
                val body = (expr(children[1]) as BlockExpr).block
                val cond = expr(children[4])
                AstValue.ExprV(DoWhileExpr(body, cond, span(children)))
            }

            KodeGrammar.NT.ForExpr -> {
                // for { init; cond; incr } block
                val init = when (val initVal = children[2]) {
                    is AstValue.ItemV -> initVal.item
                    is AstValue.ExprV -> ExprStmt(initVal.expr, initVal.expr.span)
                    else -> error("Unexpected ForInit: $initVal")
                }
                val cond = expr(children[4])
                val incr = expr(children[6])
                val body = (expr(children[8]) as BlockExpr).block
                AstValue.ExprV(ForExpr(init, cond, incr, body, span(children)))
            }

            KodeGrammar.NT.ForInitOpt -> if (rhs.isEmpty()) AstValue.ItemV(
                SkipStmt(
                    Span(
                        Position.Zero, Position.Zero
                    )
                )
            ) else children[0]

            KodeGrammar.NT.ForCondOpt -> if (rhs.isEmpty()) AstValue.ExprV(
                IntLit(
                    1, Span(Position.Zero, Position.Zero)
                )
            ) else children[0]

            KodeGrammar.NT.ForIncrOpt -> if (rhs.isEmpty()) {
                AstValue.ExprV(
                    IntLit(0, Span(Position.Zero, Position.Zero))
                )
            } else children[0]

            KodeGrammar.NT.SwitchExpr -> {
                // switch { expr } (cases defaultCase)
                val switchExpr = expr(children[2])
                val cases = switchCases(children[5]).list
                val defaultCase = when (val dc = children[6]) {
                    is AstValue.ExprV -> dc.expr
                    else -> null
                }
                AstValue.ExprV(SwitchExpr(switchExpr, cases, defaultCase, span(children)))
            }

            KodeGrammar.NT.CaseListOpt -> if (rhs.isEmpty()) AstValue.SwitchCasesV(emptyList()) else children[0]
            KodeGrammar.NT.CaseList -> when (rhs.size) {
                1 -> AstValue.SwitchCasesV(listOf(switchCase(children[0]).case))
                2 -> AstValue.SwitchCasesV(switchCases(children[0]).list + switchCase(children[1]).case)
                else -> error("CaseList shape")
            }

            KodeGrammar.NT.Case -> {
                // INT -> Expr ;
                val value = (tok(children[0]) as Token.IntLiteral).lexeme.toInt()
                val result = expr(children[2])
                AstValue.SwitchCaseV(SwitchCase(value, result, span(children)))
            }

            KodeGrammar.NT.DefaultCaseOpt -> if (rhs.isEmpty()) AstValue.Empty else {
                // else -> Expr ;
                children[2]
            }

            // Object/typealias
            KodeGrammar.NT.FieldDeclsOpt -> if (rhs.isEmpty()) AstValue.FieldsV(emptyList()) else children[0]
            KodeGrammar.NT.FieldDecls -> when (rhs.size) {
                1 -> AstValue.FieldsV(listOf(field(children[0]).field))
                2 -> AstValue.FieldsV(fields(children[0]).list + field(children[1]).field)
                else -> error("FieldDecls shape")
            }

            KodeGrammar.NT.FieldDecl -> AstValue.FieldV(
                FieldDecl(
                    tok(children[0]).lexeme, ty(children[2]).type, span(children)
                )
            )

            KodeGrammar.NT.TypeAlias -> {
                val t = ty(children[3]).type
                val (params, ret) = if (t is FuncType) {
                    t.paramTypes to t.returnType
                } else {
                    emptyList<TypeRef>() to t
                }
                AstValue.TD(
                    TypeAlias(
                        tok(children[1]).lexeme, params, ret, span(children)
                    )
                )
            }

            KodeGrammar.NT.RetTypeOpt -> if (rhs.isEmpty()) {
                error("Return type expected at ${span(children)}")
            } else AstValue.Ty(ty(children[1]).type)

            // Struct initialization
            KodeGrammar.NT.StructInitExpr -> {
                val typeName = (tok(children[0]) as Token.TypeName).lexeme
                val fieldInitsList = fieldInits(children[2]).list
                AstValue.ExprV(StructInit(typeName, fieldInitsList, span(children)))
            }

            KodeGrammar.NT.FieldInitsOpt -> if (rhs.isEmpty()) AstValue.FieldInitsV(emptyList()) else children[0]
            KodeGrammar.NT.FieldInitList -> when (rhs.size) {
                1 -> AstValue.FieldInitsV(listOf(fieldInit(children[0]).fieldInit))
                3 -> AstValue.FieldInitsV(fieldInits(children[0]).list + fieldInit(children[2]).fieldInit)
                else -> error("FieldInitList shape")
            }

            KodeGrammar.NT.FieldInit -> {
                // Named: IDENT : Expr
                val name = tok(children[0]).lexeme
                val value = expr(children[2])
                AstValue.FieldInitV(FieldInit(name, value, span(children)))
            }

            else -> if (children.size == 1) children[0] else error("Unhandled reduction for ${rule.lhs} := ${rule.rhs}")
        }
    }

    // Helpers
    private fun binLeft(children: List<AstValue>, op: BinaryOp): AstValue = if (children.size == 3) AstValue.ExprV(
        Binary(
            op, expr(children[0]), expr(children[2]), span(children)
        )
    ) else children[0]
}

fun AstValue.unexpected(): Nothing = error("Unexpected syntax $this at ${span(listOf(this))}")
