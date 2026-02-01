package ast

import lexer.Position
import lexer.Span
import lexer.Token
import parser.ast.unexpected

/**
 * Strongly typed semantic value used by LR semantic actions to avoid `Any?`.
 * Every reduction and shift returns one of these variants.
 */
sealed interface AstValue {
    data class T(val token: Token) : AstValue {
        override fun toString(): String = token.toString()
    }

    // Top-level
    data class Pgm(val program: Program) : AstValue
    data class TD(val decl: TopDecl) : AstValue
    data class TDs(val list: List<TopDecl>) : AstValue
    data class FunHeaderV(val name: String, val params: List<Param>, val retType: TypeRef?, val span: Span) : AstValue
    data class ObjectHeaderV(val name: String, val span: Span) : AstValue

    // Types and params
    data class Ty(val type: TypeRef) : AstValue
    data class Tys(val list: List<TypeRef>) : AstValue
    data class ParamV(val param: Param) : AstValue
    data class ParamsV(val list: List<Param>) : AstValue

    // Declarations and fields
    data class DeclaratorV(val decl: Declarator) : AstValue
    data class DeclaratorsV(val list: List<Declarator>) : AstValue
    data class FieldV(val field: FieldDecl) : AstValue
    data class FieldsV(val list: List<FieldDecl>) : AstValue
    data class InitV(val init: Init?) : AstValue

    // Items and blocks
    data class ItemV(val item: Item) : AstValue
    data class ItemsV(val list: List<Item>) : AstValue
    data class BlockV(val block: Block) : AstValue
    data class BlockExprV(val blockExpr: BlockExpr) : AstValue

    // Expressions
    data class ExprV(val expr: Expr) : AstValue
    data class ExprsV(val list: List<Expr>) : AstValue

    // Switch cases
    data class SwitchCaseV(val case: SwitchCase) : AstValue
    data class SwitchCasesV(val list: List<SwitchCase>) : AstValue

    // Struct initialization
    data class FieldInitV(val fieldInit: FieldInit) : AstValue
    data class FieldInitsV(val list: List<FieldInit>) : AstValue

    data object Empty : AstValue
}

internal fun tok(v: AstValue) = (v as? AstValue.T ?: v.unexpected()).token
internal fun td(v: AstValue) = (v as? AstValue.TD ?: v.unexpected())
internal fun tds(v: AstValue) = (v as? AstValue.TDs ?: v.unexpected())
internal fun ty(v: AstValue) = (v as? AstValue.Ty ?: v.unexpected())
internal fun tys(v: AstValue) = (v as AstValue.Tys)
internal fun param(v: AstValue) = (v as AstValue.ParamV)
internal fun params(v: AstValue) = (v as AstValue.ParamsV)
internal fun declarator(v: AstValue) = (v as AstValue.DeclaratorV)
internal fun declarators(v: AstValue) = (v as AstValue.DeclaratorsV)
internal fun item(v: AstValue) = (v as AstValue.ItemV)
internal fun items(v: AstValue) = (v as AstValue.ItemsV)
internal fun expr(v: AstValue) = (v as AstValue.ExprV).expr
internal fun exprs(v: AstValue) = (v as AstValue.ExprsV)
internal fun field(v: AstValue) = (v as AstValue.FieldV)
internal fun fields(v: AstValue) = (v as AstValue.FieldsV)
internal fun switchCase(v: AstValue) = (v as AstValue.SwitchCaseV)
internal fun switchCases(v: AstValue) = (v as AstValue.SwitchCasesV)
internal fun initV(v: AstValue) = (v as AstValue.InitV)
internal fun fieldInit(v: AstValue) = (v as AstValue.FieldInitV)
internal fun fieldInits(v: AstValue) = (v as AstValue.FieldInitsV)

internal fun span(values: List<AstValue>): Span {
    var first: Span? = null
    var last: Span? = null
    fun remember(s: Span) {
        if (first == null || s.start.index < first!!.start.index) first = s
        if (last == null || s.end.index > last!!.end.index) last = s
    }
    for (v in values) when (v) {
        is AstValue.T -> remember(v.token.span)
        is AstValue.Pgm -> remember(v.program.span)
        is AstValue.TD -> remember(v.decl.span)
        is AstValue.TDs -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.FunHeaderV -> remember(v.span)
        is AstValue.ObjectHeaderV -> remember(v.span)
        is AstValue.Ty -> remember(v.type.span)
        is AstValue.ParamV -> remember(v.param.span)
        is AstValue.ParamsV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.DeclaratorV -> remember(v.decl.span)
        is AstValue.DeclaratorsV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.FieldV -> remember(v.field.span)
        is AstValue.FieldsV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.ItemV -> remember(v.item.span)
        is AstValue.ItemsV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.BlockV -> remember(v.block.span)
        is AstValue.BlockExprV -> remember(v.blockExpr.span)
        is AstValue.ExprV -> remember(v.expr.span)
        is AstValue.ExprsV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.SwitchCaseV -> remember(v.case.span)
        is AstValue.SwitchCasesV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.Tys -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.InitV -> v.init?.span?.let { remember(it) }
        is AstValue.FieldInitV -> remember(v.fieldInit.span)
        is AstValue.FieldInitsV -> v.list.firstOrNull()?.span?.let { remember(it) }
        is AstValue.Empty -> {}
    }
    return when {
        first != null && last != null -> Span(first.start, last.end)
        else -> Span(start = Position.Zero, end = Position.Zero)
    }
}
