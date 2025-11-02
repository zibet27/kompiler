package parser.ast

import lexer.Token

/**
 * Strongly typed semantic value used by LR semantic actions to avoid `Any?`.
 * Every reduction and shift returns one of these variants.
 */
sealed interface AstValue {
    data class T(val token: Token) : AstValue

    // Top-level
    data class Pgm(val program: Program) : AstValue
    data class TD(val decl: TopDecl) : AstValue
    data class TDs(val list: List<TopDecl>) : AstValue

    // Types and params
    data class Ty(val type: TypeRef) : AstValue
    data class TyOpt(val type: TypeRef?) : AstValue
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

    // Utility
    data class IntV(val value: Int) : AstValue
}
