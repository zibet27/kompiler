package ast.visitor

import ast.*

/**
 * A classic Visitor interface for the AST. Each node has a dedicated `visit` method.
 * Use the generated `accept` extension on each node type for double dispatch.
 */
interface AstVisitor<R> {
    // Program and top-level declarations
    fun visit(node: Program): R
    fun visit(node: FunDecl): R
    fun visit(node: FunDef): R
    fun visit(node: AlienFunDecl): R
    fun visit(node: ObjectDecl): R
    fun visit(node: ObjectDef): R
    fun visit(node: TypeAlias): R
    fun visit(node: GlobalVarDecl): R

    fun visit(node: FieldDecl): R
    fun visit(node: Param): R
    fun visit(node: Declarator): R
    fun visit(node: WithInit): R
    fun visit(node: AssignInit): R

    // Types
    fun visit(node: BuiltinType): R
    fun visit(node: NamedType): R
    fun visit(node: PointerType): R
    fun visit(node: FuncType): R

    // Block and items/statements
    fun visit(node: Block): R
    fun visit(node: LocalVarDecl): R
    fun visit(node: ExprStmt): R
    fun visit(node: SkipStmt): R
    fun visit(node: StopStmt): R

    // Expressions
    fun visit(node: IntLit): R
    fun visit(node: F64Lit): R
    fun visit(node: CharLit): R
    fun visit(node: StringLit): R
    fun visit(node: Ident): R
    fun visit(node: Unary): R
    fun visit(node: Binary): R
    fun visit(node: Call): R
    fun visit(node: Index): R
    fun visit(node: Member): R
    fun visit(node: Assign): R
    fun visit(node: BlockExpr): R
    fun visit(node: IfExpr): R
    fun visit(node: WhileExpr): R
    fun visit(node: DoWhileExpr): R
    fun visit(node: ForExpr): R
    fun visit(node: SwitchExpr): R
    fun visit(node: SwitchCase): R
    fun visit(node: Cast): R
    fun visit(node: PostfixInc): R
    fun visit(node: PostfixDec): R
}

// Convenience double-dispatch entry points
fun <R> Node.accept(v: AstVisitor<R>): R = when (this) {
    is Program -> v.visit(this)
    is FunDecl -> v.visit(this)
    is FunDef -> v.visit(this)
    is AlienFunDecl -> v.visit(this)
    is ObjectDecl -> v.visit(this)
    is ObjectDef -> v.visit(this)
    is TypeAlias -> v.visit(this)
    is GlobalVarDecl -> v.visit(this)
    is FieldDecl -> v.visit(this)
    is Param -> v.visit(this)
    is Declarator -> v.visit(this)
    is WithInit -> v.visit(this)
    is AssignInit -> v.visit(this)
    is BuiltinType -> v.visit(this)
    is NamedType -> v.visit(this)
    is PointerType -> v.visit(this)
    is FuncType -> v.visit(this)
    is Block -> v.visit(this)
    is LocalVarDecl -> v.visit(this)
    is ExprStmt -> v.visit(this)
    is SkipStmt -> v.visit(this)
    is StopStmt -> v.visit(this)
    is IntLit -> v.visit(this)
    is F64Lit -> v.visit(this)
    is CharLit -> v.visit(this)
    is StringLit -> v.visit(this)
    is Ident -> v.visit(this)
    is Unary -> v.visit(this)
    is Binary -> v.visit(this)
    is Call -> v.visit(this)
    is Index -> v.visit(this)
    is Member -> v.visit(this)
    is Assign -> v.visit(this)
    is BlockExpr -> v.visit(this)
    is IfExpr -> v.visit(this)
    is WhileExpr -> v.visit(this)
    is DoWhileExpr -> v.visit(this)
    is ForExpr -> v.visit(this)
    is SwitchExpr -> v.visit(this)
    is SwitchCase -> v.visit(this)
    is Cast -> v.visit(this)
    is PostfixInc -> v.visit(this)
    is PostfixDec -> v.visit(this)
    else -> error("Unrecognized node type: ${this::class.simpleName}")
}
