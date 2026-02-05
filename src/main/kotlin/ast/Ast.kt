package ast

import lexer.Span
import type.KodeType

/**
 * Base interface for all AST nodes.
 */
sealed interface Node {
    val span: Span
}

// Program and top-level declarations
data class Program(val declarations: List<TopDecl>, override val span: Span) : Node

sealed interface TopDecl : Node

data class FunDecl(
    val name: String,
    val params: List<Param>,
    val returnType: TypeRef,
    override val span: Span
) : TopDecl {
    lateinit var kodeType: KodeType.Fn
}

data class FunDef(
    val name: String,
    val params: List<Param>,
    val returnType: TypeRef,
    val body: Block,
    override val span: Span
) : TopDecl {
    lateinit var kodeType: KodeType.Fn
}

data class AlienFunDecl(
    val name: String,
    val params: List<Param>,
    val returnType: TypeRef,
    override val span: Span
) : TopDecl {
    lateinit var kodeType: KodeType.Fn
}

data class ObjectDecl(val name: String, override val span: Span) : TopDecl

data class ObjectDef(
    val name: String,
    val fields: List<FieldDecl>,
    override val span: Span
) : TopDecl {
    var kodeType: KodeType.Obj? = null
}

data class FieldDecl(val name: String, val type: TypeRef, override val span: Span) : Node

data class TypeAlias(
    val name: String,
    val paramTypes: List<TypeRef>,
    val returnType: TypeRef,
    override val span: Span
) : TopDecl

interface VarDecl : Node {
    val type: TypeRef
    val declarators: List<Declarator>
}

data class GlobalVarDecl(
    override val type: TypeRef,
    override val declarators: List<Declarator>,
    override val span: Span
) : TopDecl, VarDecl

data class Param(
    val name: String, val type: TypeRef, override val span: Span
) : Node {
    var kodeType: KodeType? = null
}

data class Declarator(
    val name: String,
    val init: Init,
    val arrayDims: List<IntLit>,
    override val span: Span
) : Node {
    var kodeType: KodeType? = null
}

sealed interface Init : Node
data class WithInit(val expr: Expr, override val span: Span) : Init
data class AssignInit(val expr: Expr, override val span: Span) : Init

// Types
sealed interface TypeRef : Node

data class BuiltinType(val kind: Kind, override val span: Span) : TypeRef {
    enum class Kind {
        I32, U8, F64, Void
    }
}

data class NamedType(val name: String, override val span: Span) : TypeRef

data class PointerType(val base: TypeRef, val levels: Int, override val span: Span) : TypeRef

data class FuncType(
    val paramTypes: List<TypeRef>,
    val returnType: TypeRef,
    override val span: Span
) : TypeRef

// Block, items and statements
data class Block(val items: List<Item>, override val span: Span) : Node

sealed interface Item : Node
data class LocalVarDecl(
    override val type: TypeRef,
    override val declarators: List<Declarator>,
    override val span: Span
) : Item, VarDecl

data class ExprStmt(val expr: Expr, override val span: Span) : Item
data class SkipStmt(override val span: Span) : Item
data class StopStmt(override val span: Span) : Item

// Expressions
sealed interface Expr : Node

data class IntLit(val value: Int, override val span: Span) : Expr
data class F64Lit(val value: Double, override val span: Span) : Expr
data class CharLit(val value: UByte, override val span: Span) : Expr
data class StringLit(val value: String, override val span: Span) : Expr
data class Ident(val name: String, override val span: Span) : Expr
data class Unary(val op: UnaryOp, val expr: Expr, override val span: Span) : Expr
data class Binary(val op: BinaryOp, val left: Expr, val right: Expr, override val span: Span) : Expr
data class Call(val callee: Ident, val args: List<Expr>, override val span: Span) : Expr
data class Index(val target: Expr, val index: Expr, override val span: Span) : Expr
data class Member(val target: Expr, val name: String, val viaArrow: Boolean, override val span: Span) : Expr
data class Assign(val target: Expr, val value: Expr, override val span: Span) : Expr
data class BlockExpr(val block: Block, override val span: Span) : Expr
data class IfExpr(val cond: Expr, val thenBlock: Block, val elseBlock: Block, override val span: Span) : Expr
data class WhileExpr(val cond: Expr, val body: Block, override val span: Span) : Expr
data class DoWhileExpr(val body: Block, val cond: Expr, override val span: Span) : Expr
data class ForExpr(val init: Item, val cond: Expr, val incr: Expr, val body: Block, override val span: Span) : Expr
data class SwitchExpr(val expr: Expr, val cases: List<SwitchCase>, val defaultCase: Expr?, override val span: Span) :
    Expr

data class SwitchCase(val value: Int, val result: Expr, override val span: Span) : Node
data class Cast(val expr: Expr, val toType: TypeRef, override val span: Span) : Expr {
    var kodeType: KodeType? = null
}
data class PostfixInc(val target: Expr, override val span: Span) : Expr
data class PostfixDec(val target: Expr, override val span: Span) : Expr
data class StructInit(val typeName: String, val fieldInits: List<FieldInit>, override val span: Span) : Expr

data class FieldInit(val name: String, val value: Expr, override val span: Span) : Node

enum class UnaryOp { Not, BitNot, Plus, Minus, Deref, AddressOf, PreInc, PreDec }

enum class BinaryOp {
    OrOr, AndAnd, BitOr, BitXor, BitAnd, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod
}