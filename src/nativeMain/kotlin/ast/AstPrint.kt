package ast

fun Node.prettyPrint(indent: Int = 0): String {
    val prefix = "  ".repeat(indent)
    return when (this) {
        is Program -> "${prefix}Program(\n" +
                decls.joinToString("\n") { it.prettyPrint(indent + 1) } +
                "\n$prefix)"

        is FunDecl -> "${prefix}FunDecl(name='$name', params=[${params.joinToString { it.name }}], returnType=${returnType.toTypeString()})"
        is FunDef -> "${prefix}FunDef(name='$name', params=[${params.joinToString { it.name }}], returnType=${returnType.toTypeString()}\n" +
                body.prettyPrint(indent + 1) +
                "\n$prefix)"

        is AlienFunDecl -> "${prefix}AlienFunDecl(name='$name', params=[${params.joinToString { it.name }}], returnType=${returnType.toTypeString()})"
        is ObjectDecl -> "${prefix}ObjectDecl(name='$name')"
        is ObjectDef -> "${prefix}ObjectDef(name='$name', fields=[\n" +
                fields.joinToString("\n") { it.prettyPrint(indent + 1) } +
                "\n$prefix])"

        is TypeAlias -> "${prefix}TypeAlias(name='$name', paramTypes=[${paramTypes.joinToString { it.toTypeString() }}], target=${returnType.toTypeString()})"
        is GlobalVarDecl -> "${prefix}GlobalVarDecl(type=${type.toTypeString()}, declarators=[${declarators.joinToString { it.name }}])"

        is FieldDecl -> "${prefix}FieldDecl(name='$name', type=${type.toTypeString()})"
        is Param -> "${prefix}Param(name='$name', type=${type.toTypeString()})"
        is Declarator -> "${prefix}Declarator(name='$name'${if (arrayDims.isNotEmpty()) ", arrayDims=${arrayDims.size}" else ""}${", init"})"
        is WithInit -> "${prefix}WithInit(${expr.exprToString()})"
        is AssignInit -> "${prefix}AssignInit(${expr.exprToString()})"

        is Block -> "${prefix}Block(\n" +
                items.joinToString("\n") { it.prettyPrint(indent + 1) } +
                "\n$prefix)"

        is LocalVarDecl -> "${prefix}LocalVarDecl(type=${type.toTypeString()}, declarators=[${declarators.joinToString { it.name }}])"
        is ExprStmt -> "${prefix}ExprStmt(${expr.exprToString()})"
        is SkipStmt -> "${prefix}SkipStmt"
        is StopStmt -> "${prefix}StopStmt"

        is IntLit -> "${prefix}IntLit($value)"
        is F64Lit -> "${prefix}FloatLit($value)"
        is CharLit -> "${prefix}CharLit('$value')"
        is StringLit -> "${prefix}StringLit(\"$value\")"
        is Ident -> "${prefix}Ident($name)"
        is Unary -> "${prefix}Unary($op, ${expr.exprToString()})"
        is Binary -> "${prefix}Binary(${left.exprToString()} $op ${right.exprToString()})"
        is Call -> "${prefix}Call(${callee.exprToString()}, [${args.joinToString { it.exprToString() }}])"
        is Index -> "${prefix}Index(${target.exprToString()}[${index.exprToString()}])"
        is Member -> "${prefix}Member(${target.exprToString()}${if (viaArrow) "->" else "."}$name)"
        is Assign -> "${prefix}Assign(${target.exprToString()} = ${value.exprToString()})"
        is BlockExpr -> "${prefix}BlockExpr(\n${block.prettyPrint(indent + 1)}\n$prefix)"
        is IfExpr -> "${prefix}IfExpr(\n${prefix}  cond: ${cond.exprToString()}\n${prefix}  then:\n${
            thenBlock.prettyPrint(
                indent + 1
            )
        }\n${prefix}  else:\n${elseBlock.prettyPrint(indent + 1)}\n$prefix)"

        is WhileExpr -> "${prefix}WhileExpr(cond=${cond.exprToString()}\n${body.prettyPrint(indent + 1)}\n$prefix)"
        is DoWhileExpr -> "${prefix}DoWhileExpr(\n${body.prettyPrint(indent + 1)}\n${prefix}  while: ${cond.exprToString()}\n$prefix)"
        is ForExpr -> "${prefix}ForExpr(\n${prefix}  init: ${
            init.prettyPrint(0).trim()
        }\n${prefix}  cond: ${cond.exprToString()}\n${prefix}  incr: ${incr.exprToString()}\n${body.prettyPrint(indent + 1)}\n$prefix)"

        is SwitchExpr -> "${prefix}SwitchExpr(${expr.exprToString()}\n" +
                cases.joinToString("\n") { "$prefix  case ${it.value}: ${it.result.exprToString()}" } +
                (defaultCase?.let { "\n${prefix}  default: ${it.exprToString()}" } ?: "") +
                "\n$prefix)"

        is SwitchCase -> "${prefix}SwitchCase($value: ${result.exprToString()})"
        is Cast -> "${prefix}Cast($ident as ${type.toTypeString()})"
        is PostfixInc -> "${prefix}PostfixInc(${target.exprToString()}++)"
        is PostfixDec -> "${prefix}PostfixDec(${target.exprToString()}--)"

        else -> "${prefix}${this::class.simpleName}"
    }
}

private fun TypeRef.toTypeString(): String = when (this) {
    is BuiltinType -> kind.name.lowercase()
    is NamedType -> name
    is PointerType -> "${base.toTypeString()}${"*".repeat(levels)}"
    is FuncType -> "fn(${paramTypes.joinToString { it.toTypeString() }}) -> ${returnType.toTypeString()}"
}

private fun Expr.exprToString(): String = when (this) {
    is IntLit -> value.toString()
    is F64Lit -> value.toString()
    is CharLit -> "'$value'"
    is StringLit -> "\"$value\""
    is Ident -> name
    is Unary -> "$op${exprToString()}"
    is Binary -> "(${left.exprToString()} $op ${right.exprToString()})"
    is Call -> "${callee.exprToString()}(${args.joinToString { it.exprToString() }})"
    is Index -> "${target.exprToString()}[${index.exprToString()}]"
    is Member -> "${target.exprToString()}${if (viaArrow) "->" else "."}$name"
    is Assign -> "${target.exprToString()} = ${value.exprToString()}"
    is BlockExpr -> "{...}"
    is IfExpr -> "if(...)"
    is WhileExpr -> "while(...)"
    is DoWhileExpr -> "do...while(...)"
    is ForExpr -> "for(...)"
    is SwitchExpr -> "switch(...)"
    is Cast -> "($ident as ${type.toTypeString()})"
    is PostfixInc -> "${target.exprToString()}++"
    is PostfixDec -> "${target.exprToString()}--"
}
