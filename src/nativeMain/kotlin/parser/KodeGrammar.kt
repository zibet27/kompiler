package parser

import lexer.TokenType
import parser.generator.Grammar
import parser.generator.grammar
import parser.generator.nt
import parser.generator.t

object KodeGrammar {
    object NT {
        val Program = "Program".nt
        val TopDeclList = "TopDeclList".nt
        val TopDecl = "TopDecl".nt
        val FunHeader = "FunHeader".nt
        val FunBody = "FunBody".nt
        val AlienFunDecl = "AlienFunDecl".nt
        val RetTypeOpt = "RetTypeOpt".nt
        val ObjectHeader = "ObjectHeader".nt
        val ObjectBody = "ObjectBody".nt
        val FieldDeclsOpt = "FieldDeclsOpt".nt
        val FieldDecls = "FieldDecls".nt
        val FieldDecl = "FieldDecl".nt
        val TypeAlias = "TypeAlias".nt
        val ParamTypeListOpt = "ParamTypeListOpt".nt
        val ParamTypeList = "ParamTypeList".nt
        val GlobalVarDecl = "GlobalVarDecl".nt
        val DeclaratorList = "DeclaratorList".nt
        val Declarator = "Declarator".nt
        val ArrayDimensionsOpt = "ArrayDimensionsOpt".nt
        val ArrayDimensions = "ArrayDimensions".nt
        val InitOpt = "InitOpt".nt
        val ParamsOpt = "ParamsOpt".nt
        val Params = "Params".nt
        val ParamList = "ParamList".nt
        val Param = "Param".nt
        val Type = "Type".nt
        val NonFuncType = "NonFuncType".nt
        val PrimaryType = "PrimaryType".nt
        val BaseType = "BaseType".nt
        val ItemListOpt = "ItemListOpt".nt
        val ItemList = "ItemList".nt
        val Item = "Item".nt
        val LocalVarDecl = "LocalVarDecl".nt
        val BlockExpr = "BlockExpr".nt
        val IfExpr = "IfExpr".nt
        val WhileExpr = "WhileExpr".nt
        val DoWhileExpr = "DoWhileExpr".nt
        val ForExpr = "ForExpr".nt
        val ForInitOpt = "ForInitOpt".nt
        val ForCondOpt = "ForCondOpt".nt
        val ForIncrOpt = "ForIncrOpt".nt
        val Expr = "Expr".nt
        val AssignExpr = "AssignExpr".nt
        val AssignOp = "AssignOp".nt
        val LogicalOrExpr = "LogicalOrExpr".nt
        val LogicalAndExpr = "LogicalAndExpr".nt
        val BitwiseOrExpr = "BitwiseOrExpr".nt
        val BitwiseXorExpr = "BitwiseXorExpr".nt
        val BitwiseAndExpr = "BitwiseAndExpr".nt
        val EqualityExpr = "EqualityExpr".nt
        val RelationalExpr = "RelationalExpr".nt
        val ShiftExpr = "ShiftExpr".nt
        val AdditiveExpr = "AdditiveExpr".nt
        val MultiplicativeExpr = "MultiplicativeExpr".nt
        val UnaryExpr = "UnaryExpr".nt
        val PrefixOp = "PrefixOp".nt
        val PostfixExpr = "PostfixExpr".nt
        val PrimaryExpr = "PrimaryExpr".nt
        val GroupedExpr = "GroupedExpr".nt
        val ArgsOpt = "ArgsOpt".nt
        val ArgList = "ArgList".nt
        val SwitchExpr = "SwitchExpr".nt
        val CaseListOpt = "CaseListOpt".nt
        val CaseList = "CaseList".nt
        val Case = "Case".nt
        val DefaultCaseOpt = "DefaultCaseOpt".nt
    }
    fun build(): Grammar = grammar(startSymbol = NT.Program) {
        // Program and top-level declarations
        rule(lhs = NT.Program, NT.TopDeclList)
        rule(lhs = NT.TopDeclList, NT.TopDeclList, NT.TopDecl)
        rule(lhs = NT.TopDeclList)

        rule(lhs = NT.TopDecl, NT.FunHeader, NT.FunBody)
        rule(lhs = NT.TopDecl, NT.AlienFunDecl)
        rule(lhs = NT.TopDecl, NT.ObjectHeader, NT.ObjectBody)
        rule(lhs = NT.TopDecl, NT.TypeAlias)
        rule(lhs = NT.TopDecl, NT.GlobalVarDecl)

        // FunHeader -> fun IDENT { ParamsOpt } RetTypeOpt
        rule(
            lhs = NT.FunHeader,
            TokenType.FUN.t,
            TokenType.IDENT.t,
            TokenType.LBRACE.t, NT.ParamsOpt, TokenType.RBRACE.t,
            NT.RetTypeOpt
        )
        // FunBody -> ; | ( ItemListOpt ) ;
        rule(lhs = NT.FunBody, TokenType.SEMICOLON.t)
        rule(
            lhs = NT.FunBody,
            TokenType.LPAREN.t, NT.ItemListOpt, TokenType.RPAREN.t,
            TokenType.SEMICOLON.t
        )

        // alien fun name{ params? } (':' Type)? ;
        rule(
            lhs = NT.AlienFunDecl,
            TokenType.ALIEN.t, TokenType.FUN.t, TokenType.IDENT.t,
            TokenType.LBRACE.t, NT.ParamsOpt, TokenType.RBRACE.t,
            NT.RetTypeOpt, TokenType.SEMICOLON.t
        )
        rule(lhs = NT.RetTypeOpt, TokenType.COLON.t, NT.Type)
        rule(lhs = NT.RetTypeOpt)

        // ObjectHeader -> object IDENT | object TYPENAME (for forward-declared types)
        rule(lhs = NT.ObjectHeader, TokenType.OBJECT.t, TokenType.IDENT.t)
        rule(lhs = NT.ObjectHeader, TokenType.OBJECT.t, TokenType.TYPENAME.t)
        // ObjectBody -> ; | ( FieldDeclsOpt ) ;
        rule(lhs = NT.ObjectBody, TokenType.SEMICOLON.t)
        rule(
            lhs = NT.ObjectBody,
            TokenType.LPAREN.t, NT.FieldDeclsOpt, TokenType.RPAREN.t,
            TokenType.SEMICOLON.t
        )
        rule(lhs = NT.FieldDeclsOpt, NT.FieldDecls)
        rule(lhs = NT.FieldDeclsOpt)
        rule(lhs = NT.FieldDecls, NT.FieldDecls, NT.FieldDecl)
        rule(lhs = NT.FieldDecls, NT.FieldDecl)
        rule(lhs = NT.FieldDecl, TokenType.IDENT.t, TokenType.COLON.t, NT.Type, TokenType.SEMICOLON.t)

        // typealias Name = Type ;
        rule(
            lhs = NT.TypeAlias,
            TokenType.TYPEALIAS.t, TokenType.IDENT.t, TokenType.ASSIGN.t,
            NT.Type, TokenType.SEMICOLON.t
        )

        // Globals / local declarations
        rule(lhs = NT.GlobalVarDecl, NT.Type, NT.DeclaratorList, TokenType.SEMICOLON.t)
        rule(lhs = NT.DeclaratorList, NT.DeclaratorList, TokenType.COMMA.t, NT.Declarator)
        rule(lhs = NT.DeclaratorList, NT.Declarator)
        rule(lhs = NT.Declarator, TokenType.IDENT.t, NT.ArrayDimensionsOpt, NT.InitOpt)
        rule(lhs = NT.ArrayDimensionsOpt, NT.ArrayDimensions)
        rule(lhs = NT.ArrayDimensionsOpt)
        rule(lhs = NT.ArrayDimensions, NT.ArrayDimensions, TokenType.LBRACKET.t, NT.Expr, TokenType.RBRACKET.t)
        rule(lhs = NT.ArrayDimensions, TokenType.LBRACKET.t, NT.Expr, TokenType.RBRACKET.t)
        rule(lhs = NT.InitOpt, TokenType.WITH.t, NT.Expr)
        rule(lhs = NT.InitOpt, TokenType.ASSIGN.t, NT.Expr)
        rule(lhs = NT.InitOpt)

        // Parameters and types
        rule(lhs = NT.ParamsOpt, NT.Params)
        rule(lhs = NT.ParamsOpt)
        rule(lhs = NT.Params, NT.ParamList)
        rule(lhs = NT.ParamList, NT.ParamList, TokenType.COMMA.t, NT.Param)
        rule(lhs = NT.ParamList, NT.Param)
        rule(lhs = NT.Param, TokenType.IDENT.t, TokenType.COLON.t, NT.Type)

        // Type -> NonFuncType | { ParamTypeListOpt } -> Type
        rule(lhs = NT.Type, NT.NonFuncType)
        rule(
            lhs = NT.Type,
            TokenType.LBRACE.t, NT.ParamTypeListOpt, TokenType.RBRACE.t,
            TokenType.ARROW.t, NT.Type
        )
        // NonFuncType -> PrimaryType | NonFuncType ptr
        rule(lhs = NT.NonFuncType, NT.PrimaryType)
        rule(lhs = NT.NonFuncType, NT.NonFuncType, TokenType.PTR.t)
        // PrimaryType -> BaseType | TYPENAME | ( Type )
        rule(lhs = NT.PrimaryType, NT.BaseType)
        rule(lhs = NT.PrimaryType, TokenType.TYPENAME.t)
        rule(lhs = NT.PrimaryType, TokenType.LPAREN.t, NT.Type, TokenType.RPAREN.t)
        // BaseType
        rule(lhs = NT.BaseType, TokenType.VOID.t)
        rule(lhs = NT.BaseType, TokenType.I32.t)
        rule(lhs = NT.BaseType, TokenType.U8.t)
        rule(lhs = NT.BaseType, TokenType.F64.t)

        rule(lhs = NT.ParamTypeListOpt, NT.ParamTypeList)
        rule(lhs = NT.ParamTypeListOpt)
        rule(lhs = NT.ParamTypeList, NT.ParamTypeList, TokenType.COMMA.t, NT.Type)
        rule(lhs = NT.ParamTypeList, NT.Type)

        // Block expression items (declarations and expression statements)
        rule(lhs = NT.ItemListOpt, NT.ItemList)
        rule(lhs = NT.ItemListOpt)
        rule(lhs = NT.ItemList, NT.ItemList, NT.Item)
        rule(lhs = NT.ItemList, NT.Item)
        rule(lhs = NT.Item, NT.LocalVarDecl, TokenType.SEMICOLON.t)
        rule(lhs = NT.Item, NT.Expr, TokenType.SEMICOLON.t)
        rule(lhs = NT.Item, TokenType.SKIP.t, TokenType.SEMICOLON.t)
        rule(lhs = NT.Item, TokenType.STOP.t, TokenType.SEMICOLON.t)

        rule(lhs = NT.LocalVarDecl, NT.Type, NT.DeclaratorList)

        // Block expression now yields a value: ( items )
        rule(lhs = NT.BlockExpr, TokenType.LPAREN.t, NT.ItemListOpt, TokenType.RPAREN.t)

        // if-expression returns the value of the chosen block
        rule(
            lhs = NT.IfExpr,
            TokenType.IF.t,
            TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t,
            NT.BlockExpr,
            TokenType.ELSE.t,
            NT.BlockExpr
        )
        // else-if chain (else followed by another if)
        rule(
            lhs = NT.IfExpr,
            TokenType.IF.t,
            TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t,
            NT.BlockExpr,
            TokenType.ELSE.t,
            NT.IfExpr
        )
        // if-without-else for statements
        rule(
            lhs = NT.IfExpr,
            TokenType.IF.t,
            TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t,
            NT.BlockExpr
        )

        // Loops as expressions
        rule(lhs = NT.WhileExpr, TokenType.WHILE.t, TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t, NT.BlockExpr)
        rule(
            lhs = NT.DoWhileExpr,
            TokenType.DO.t,
            NT.BlockExpr,
            TokenType.WHILE.t,
            TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t
        )
        rule(
            lhs = NT.ForExpr,
            TokenType.FOR.t, TokenType.LBRACE.t,
            NT.ForInitOpt, TokenType.SEMICOLON.t,
            NT.ForCondOpt, TokenType.SEMICOLON.t,
            NT.ForIncrOpt, TokenType.RBRACE.t,
            NT.BlockExpr
        )
        rule(lhs = NT.ForInitOpt, NT.LocalVarDecl)
        rule(lhs = NT.ForInitOpt, NT.Expr)
        rule(lhs = NT.ForInitOpt)
        rule(lhs = NT.ForCondOpt, NT.Expr)
        rule(lhs = NT.ForCondOpt)
        rule(lhs = NT.ForIncrOpt, NT.Expr)
        rule(lhs = NT.ForIncrOpt)

        // Expressions (precedence climbing via grammar levels)
        rule(lhs = NT.Expr, NT.AssignExpr)

        rule(lhs = NT.AssignExpr, NT.UnaryExpr, NT.AssignOp, NT.AssignExpr)
        rule(lhs = NT.AssignExpr, NT.LogicalOrExpr)
        rule(lhs = NT.AssignOp, TokenType.ASSIGN.t)

        rule(lhs = NT.LogicalOrExpr, NT.LogicalOrExpr, TokenType.OR_OR.t, NT.LogicalAndExpr)
        rule(lhs = NT.LogicalOrExpr, NT.LogicalAndExpr)
        rule(lhs = NT.LogicalAndExpr, NT.LogicalAndExpr, TokenType.AND_AND.t, NT.BitwiseOrExpr)
        rule(lhs = NT.LogicalAndExpr, NT.BitwiseOrExpr)
        rule(lhs = NT.BitwiseOrExpr, NT.BitwiseOrExpr, TokenType.PIPE.t, NT.BitwiseXorExpr)
        rule(lhs = NT.BitwiseOrExpr, NT.BitwiseXorExpr)
        rule(lhs = NT.BitwiseXorExpr, NT.BitwiseXorExpr, TokenType.CARET.t, NT.BitwiseAndExpr)
        rule(lhs = NT.BitwiseXorExpr, NT.BitwiseAndExpr)
        rule(lhs = NT.BitwiseAndExpr, NT.BitwiseAndExpr, TokenType.AMP.t, NT.EqualityExpr)
        rule(lhs = NT.BitwiseAndExpr, NT.EqualityExpr)
        rule(lhs = NT.EqualityExpr, NT.EqualityExpr, TokenType.EQ_EQ.t, NT.RelationalExpr)
        rule(lhs = NT.EqualityExpr, NT.EqualityExpr, TokenType.BANG_EQ.t, NT.RelationalExpr)
        rule(lhs = NT.EqualityExpr, NT.RelationalExpr)
        rule(lhs = NT.RelationalExpr, NT.RelationalExpr, TokenType.LT.t, NT.ShiftExpr)
        rule(lhs = NT.RelationalExpr, NT.RelationalExpr, TokenType.GT.t, NT.ShiftExpr)
        rule(lhs = NT.RelationalExpr, NT.RelationalExpr, TokenType.LTE.t, NT.ShiftExpr)
        rule(lhs = NT.RelationalExpr, NT.RelationalExpr, TokenType.GTE.t, NT.ShiftExpr)
        rule(lhs = NT.RelationalExpr, NT.ShiftExpr)
        rule(lhs = NT.ShiftExpr, NT.ShiftExpr, TokenType.SHL.t, NT.AdditiveExpr)
        rule(lhs = NT.ShiftExpr, NT.ShiftExpr, TokenType.SHR.t, NT.AdditiveExpr)
        rule(lhs = NT.ShiftExpr, NT.AdditiveExpr)
        rule(lhs = NT.AdditiveExpr, NT.AdditiveExpr, TokenType.PLUS.t, NT.MultiplicativeExpr)
        rule(lhs = NT.AdditiveExpr, NT.AdditiveExpr, TokenType.MINUS.t, NT.MultiplicativeExpr)
        rule(lhs = NT.AdditiveExpr, NT.MultiplicativeExpr)
        rule(lhs = NT.MultiplicativeExpr, NT.MultiplicativeExpr, TokenType.STAR.t, NT.UnaryExpr)
        rule(lhs = NT.MultiplicativeExpr, NT.MultiplicativeExpr, TokenType.SLASH.t, NT.UnaryExpr)
        rule(lhs = NT.MultiplicativeExpr, NT.MultiplicativeExpr, TokenType.PERCENT.t, NT.UnaryExpr)
        rule(lhs = NT.MultiplicativeExpr, NT.UnaryExpr)

        rule(lhs = NT.UnaryExpr, NT.PostfixExpr)
        rule(lhs = NT.UnaryExpr, NT.PrefixOp, NT.UnaryExpr)
        rule(lhs = NT.PrefixOp, TokenType.PLUSPLUS.t)
        rule(lhs = NT.PrefixOp, TokenType.MINUS_MINUS.t)
        rule(lhs = NT.PrefixOp, TokenType.AMP.t)
        rule(lhs = NT.PrefixOp, TokenType.STAR.t)
        rule(lhs = NT.PrefixOp, TokenType.PLUS.t)
        rule(lhs = NT.PrefixOp, TokenType.MINUS.t)
        rule(lhs = NT.PrefixOp, TokenType.BANG.t)
        rule(lhs = NT.PrefixOp, TokenType.TILDE.t)

        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.LBRACKET.t, NT.Expr, TokenType.RBRACKET.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.LBRACE.t, NT.ArgsOpt, TokenType.RBRACE.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.DOT.t, TokenType.IDENT.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.DOT.t, TokenType.TYPENAME.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.ARROW.t, TokenType.IDENT.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.ARROW.t, TokenType.TYPENAME.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.PLUSPLUS.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.MINUS_MINUS.t)
        rule(lhs = NT.PostfixExpr, NT.PostfixExpr, TokenType.TILDE_GT.t, NT.Type)
        rule(lhs = NT.PostfixExpr, NT.PrimaryExpr)

        rule(lhs = NT.ArgsOpt, NT.ArgList)
        rule(lhs = NT.ArgsOpt)
        rule(lhs = NT.ArgList, NT.ArgList, TokenType.COMMA.t, NT.Expr)
        rule(lhs = NT.ArgList, NT.Expr)

        rule(lhs = NT.PrimaryExpr, TokenType.IDENT.t)
        rule(lhs = NT.PrimaryExpr, TokenType.INT.t)
        rule(lhs = NT.PrimaryExpr, TokenType.FLOAT.t)
        rule(lhs = NT.PrimaryExpr, TokenType.CHAR_LIT.t)
        rule(lhs = NT.PrimaryExpr, TokenType.STRING_LIT.t)
        rule(lhs = NT.PrimaryExpr, TokenType.LPAREN.t, NT.Expr, TokenType.RPAREN.t)
        rule(lhs = NT.PrimaryExpr, NT.GroupedExpr)
        rule(lhs = NT.PrimaryExpr, NT.BlockExpr)
        rule(lhs = NT.PrimaryExpr, NT.IfExpr)
        rule(lhs = NT.PrimaryExpr, NT.WhileExpr)
        rule(lhs = NT.PrimaryExpr, NT.DoWhileExpr)
        rule(lhs = NT.PrimaryExpr, NT.ForExpr)
        rule(lhs = NT.PrimaryExpr, NT.SwitchExpr)

        rule(lhs = NT.GroupedExpr, TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t)

        rule(
            lhs = NT.SwitchExpr,
            TokenType.SWITCH.t, TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t,
            TokenType.LPAREN.t, NT.CaseListOpt, NT.DefaultCaseOpt, TokenType.RPAREN.t
        )
        rule(lhs = NT.CaseListOpt, NT.CaseList)
        rule(lhs = NT.CaseListOpt)
        rule(lhs = NT.CaseList, NT.CaseList, NT.Case)
        rule(lhs = NT.CaseList, NT.Case)
        rule(lhs = NT.Case, TokenType.INT.t, TokenType.ARROW.t, NT.Expr, TokenType.SEMICOLON.t)
        rule(lhs = NT.DefaultCaseOpt, TokenType.ELSE.t, TokenType.ARROW.t, NT.Expr, TokenType.SEMICOLON.t)
        rule(lhs = NT.DefaultCaseOpt)
    }
}
