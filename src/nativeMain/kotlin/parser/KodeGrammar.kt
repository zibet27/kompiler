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
        val FunDef = "FunDef".nt
        val FunDecl = "FunDecl".nt
        val AlienFunDecl = "AlienFunDecl".nt
        val RetTypeOpt = "RetTypeOpt".nt
        val ObjectDecl = "ObjectDecl".nt
        val ObjectDef = "ObjectDef".nt
        val FieldDeclsOpt = "FieldDeclsOpt".nt
        val FieldDecls = "FieldDecls".nt
        val FieldDecl = "FieldDecl".nt
        val TypeAlias = "TypeAlias".nt
        val ParamTypeListOpt = "ParamTypeListOpt".nt
        val ParamTypeList = "ParamTypeList".nt
        val GlobalVarDecl = "GlobalVarDecl".nt
        val DeclaratorList = "DeclaratorList".nt
        val Declarator = "Declarator".nt
        val ArraySuffixOpt = "ArraySuffixOpt".nt
        val InitOpt = "InitOpt".nt
        val ParamsOpt = "ParamsOpt".nt
        val Params = "Params".nt
        val ParamList = "ParamList".nt
        val Param = "Param".nt
        val Type = "Type".nt
        val BaseType = "BaseType".nt
        val TypeSuffix = "TypeSuffix".nt
        val FuncType = "FuncType".nt
        val ItemListOpt = "ItemListOpt".nt
        val ItemList = "ItemList".nt
        val Item = "Item".nt
        val LocalVarDecl = "LocalVarDecl".nt
        val BlockExpr = "BlockExpr".nt
        val IfExpr = "IfExpr".nt
        val WhileStmt = "WhileStmt".nt
        val DoWhileStmt = "DoWhileStmt".nt
        val ForStmt = "ForStmt".nt
        val ForInitOpt = "ForInitOpt".nt
        val ForCondOpt = "ForCondOpt".nt
        val ForIncrOpt = "ForIncrOpt".nt
        val Expr = "Expr".nt
        val Assign = "Assign".nt
        val LValue = "LValue".nt
        val OrExpr = "OrExpr".nt
        val AndExpr = "AndExpr".nt
        val BitOrExpr = "BitOrExpr".nt
        val BitXorExpr = "BitXorExpr".nt
        val BitAndExpr = "BitAndExpr".nt
        val Equality = "Equality".nt
        val Rel = "Rel".nt
        val Shift = "Shift".nt
        val Add = "Add".nt
        val Mul = "Mul".nt
        val Factor = "Factor".nt
        val Prefixed = "Prefixed".nt
        val Postfix = "Postfix".nt
        val Simple = "Simple".nt
        val CastExpr = "CastExpr".nt
        val ArgsOpt = "ArgsOpt".nt
        val ArgList = "ArgList".nt
        val Primary = "Primary".nt
        val PrimaryNoIdent = "PrimaryNoIdent".nt
        val SwitchExpr = "SwitchExpr".nt
        val CaseList = "CaseList".nt
        val Case = "Case".nt
        val DefaultCase = "DefaultCase".nt
    }
    fun build(): Grammar = grammar(startSymbol = NT.Program) {
        // Program and top-level declarations
        rule(lhs = NT.Program, NT.TopDeclList)
        rule(lhs = NT.TopDeclList, NT.TopDeclList, NT.TopDecl)
        rule(lhs = NT.TopDeclList, NT.TopDecl)

        rule(lhs = NT.TopDecl, NT.FunDef)
        rule(lhs = NT.TopDecl, NT.FunDecl)
        rule(lhs = NT.TopDecl, NT.AlienFunDecl)
        rule(lhs = NT.TopDecl, NT.ObjectDecl)
        rule(lhs = NT.TopDecl, NT.ObjectDef)
        rule(lhs = NT.TopDecl, NT.TypeAlias)
        rule(lhs = NT.TopDecl, NT.GlobalVarDecl)

        // fun name{ params? }: Type ( items? ) ;  — body is a block-expression
        rule(
            lhs = NT.FunDef,
            TokenType.FUN.t,
            TokenType.IDENT.t,
            TokenType.LBRACE.t, NT.ParamsOpt, TokenType.RBRACE.t,
            TokenType.COLON.t, NT.Type,
            TokenType.LPAREN.t, NT.ItemListOpt, TokenType.RPAREN.t,
            TokenType.SEMICOLON.t
        )

        // fun name{ params? }: Type ;
        rule(
            lhs = NT.FunDecl,
            TokenType.FUN.t,
            TokenType.IDENT.t,
            TokenType.LBRACE.t, NT.ParamsOpt, TokenType.RBRACE.t,
            TokenType.COLON.t, NT.Type,
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

        // object IDENT ;
        rule(lhs = NT.ObjectDecl, TokenType.OBJECT.t, TokenType.IDENT.t, TokenType.SEMICOLON.t)
        // object IDENT ( FieldDeclsOpt ) ;
        rule(
            lhs = NT.ObjectDef,
            TokenType.OBJECT.t, TokenType.IDENT.t,
            TokenType.LPAREN.t, NT.FieldDeclsOpt, TokenType.RPAREN.t,
            TokenType.SEMICOLON.t
        )
        rule(lhs = NT.FieldDeclsOpt, NT.FieldDecls)
        rule(lhs = NT.FieldDeclsOpt)
        rule(lhs = NT.FieldDecls, NT.FieldDecls, NT.FieldDecl)
        rule(lhs = NT.FieldDecls, NT.FieldDecl)
        rule(lhs = NT.FieldDecl, TokenType.IDENT.t, TokenType.COLON.t, NT.Type, TokenType.SEMICOLON.t)

        // typealias Name = { ParamTypeListOpt } -> Type ;
        rule(
            lhs = NT.TypeAlias,
            TokenType.TYPEALIAS.t, TokenType.IDENT.t, TokenType.ASSIGN.t,
            TokenType.LBRACE.t, NT.ParamTypeListOpt, TokenType.RBRACE.t,
            TokenType.ARROW.t, NT.Type, TokenType.SEMICOLON.t
        )
        rule(lhs = NT.ParamTypeListOpt, NT.ParamTypeList)
        rule(lhs = NT.ParamTypeListOpt)
        rule(lhs = NT.ParamTypeList, NT.ParamTypeList, TokenType.COMMA.t, NT.Type)
        rule(lhs = NT.ParamTypeList, NT.Type)

        // Globals / local declarations
        rule(lhs = NT.GlobalVarDecl, NT.Type, NT.DeclaratorList, TokenType.SEMICOLON.t)
        rule(lhs = NT.DeclaratorList, NT.DeclaratorList, TokenType.COMMA.t, NT.Declarator)
        rule(lhs = NT.DeclaratorList, NT.Declarator)
        rule(lhs = NT.Declarator, TokenType.IDENT.t, NT.ArraySuffixOpt, NT.InitOpt)
        rule(lhs = NT.ArraySuffixOpt, NT.ArraySuffixOpt, TokenType.LBRACKET.t, NT.Expr, TokenType.RBRACKET.t)
        rule(lhs = NT.ArraySuffixOpt)
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

        rule(lhs = NT.Type, NT.BaseType, NT.TypeSuffix)
        rule(lhs = NT.Type, TokenType.IDENT.t, NT.TypeSuffix)
        rule(lhs = NT.Type, NT.FuncType)
        rule(lhs = NT.TypeSuffix, NT.TypeSuffix, TokenType.PTR.t)
        rule(lhs = NT.TypeSuffix)
        rule(lhs = NT.BaseType, TokenType.VOID.t)
        rule(lhs = NT.BaseType, TokenType.I32.t)
        rule(lhs = NT.BaseType, TokenType.U8.t)
        rule(lhs = NT.BaseType, TokenType.F64.t)
        rule(
            lhs = NT.FuncType,
            TokenType.LBRACE.t,
            NT.ParamTypeListOpt,
            TokenType.RBRACE.t,
            TokenType.ARROW.t,
            NT.Type
        )

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

        // While/Do/For are statement-forms that can appear as items within blocks
        rule(lhs = NT.Item, NT.WhileStmt)
        rule(lhs = NT.Item, NT.DoWhileStmt)
        rule(lhs = NT.Item, NT.ForStmt)

        // Block expression now yields a value: ( items )
        rule(lhs = NT.BlockExpr, TokenType.LPAREN.t, NT.ItemListOpt, TokenType.RPAREN.t)

        // if-expression returns value of the chosen block
        rule(
            lhs = NT.IfExpr,
            TokenType.IF.t,
            TokenType.LBRACE.t,
            NT.Expr,
            TokenType.RBRACE.t,
            NT.BlockExpr,
            TokenType.ELSE.t,
            NT.BlockExpr
        )

        // Loops (as items)
        rule(lhs = NT.WhileStmt, TokenType.WHILE.t, TokenType.LBRACE.t, NT.Expr, TokenType.RBRACE.t, NT.BlockExpr)
        rule(
            lhs = NT.DoWhileStmt,
            TokenType.DO.t,
            NT.BlockExpr,
            TokenType.WHILE.t,
            TokenType.LBRACE.t,
            NT.Expr,
            TokenType.RBRACE.t,
            TokenType.SEMICOLON.t
        )
        rule(
            lhs = NT.ForStmt,
            TokenType.FOR.t, TokenType.LBRACE.t,
            NT.ForInitOpt, TokenType.SEMICOLON.t,
            NT.ForCondOpt, TokenType.SEMICOLON.t,
            NT.ForIncrOpt, TokenType.RBRACE.t,
            NT.BlockExpr, TokenType.SEMICOLON.t
        )
        rule(lhs = NT.ForInitOpt, NT.LocalVarDecl)
        rule(lhs = NT.ForInitOpt, NT.Expr)
        rule(lhs = NT.ForInitOpt)
        rule(lhs = NT.ForCondOpt, NT.Expr)
        rule(lhs = NT.ForCondOpt)
        rule(lhs = NT.ForIncrOpt, NT.Expr)
        rule(lhs = NT.ForIncrOpt)

        // Expressions (precedence climbing via grammar levels)
        rule(lhs = NT.Expr, NT.Assign)
        rule(lhs = NT.Assign, NT.LValue, TokenType.ASSIGN.t, NT.Assign)
        rule(lhs = NT.Assign, NT.OrExpr)
        rule(lhs = NT.LValue, NT.Postfix)

        rule(lhs = NT.OrExpr, NT.OrExpr, TokenType.OR_OR.t, NT.AndExpr)
        rule(lhs = NT.OrExpr, NT.AndExpr)
        rule(lhs = NT.AndExpr, NT.AndExpr, TokenType.AND_AND.t, NT.BitOrExpr)
        rule(lhs = NT.AndExpr, NT.BitOrExpr)
        rule(lhs = NT.BitOrExpr, NT.BitOrExpr, TokenType.PIPE.t, NT.BitXorExpr)
        rule(lhs = NT.BitOrExpr, NT.BitXorExpr)
        rule(lhs = NT.BitXorExpr, NT.BitXorExpr, TokenType.CARET.t, NT.BitAndExpr)
        rule(lhs = NT.BitXorExpr, NT.BitAndExpr)
        rule(lhs = NT.BitAndExpr, NT.BitAndExpr, TokenType.AMP.t, NT.Equality)
        rule(lhs = NT.BitAndExpr, NT.Equality)
        rule(lhs = NT.Equality, NT.Equality, TokenType.EQ_EQ.t, NT.Rel)
        rule(lhs = NT.Equality, NT.Equality, TokenType.BANG_EQ.t, NT.Rel)
        rule(lhs = NT.Equality, NT.Rel)
        rule(lhs = NT.Rel, NT.Rel, TokenType.LT.t, NT.Shift)
        rule(lhs = NT.Rel, NT.Rel, TokenType.GT.t, NT.Shift)
        rule(lhs = NT.Rel, NT.Rel, TokenType.LTE.t, NT.Shift)
        rule(lhs = NT.Rel, NT.Rel, TokenType.GTE.t, NT.Shift)
        rule(lhs = NT.Rel, NT.Shift)
        rule(lhs = NT.Shift, NT.Shift, TokenType.SHL.t, NT.Add)
        rule(lhs = NT.Shift, NT.Shift, TokenType.SHR.t, NT.Add)
        rule(lhs = NT.Shift, NT.Add)
        rule(lhs = NT.Add, NT.Add, TokenType.PLUS.t, NT.Mul)
        rule(lhs = NT.Add, NT.Add, TokenType.MINUS.t, NT.Mul)
        rule(lhs = NT.Add, NT.Mul)
        rule(lhs = NT.Mul, NT.Mul, TokenType.STAR.t, NT.Factor)
        rule(lhs = NT.Mul, NT.Mul, TokenType.SLASH.t, NT.Factor)
        rule(lhs = NT.Mul, NT.Mul, TokenType.PERCENT.t, NT.Factor)
        rule(lhs = NT.Mul, NT.Factor)

        // Factor splits pre-fixed forms from postfix/base forms (no Prefix→Postfix reduce)
        rule(lhs = NT.Factor, NT.Postfix)
        rule(lhs = NT.Factor, NT.Prefixed)

        rule(lhs = NT.Prefixed, TokenType.BANG.t, NT.Factor)
        rule(lhs = NT.Prefixed, TokenType.TILDE.t, NT.Factor)
        rule(lhs = NT.Prefixed, TokenType.PLUS.t, NT.Factor)
        rule(lhs = NT.Prefixed, TokenType.MINUS.t, NT.Factor)
        rule(lhs = NT.Prefixed, TokenType.PLUSPLUS.t, NT.Factor)
        rule(lhs = NT.Prefixed, TokenType.MINUS_MINUS.t, NT.Factor)
        rule(lhs = NT.Prefixed, TokenType.STAR.t, NT.Factor) // deref
        rule(lhs = NT.Prefixed, TokenType.AMP.t, NT.Factor)  // address-of

        // Left-recursive postfix without epsilon tails; includes binary cast IDENT ~> Type
        rule(lhs = NT.Postfix, NT.Postfix, TokenType.PLUSPLUS.t)
        rule(lhs = NT.Postfix, NT.Postfix, TokenType.MINUS_MINUS.t)
        rule(lhs = NT.Postfix, NT.Postfix, TokenType.LBRACKET.t, NT.Expr, TokenType.RBRACKET.t)
        rule(lhs = NT.Postfix, NT.Postfix, TokenType.DOT.t, TokenType.IDENT.t)
        rule(lhs = NT.Postfix, NT.Postfix, TokenType.ARROW.t, TokenType.IDENT.t)
        rule(lhs = NT.Postfix, NT.Postfix, TokenType.LBRACE.t, NT.ArgsOpt, TokenType.RBRACE.t)
        rule(lhs = NT.Postfix, NT.Simple)

        rule(lhs = NT.Simple, NT.CastExpr)
        rule(lhs = NT.Simple, TokenType.IDENT.t)
        rule(lhs = NT.Simple, NT.PrimaryNoIdent)

        rule(lhs = NT.CastExpr, TokenType.IDENT.t, TokenType.TILDE_GT.t, NT.Type) // IDENT ~> Type
        rule(lhs = NT.ArgsOpt, NT.ArgList)
        rule(lhs = NT.ArgsOpt)
        rule(lhs = NT.ArgList, NT.ArgList, TokenType.COMMA.t, NT.Expr)
        rule(lhs = NT.ArgList, NT.Expr)

        // Primaries including expression blocks and if-expression (IDENT handled by Postfix)
        rule(lhs = NT.Primary, NT.BlockExpr)
        rule(lhs = NT.Primary, NT.IfExpr)
        rule(lhs = NT.Primary, TokenType.LPAREN.t, NT.Expr, TokenType.RPAREN.t)
        rule(lhs = NT.Primary, TokenType.IDENT.t)
        rule(lhs = NT.Primary, TokenType.INT.t)
        rule(lhs = NT.Primary, TokenType.CHAR_LIT.t)
        rule(lhs = NT.Primary, TokenType.STRING_LIT.t)
        rule(lhs = NT.Primary, NT.SwitchExpr)

        // Primary without IDENT (used to separate cast forms)
        rule(lhs = NT.PrimaryNoIdent, NT.BlockExpr)
        rule(lhs = NT.PrimaryNoIdent, NT.IfExpr)
        rule(lhs = NT.PrimaryNoIdent, TokenType.LPAREN.t, NT.Expr, TokenType.RPAREN.t)
        rule(lhs = NT.PrimaryNoIdent, TokenType.INT.t)
        rule(lhs = NT.PrimaryNoIdent, TokenType.CHAR_LIT.t)
        rule(lhs = NT.PrimaryNoIdent, TokenType.STRING_LIT.t)
        rule(lhs = NT.PrimaryNoIdent, NT.SwitchExpr)

        // switch scrutinee is a general expression, cases are integer literals (docs-aligned)
        rule(
            lhs = NT.SwitchExpr,
            TokenType.SWITCH.t, TokenType.LPAREN.t, NT.Expr, TokenType.RPAREN.t,
            TokenType.LBRACE.t, NT.CaseList, NT.DefaultCase, TokenType.RBRACE.t
        )
        rule(lhs = NT.CaseList, NT.CaseList, NT.Case)
        rule(lhs = NT.CaseList)
        rule(lhs = NT.Case, TokenType.INT.t, TokenType.ARROW.t, NT.Expr, TokenType.SEMICOLON.t)
        rule(lhs = NT.DefaultCase, TokenType.ELSE.t, TokenType.ARROW.t, NT.Expr, TokenType.SEMICOLON.t)
    }
}
