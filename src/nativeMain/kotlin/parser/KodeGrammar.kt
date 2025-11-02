package parser

import lexer.TokenType
import parser.generator.Grammar
import parser.generator.grammar
import parser.generator.nt
import parser.generator.t

object KodeGrammar {
    fun build(): Grammar = grammar(startSymbol = "Program".nt) {
        // Program and top-level declarations
        rule(lhs = "Program".nt, "TopDeclList".nt)
        rule(lhs = "TopDeclList".nt, "TopDeclList".nt, "TopDecl".nt)
        rule(lhs = "TopDeclList".nt, "TopDecl".nt)

        rule(lhs = "TopDecl".nt, "FunDef".nt)
        rule(lhs = "TopDecl".nt, "FunDecl".nt)
        rule(lhs = "TopDecl".nt, "AlienFunDecl".nt)
        rule(lhs = "TopDecl".nt, "ObjectDecl".nt)
        rule(lhs = "TopDecl".nt, "ObjectDef".nt)
        rule(lhs = "TopDecl".nt, "TypeAlias".nt)
        rule(lhs = "TopDecl".nt, "GlobalVarDecl".nt)

        // fun name{ params? }: Type ( items? ) ;  — body is a block-expression
        rule(
            lhs = "FunDef".nt,
            TokenType.FUN.t,
            TokenType.IDENT.t,
            TokenType.LBRACE.t, "ParamsOpt".nt, TokenType.RBRACE.t,
            TokenType.COLON.t, "Type".nt,
            TokenType.LPAREN.t, "ItemListOpt".nt, TokenType.RPAREN.t,
            TokenType.SEMICOLON.t
        )

        // fun name{ params? }: Type ;
        rule(
            lhs = "FunDecl".nt,
            TokenType.FUN.t,
            TokenType.IDENT.t,
            TokenType.LBRACE.t, "ParamsOpt".nt, TokenType.RBRACE.t,
            TokenType.COLON.t, "Type".nt,
            TokenType.SEMICOLON.t
        )

        // alien fun name{ params? } (':' Type)? ;
        rule(
            lhs = "AlienFunDecl".nt,
            TokenType.ALIEN.t, TokenType.FUN.t, TokenType.IDENT.t,
            TokenType.LBRACE.t, "ParamsOpt".nt, TokenType.RBRACE.t,
            "RetTypeOpt".nt, TokenType.SEMICOLON.t
        )
        rule(lhs = "RetTypeOpt".nt, TokenType.COLON.t, "Type".nt)
        rule(lhs = "RetTypeOpt".nt)

        // object IDENT ;
        rule(lhs = "ObjectDecl".nt, TokenType.OBJECT.t, TokenType.IDENT.t, TokenType.SEMICOLON.t)
        // object IDENT ( FieldDeclsOpt ) ;
        rule(
            lhs = "ObjectDef".nt,
            TokenType.OBJECT.t, TokenType.IDENT.t,
            TokenType.LPAREN.t, "FieldDeclsOpt".nt, TokenType.RPAREN.t,
            TokenType.SEMICOLON.t
        )
        rule(lhs = "FieldDeclsOpt".nt, "FieldDecls".nt)
        rule(lhs = "FieldDeclsOpt".nt)
        rule(lhs = "FieldDecls".nt, "FieldDecls".nt, "FieldDecl".nt)
        rule(lhs = "FieldDecls".nt, "FieldDecl".nt)
        rule(lhs = "FieldDecl".nt, TokenType.IDENT.t, TokenType.COLON.t, "Type".nt, TokenType.SEMICOLON.t)

        // typealias Name = { ParamTypeListOpt } -> Type ;
        rule(
            lhs = "TypeAlias".nt,
            TokenType.TYPEALIAS.t, TokenType.IDENT.t, TokenType.ASSIGN.t,
            TokenType.LBRACE.t, "ParamTypeListOpt".nt, TokenType.RBRACE.t,
            TokenType.ARROW.t, "Type".nt, TokenType.SEMICOLON.t
        )
        rule(lhs = "ParamTypeListOpt".nt, "ParamTypeList".nt)
        rule(lhs = "ParamTypeListOpt".nt)
        rule(lhs = "ParamTypeList".nt, "ParamTypeList".nt, TokenType.COMMA.t, "Type".nt)
        rule(lhs = "ParamTypeList".nt, "Type".nt)

        // Globals / local declarations
        rule(lhs = "GlobalVarDecl".nt, "Type".nt, "DeclaratorList".nt, TokenType.SEMICOLON.t)
        rule(lhs = "DeclaratorList".nt, "DeclaratorList".nt, TokenType.COMMA.t, "Declarator".nt)
        rule(lhs = "DeclaratorList".nt, "Declarator".nt)
        rule(lhs = "Declarator".nt, TokenType.IDENT.t, "ArraySuffixOpt".nt, "InitOpt".nt)
        rule(lhs = "ArraySuffixOpt".nt, "ArraySuffixOpt".nt, TokenType.LBRACKET.t, "Expr".nt, TokenType.RBRACKET.t)
        rule(lhs = "ArraySuffixOpt".nt)
        rule(lhs = "InitOpt".nt, TokenType.WITH.t, "Expr".nt)
        rule(lhs = "InitOpt".nt, TokenType.ASSIGN.t, "Expr".nt)
        rule(lhs = "InitOpt".nt)

        // Parameters and types
        rule(lhs = "ParamsOpt".nt, "Params".nt)
        rule(lhs = "ParamsOpt".nt)
        rule(lhs = "Params".nt, "ParamList".nt)
        rule(lhs = "ParamList".nt, "ParamList".nt, TokenType.COMMA.t, "Param".nt)
        rule(lhs = "ParamList".nt, "Param".nt)
        rule(lhs = "Param".nt, TokenType.IDENT.t, TokenType.COLON.t, "Type".nt)

        rule(lhs = "Type".nt, "BaseType".nt, "TypeSuffix".nt)
        rule(lhs = "Type".nt, TokenType.IDENT.t, "TypeSuffix".nt)
        rule(lhs = "Type".nt, "FuncType".nt)
        rule(lhs = "TypeSuffix".nt, "TypeSuffix".nt, TokenType.PTR.t)
        rule(lhs = "TypeSuffix".nt)
        rule(lhs = "BaseType".nt, TokenType.VOID.t)
        rule(lhs = "BaseType".nt, TokenType.I32.t)
        rule(lhs = "BaseType".nt, TokenType.U8.t)
        rule(lhs = "BaseType".nt, TokenType.F64.t)
        rule(
            lhs = "FuncType".nt,
            TokenType.LBRACE.t,
            "ParamTypeListOpt".nt,
            TokenType.RBRACE.t,
            TokenType.ARROW.t,
            "Type".nt
        )

        // Block expression items (declarations and expression statements)
        rule(lhs = "ItemListOpt".nt, "ItemList".nt)
        rule(lhs = "ItemListOpt".nt)
        rule(lhs = "ItemList".nt, "ItemList".nt, "Item".nt)
        rule(lhs = "ItemList".nt, "Item".nt)
        rule(lhs = "Item".nt, "LocalVarDecl".nt, TokenType.SEMICOLON.t)
        rule(lhs = "Item".nt, "Expr".nt, TokenType.SEMICOLON.t)
        rule(lhs = "Item".nt, TokenType.SKIP.t, TokenType.SEMICOLON.t)
        rule(lhs = "Item".nt, TokenType.STOP.t, TokenType.SEMICOLON.t)

        rule(lhs = "LocalVarDecl".nt, "Type".nt, "DeclaratorList".nt)

        // While/Do/For are statement-forms that can appear as items within blocks
        rule(lhs = "Item".nt, "WhileStmt".nt)
        rule(lhs = "Item".nt, "DoWhileStmt".nt)
        rule(lhs = "Item".nt, "ForStmt".nt)

        // Block expression now yields a value: ( items )
        rule(lhs = "BlockExpr".nt, TokenType.LPAREN.t, "ItemListOpt".nt, TokenType.RPAREN.t)

        // if-expression returns value of the chosen block
        rule(
            lhs = "IfExpr".nt,
            TokenType.IF.t,
            TokenType.LBRACE.t,
            "Expr".nt,
            TokenType.RBRACE.t,
            "BlockExpr".nt,
            TokenType.ELSE.t,
            "BlockExpr".nt
        )

        // Loops (as items)
        rule(lhs = "WhileStmt".nt, TokenType.WHILE.t, TokenType.LBRACE.t, "Expr".nt, TokenType.RBRACE.t, "BlockExpr".nt)
        rule(
            lhs = "DoWhileStmt".nt,
            TokenType.DO.t,
            "BlockExpr".nt,
            TokenType.WHILE.t,
            TokenType.LBRACE.t,
            "Expr".nt,
            TokenType.RBRACE.t,
            TokenType.SEMICOLON.t
        )
        rule(
            lhs = "ForStmt".nt,
            TokenType.FOR.t, TokenType.LBRACE.t,
            "ForInitOpt".nt, TokenType.SEMICOLON.t,
            "ForCondOpt".nt, TokenType.SEMICOLON.t,
            "ForIncrOpt".nt, TokenType.RBRACE.t,
            "BlockExpr".nt, TokenType.SEMICOLON.t
        )
        rule(lhs = "ForInitOpt".nt, "LocalVarDecl".nt)
        rule(lhs = "ForInitOpt".nt, "Expr".nt)
        rule(lhs = "ForInitOpt".nt)
        rule(lhs = "ForCondOpt".nt, "Expr".nt)
        rule(lhs = "ForCondOpt".nt)
        rule(lhs = "ForIncrOpt".nt, "Expr".nt)
        rule(lhs = "ForIncrOpt".nt)

        // Expressions (precedence climbing via grammar levels)
        rule(lhs = "Expr".nt, "Assign".nt)
        rule(lhs = "Assign".nt, "LValue".nt, TokenType.ASSIGN.t, "Assign".nt)
        rule(lhs = "Assign".nt, "OrExpr".nt)
        rule(lhs = "LValue".nt, "Postfix".nt)

        rule(lhs = "OrExpr".nt, "OrExpr".nt, TokenType.OROR.t, "AndExpr".nt)
        rule(lhs = "OrExpr".nt, "AndExpr".nt)
        rule(lhs = "AndExpr".nt, "AndExpr".nt, TokenType.ANDAND.t, "BitOrExpr".nt)
        rule(lhs = "AndExpr".nt, "BitOrExpr".nt)
        rule(lhs = "BitOrExpr".nt, "BitOrExpr".nt, TokenType.PIPE.t, "BitXorExpr".nt)
        rule(lhs = "BitOrExpr".nt, "BitXorExpr".nt)
        rule(lhs = "BitXorExpr".nt, "BitXorExpr".nt, TokenType.CARET.t, "BitAndExpr".nt)
        rule(lhs = "BitXorExpr".nt, "BitAndExpr".nt)
        rule(lhs = "BitAndExpr".nt, "BitAndExpr".nt, TokenType.AMP.t, "Equality".nt)
        rule(lhs = "BitAndExpr".nt, "Equality".nt)
        rule(lhs = "Equality".nt, "Equality".nt, TokenType.EQEQ.t, "Rel".nt)
        rule(lhs = "Equality".nt, "Equality".nt, TokenType.BANGEQ.t, "Rel".nt)
        rule(lhs = "Equality".nt, "Rel".nt)
        rule(lhs = "Rel".nt, "Rel".nt, TokenType.LT.t, "Shift".nt)
        rule(lhs = "Rel".nt, "Rel".nt, TokenType.GT.t, "Shift".nt)
        rule(lhs = "Rel".nt, "Rel".nt, TokenType.LTE.t, "Shift".nt)
        rule(lhs = "Rel".nt, "Rel".nt, TokenType.GTE.t, "Shift".nt)
        rule(lhs = "Rel".nt, "Shift".nt)
        rule(lhs = "Shift".nt, "Shift".nt, TokenType.SHL.t, "Add".nt)
        rule(lhs = "Shift".nt, "Shift".nt, TokenType.SHR.t, "Add".nt)
        rule(lhs = "Shift".nt, "Add".nt)
        rule(lhs = "Add".nt, "Add".nt, TokenType.PLUS.t, "Mul".nt)
        rule(lhs = "Add".nt, "Add".nt, TokenType.MINUS.t, "Mul".nt)
        rule(lhs = "Add".nt, "Mul".nt)
        rule(lhs = "Mul".nt, "Mul".nt, TokenType.STAR.t, "Factor".nt)
        rule(lhs = "Mul".nt, "Mul".nt, TokenType.SLASH.t, "Factor".nt)
        rule(lhs = "Mul".nt, "Mul".nt, TokenType.PERCENT.t, "Factor".nt)
        rule(lhs = "Mul".nt, "Factor".nt)

        // Factor splits pre-fixed forms from postfix/base forms (no Prefix→Postfix reduce)
        rule(lhs = "Factor".nt, "Postfix".nt)
        rule(lhs = "Factor".nt, "Prefixed".nt)

        rule(lhs = "Prefixed".nt, TokenType.BANG.t, "Factor".nt)
        rule(lhs = "Prefixed".nt, TokenType.TILDE.t, "Factor".nt)
        rule(lhs = "Prefixed".nt, TokenType.PLUS.t, "Factor".nt)
        rule(lhs = "Prefixed".nt, TokenType.MINUS.t, "Factor".nt)
        rule(lhs = "Prefixed".nt, TokenType.PLUSPLUS.t, "Factor".nt)
        rule(lhs = "Prefixed".nt, TokenType.MINUSMINUS.t, "Factor".nt)
        rule(lhs = "Prefixed".nt, TokenType.STAR.t, "Factor".nt) // deref
        rule(lhs = "Prefixed".nt, TokenType.AMP.t, "Factor".nt)  // address-of

        // Left-recursive postfix without epsilon tails; includes binary cast IDENT ~> Type
        rule(lhs = "Postfix".nt, "Postfix".nt, TokenType.PLUSPLUS.t)
        rule(lhs = "Postfix".nt, "Postfix".nt, TokenType.MINUSMINUS.t)
        rule(lhs = "Postfix".nt, "Postfix".nt, TokenType.LBRACKET.t, "Expr".nt, TokenType.RBRACKET.t)
        rule(lhs = "Postfix".nt, "Postfix".nt, TokenType.DOT.t, TokenType.IDENT.t)
        rule(lhs = "Postfix".nt, "Postfix".nt, TokenType.ARROW.t, TokenType.IDENT.t)
        rule(lhs = "Postfix".nt, "Postfix".nt, TokenType.LBRACE.t, "ArgsOpt".nt, TokenType.RBRACE.t)
        rule(lhs = "Postfix".nt, "Simple".nt)

        rule(lhs = "Simple".nt, "CastExpr".nt)
        rule(lhs = "Simple".nt, TokenType.IDENT.t)
        rule(lhs = "Simple".nt, "PrimaryNoIdent".nt)

        rule(lhs = "CastExpr".nt, TokenType.IDENT.t, TokenType.TILDE_GT.t, "Type".nt) // IDENT ~> Type
        rule(lhs = "ArgsOpt".nt, "ArgList".nt)
        rule(lhs = "ArgsOpt".nt)
        rule(lhs = "ArgList".nt, "ArgList".nt, TokenType.COMMA.t, "Expr".nt)
        rule(lhs = "ArgList".nt, "Expr".nt)

        // Primaries including expression blocks and if-expression (IDENT handled by Postfix)
        rule(lhs = "Primary".nt, "BlockExpr".nt)
        rule(lhs = "Primary".nt, "IfExpr".nt)
        rule(lhs = "Primary".nt, TokenType.LPAREN.t, "Expr".nt, TokenType.RPAREN.t)
        rule(lhs = "Primary".nt, TokenType.IDENT.t)
        rule(lhs = "Primary".nt, TokenType.INT.t)
        rule(lhs = "Primary".nt, TokenType.CHAR_LIT.t)
        rule(lhs = "Primary".nt, TokenType.STRING_LIT.t)
        rule(lhs = "Primary".nt, "SwitchExpr".nt)

        // Primary without IDENT (used to separate cast forms)
        rule(lhs = "PrimaryNoIdent".nt, "BlockExpr".nt)
        rule(lhs = "PrimaryNoIdent".nt, "IfExpr".nt)
        rule(lhs = "PrimaryNoIdent".nt, TokenType.LPAREN.t, "Expr".nt, TokenType.RPAREN.t)
        rule(lhs = "PrimaryNoIdent".nt, TokenType.INT.t)
        rule(lhs = "PrimaryNoIdent".nt, TokenType.CHAR_LIT.t)
        rule(lhs = "PrimaryNoIdent".nt, TokenType.STRING_LIT.t)
        rule(lhs = "PrimaryNoIdent".nt, "SwitchExpr".nt)

        // switch scrutinee is a general expression, cases are integer literals (docs-aligned)
        rule(
            lhs = "SwitchExpr".nt,
            TokenType.SWITCH.t, TokenType.LPAREN.t, "Expr".nt, TokenType.RPAREN.t,
            TokenType.LBRACE.t, "CaseList".nt, "DefaultCase".nt, TokenType.RBRACE.t
        )
        rule(lhs = "CaseList".nt, "CaseList".nt, "Case".nt)
        rule(lhs = "CaseList".nt)
        rule(lhs = "Case".nt, TokenType.INT.t, TokenType.ARROW.t, "Expr".nt, TokenType.SEMICOLON.t)
        rule(lhs = "DefaultCase".nt, TokenType.ELSE.t, TokenType.ARROW.t, "Expr".nt, TokenType.SEMICOLON.t)
    }
}
