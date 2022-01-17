{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConstInt  Int
          | ExprConstBool Bool
          | ExprConstChar Char
          | ExprVar       String
          | ExprOper      String Expr Expr
          deriving Show

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)


pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> (\w x y z -> StatBlock [w,StatWhile x (StatBlock [z,y]) ] )  <$ symbol KeyFor <* symbol POpen <*>  exprdecls <* sSemi <*> pExpr  <* sSemi <*> exprdecls <* symbol PClose <*> pStat
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])
           exprdecls = StatExpr <$> pExpr <|> StatDecl <$> pDecl 
                    <|> (\x y -> StatBlock [StatExpr x , y]) <$> pExpr <* sComma <*> exprdecls
                    <|> (\x y -> StatBlock [StatDecl x , y]) <$> pDecl <* sComma <*> exprdecls

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConstInt  <$> sConstI
           <|> ExprConstBool <$> sConstB
           <|> ExprConstChar <$> sConstC
           <|> ExprVar       <$> sLowerId
           <|> parenthesised pExpr

-- ex 2
{-
     Prio of operators
     1. =  
     2. || and ^ 
     3. &&
     4. <=, <, >=, >, ==, !=, =
     5. -, +
     6. *, /, % 
-}

-- pExpr :: Parser Token Expr
-- pExpr = chainr pExprSimple (ExprOper <$> sOperator)

pExpr :: Parser Token Expr
-- pExpr = pExpr1
pExpr =  ExprOper "="  <$> pExpr <* symbol (Operator "=")  <*> pExpr
     <|> ExprOper "||" <$> pExpr <* symbol (Operator "||") <*> pExpr
     <|> ExprOper "^"  <$> pExpr <* symbol (Operator "^")  <*> pExpr
     <|> ExprOper "&&" <$> pExpr <* symbol (Operator "&&") <*> pExpr
     <|> ExprOper "<=" <$> pExpr <* symbol (Operator "<=") <*> pExpr
     <|> ExprOper "<"  <$> pExpr <* symbol (Operator "<")  <*> pExpr
     <|> ExprOper ">=" <$> pExpr <* symbol (Operator ">=") <*> pExpr
     <|> ExprOper ">"  <$> pExpr <* symbol (Operator ">")  <*> pExpr
     <|> ExprOper "==" <$> pExpr <* symbol (Operator "==") <*> pExpr
     <|> ExprOper "!=" <$> pExpr <* symbol (Operator "!=") <*> pExpr
     <|> ExprOper "-"  <$> pExpr <* symbol (Operator "-")  <*> pExpr
     <|> ExprOper "+"  <$> pExpr <* symbol (Operator "+")  <*> pExpr
     <|> ExprOper "*"  <$> pExpr <* symbol (Operator "*")  <*> pExpr
     <|> ExprOper "/"  <$> pExpr <* symbol (Operator "/")  <*> pExpr
     <|> ExprOper "%"  <$> pExpr <* symbol (Operator "%")  <*> pExpr
     <|> pExprSimple

-- op1 :: Parser Token Expr
-- op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, op13, op14, op15 :: Parser Token (Expr->Expr->Expr)
-- op1 = ExprOper "=" <$> symbol (Operator "=") <*> empty
-- op2 = ExprOper "||" <$> symbol (Operator "||") <*> empty
-- op3 = ExprOper "^"  <$> symbol (Operator "^")  <*> empty
-- op4 = ExprOper "&&" <$> symbol (Operator "&&") <*> empty
-- op5 = ExprOper "<=" <$> symbol (Operator "<=") <*> empty
-- op6 = ExprOper "<"  <$> symbol (Operator "<")  <*> empty
-- op7 = ExprOper ">=" <$> symbol (Operator ">=") <*> empty
-- op8 = ExprOper ">"  <$> symbol (Operator ">")  <*> empty
-- op9 = ExprOper "==" <$> symbol (Operator "==") <*> empty
-- op10 = ExprOper "!=" <$> symbol (Operator "!=") <*> empty
-- op11 = ExprOper "-"  <$> symbol (Operator "-")  <*> empty
-- op12 = ExprOper "+"  <$> symbol (Operator "+")  <*> empty
-- op13 = ExprOper "*"  <$> symbol (Operator "*")  <*> empty
-- op14 = ExprOper "/"  <$> symbol (Operator "/")  <*> empty
-- op15 = ExprOper "%"  <$> symbol (Operator "%")  <*> empty
-- pExpr1, pExpr2, pExpr3, pExpr4, pExpr5, pExpr6, pExpr7, pExpr8, pExpr9, pExpr10, pExpr11, pExpr12, pExpr13, pExpr14, pExpr15 :: Parser Token Expr
-- pExpr1  = chainr pExpr2 op1
-- pExpr2 = chainl pExpr3 op2
-- pExpr3 = chainl pExpr4 op3
-- pExpr4 = chainl pExpr5 op4
-- pExpr5 = chainl pExpr6 op5
-- pExpr6 = chainl pExpr7 op6
-- pExpr7 = chainl pExpr8 op7
-- pExpr8 = chainl pExpr9 op8
-- pExpr9 = chainl pExpr10 op9
-- pExpr10 = chainl pExpr11 op10
-- pExpr11 = chainl pExpr12 op11
-- pExpr12 = chainl pExpr13 op12
-- pExpr13 = chainl pExpr14 op13
-- pExpr14 = chainl pExpr15 op14
-- pExpr15 = chainl pExprSimple op15

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType
     <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}
