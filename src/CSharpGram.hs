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

pExpr :: Parser Token Expr
pExpr = chainr pExprSimple (ExprOper <$> sOperator)

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
