{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import Data.Char (ord)

{-
  This file contains a starting point for the code generation which should handle very simple programs.
-}

-- The types that we generate for each datatype: Our type variables for the algebra
type C = Code                   -- Class
type M = Code                   -- Member
type S = Code                   -- Statement
type E = ValueOrAddress -> Code -- Expression


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra =
    ( codeClass
    , codeMember
    , codeStatement
    , codeExpr
    )

codeClass :: String -> [M] -> C
codeClass c ms = [Bsr "main", HALT] ++ concat ms

codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl d = []

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    fMembMeth t x ps s = [LABEL x] ++ s ++ [RET]

codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatMeth, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl d = []

    fStatExpr :: E -> S
    fStatExpr e = e Value ++ [pop]

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
        where
            c        = e Value
            (n1, n2) = (codeSize s1, codeSize s2)

    fStatWhile :: E -> S -> S
    fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
        where
            c = e Value
            (n, k) = (codeSize s1, codeSize c)

    fStatReturn :: E -> S
    fStatReturn e = e Value ++ [pop] ++ [RET]

    fStatBlock :: [S] -> S
    fStatBlock = concat

    fStatMeth :: String -> [E] -> Code
    fStatMeth "print" es = concatMap (\x -> x Value) es ++ [TRAP 0] 
    fStatMeth x       es = concatMap (\x -> x Value) es ++ [Bsr x] ++ [LDR R3] 

codeExpr = (fExprInt, fExprBool, fExprChar, fExprVar, fExprOp)
  where

    fExprInt :: Int -> E
    fExprInt n va = [LDC n]

    fExprBool True va = [LDC 1]
    fExprBool _    va = [LDC 0]

    fExprChar c va = [LDC (ord c)]

    fExprVar :: String -> E
    fExprVar x va = let loc = 42 in case va of
                                  Value    ->  [LDL  loc]
                                  Address  ->  [LDLA loc]

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
    -- ex 7
    -- The first expresion is put twice on the stack for eval. and the result. (for && and || )
    fExprOp "&&" e1 e2 va = e1' ++ e1' ++ [BRF (codeSize e2'+1)] ++ e2' ++ [AND]
      where
        e1', e2' :: Code
        e1' = e1 Value
        e2' = e2 Value
    fExprOp "||" e1 e2 va = e1' ++ e1' ++ [BRT (codeSize e2'+1)] ++ e2' ++ [OR]
      where
        e1', e2' :: Code
        e1' = e1 Value
        e2' = e2 Value
    fExprOp op e1 e2 va = e1 Value ++ e2 Value ++ [opCodes M.! op]
      where
        opCodes :: M.Map String Instr
        opCodes = M.fromList [ ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD)
                             , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ)
                             , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                             ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
