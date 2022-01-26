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
type C = Code                    -- Class
type M = Code                    -- Member
type S = Env -> (Code, Env)                     -- Statement
type E = ValueOrAddress -> Env -> Code -- Expression

type Env = M.Map String Int

codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra =
    ( codeClass
    , codeMember
    , codeStatement
    , codeExpr
    )

codeClass :: String -> [M] -> C
codeClass c ms = [ Bsr "main" , HALT] ++ concat ms

codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl d = []

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    fMembMeth t x ps s = ([ LABEL x ,LDR MP, LDRR MP SP]  ++ map loadPar ps ++ (fst $ s  env )++ [LDRR SP MP,STR MP, RET])
      where 
        env = getPars ps 1
        getPars :: [Decl] -> Int-> Env
        getPars [] _ = M.empty 
        getPars (Decl _ p: ps) i  =  M.insert p i (getPars ps (i + 1))
        loadPar _ = LDS (-(1 + M.size env))
        

codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatMeth, fStatBlock)
  where       
    fStatDecl :: Decl -> S
    fStatDecl (Decl _ d) env = ([AJS 1],M.insert d (M.size env + 1) env) -- add to env

    fStatExpr :: E -> S
    fStatExpr e env = (e Value env ++ [pop],env)

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ (fst $ s1 env) ++ [BRA n2] ++ (fst $ s2 env), env)
        where
            c        = e Value env
            (n1, n2) = (codeSize $ fst $ s1 env, codeSize $ fst $ s2 env)

    fStatWhile :: E -> S -> S
    fStatWhile e s1 env =( [BRA n] ++ (fst $ s1 env) ++ c ++ [BRT (-(n + k + 2))], env)
        where
            c =  e Value env
            (n, k) = (codeSize $ fst $ s1 env, codeSize c)

    fStatReturn :: E -> S
    fStatReturn e env = (e Value env ++ [pop] ++ [RET],env)

    fStatBlock :: [S] -> S
    fStatBlock ss env = (handleBlock ss env, env) -- when leaving a block the envirment gets reverted to the state it was in when it entered the block 
      where handleBlock :: [S] -> Env -> Code
            handleBlock [] env' = [STS (- n) ,AJS (- (n- 1) )] -- all vars that have been declared withing a block fall out of bounds when leaving said block
                where n = (M.size env' - M.size env)
            handleBlock (s:ss') env' =  (fst $ s env' ) ++ handleBlock ss' (snd $ s env') -- within the block envirment keeps being passed on and (potentially) added on

    fStatMeth :: String -> [E] -> S
    fStatMeth "print" es env = (concatMap (\x -> x Value env) es ++ [TRAP 0] ,env)
    fStatMeth x       es env = (concatMap (\x -> x Value env) es ++ [Bsr x] ++ [LDR R3] ,env)

codeExpr = (fExprInt, fExprBool, fExprChar, fExprVar, fExprOp)
  where

    fExprInt :: Int -> E
    fExprInt n va env = [LDC n] 

    fExprBool True va env = [LDC 1]
    fExprBool _    va env= [LDC 0]

    fExprChar c va env = [LDC (ord c)]

    fExprVar :: String -> E
    fExprVar x va env = case va of
                           Value    ->  [LDL ( env M.! x)]
                           Address  ->  [LDLA ( env M.! x)]

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
    -- ex 7
    -- The first expresion is put twice on the stack for eval. and the result. (for && and || )
    fExprOp "&&" e1 e2 va env = e1' ++ e1' ++ [BRF (codeSize e2'+1)] ++ e2' ++ [AND]
      where
        e1', e2' :: Code
        e1' = e1 Value env
        e2' = e2 Value env
    fExprOp "||" e1 e2 va env = e1' ++ e1' ++ [BRT (codeSize e2'+1)] ++ e2' ++ [OR]
      where
        e1', e2' :: Code
        e1' = e1 Value env
        e2' = e2 Value env
    fExprOp op e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes M.! op]
      where
        opCodes :: M.Map String Instr
        opCodes = M.fromList [ ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD)
                             , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ)
                             , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                             ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
