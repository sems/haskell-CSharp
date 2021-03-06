module CSharpLex where

import Data.Char
import Control.Monad (guard)
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>), sequence)

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyFor
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Bool
           | ConstChar Char
           | SingleComment
           deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexWhiteSpace) <* eof

lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstBool
             , lexConstChar
             , lexLowerId
             , lexUpperId
             ]


lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ ( POpen     , "("      )
      , ( PClose    , ")"      )
      , ( SOpen     , "["      )
      , ( SClose    , "]"      )
      , ( COpen     , "{"      )
      , ( CClose    , "}"      )
      , ( Comma     , ","      )
      , ( Semicolon , ";"      )
      , ( KeyIf     , "if"     )
      , ( KeyElse   , "else"   )
      , ( KeyWhile  , "while"  )
      , ( KeyReturn , "return" )
      , ( KeyTry    , "try"    )
      , ( KeyCatch  , "catch"  )
      , ( KeyClass  , "class"  )
      , ( KeyVoid   , "void"   )
      , ( KeyFor    , "for"    )
      ]


lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]
operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]


lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit)

-- ex 1
lexConstBool :: Parser Char Token
lexConstBool = ConstBool . getBool <$> (token "false" <|> token "true") -- Just like above :)
  where getBool "false" = False
        getBool "true"  = True

-- ex 1 
lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$> (symbol '\'' *> anySymbol <* symbol '\'')

-- ex 3 
lexSingleComment :: Parser Char String
lexSingleComment = "" <$ token "//" <* many (satisfy (/= '\n'))

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)


lexWhiteSpace :: Parser Char String
lexWhiteSpace = concat <$> greedy (lexSingleComment <|> (f <$> satisfy isSpace) )
  where 
    f x = [x]

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty
----- End Lexer -----


----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = (\(StdType x) -> x) <$> satisfy isStdType
  where isStdType (StdType _) = True
        isStdType _           = False

sUpperId :: Parser Token String
sUpperId = (\(UpperId x) -> x) <$> satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token String
sLowerId = (\(LowerId x) -> x) <$> satisfy isLowerId
  where isLowerId (LowerId _) = True
        isLowerId _           = False

sConstI :: Parser Token Int
sConstI  = (\(ConstInt x) -> x) <$> satisfy isConst
  where isConst (ConstInt  _) = True
        isConst _             = False

sConstB :: Parser Token Bool
sConstB  = (\(ConstBool x) ->  x) <$> satisfy isConst
  where isConst (ConstBool  _) = True
        isConst _              = False

sConstC :: Parser Token Char
sConstC  = (\(ConstChar x) ->  x) <$> satisfy isConst
  where isConst (ConstChar  _) = True
        isConst _              = False

sOperator :: Parser Token String
sOperator = (\(Operator x) -> x) <$> satisfy isOperator
  where isOperator (Operator _) = True
        isOperator _            = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

sComma :: Parser Token Token
sComma =  symbol Comma

