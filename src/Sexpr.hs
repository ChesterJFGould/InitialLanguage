{-# LANGUAGE LambdaCase #-}
module Sexpr where

import Control.Category
import qualified Data.Char as Char
import SyntaxDesc
import Prelude hiding (id, (.))

data Sexpr
  = SList [Sexpr]
  | SSymbol String
  deriving Show

instance Show Sexpr where
  -- sexpr should be total
  show x
    | Just s <- pretty sexpr x = s
    | otherwise = undefined

sListL :: UnL Sexpr [Sexpr]
sListL =
  unInj
    SList
    (\case
      (SList l) -> Just l
      _ -> Nothing
    )

sSymbolL :: UnL Sexpr String
sSymbolL =
  unInj
    SSymbol
    (\case
      (SSymbol s) -> Just s
      _ -> Nothing
    )

sexpr :: PP Sexpr
sexpr = list <> symbol

list :: PP Sexpr
list = sListL . lit "(" . fixed "" whitespace . (nonEmptySexprs <> nil) . lit ")"
  where
    nonEmptySexprs :: PP [Sexpr]
    nonEmptySexprs = consL . sexpr . many (fixed " " whitespace . sexpr) . fixed ""  whitespace

isSymbolChar :: Char -> Bool
isSymbolChar c = c /= '(' && c /= ')' && not (Char.isSpace c)

symbol :: PP Sexpr
symbol = sSymbolL . some (sat isSymbolChar)
