module Syntax where

import qualified Sexpr as S

data Kind
  = Type
  | KFun Kind Kind

data Type
  = TVar String
  | TFun Type Type
  | TAll String Kind Type
  | TApp Type Type

-- Represents the syntax for an expression in the inital language
data Expr
  = Var String
  | Lambda String Expr
  | App Expr Expr

data Def
  = DefExpr String Type Expr

-- A program in the initial language is a list of definitions
data Program = Program [Def]

sexpr2Program :: Sexpr -> Maybe Program
sexpr2Program (S.SList l) = Program <$> sequenceA (map sexpr2Def l)
sexpr2Program _ = Nothing

sexpr2Def :: Sexpr -> Maybe Def
sexpr2Def (S.SList [S.SSymbol "def", S.SSymbol x, S.SSymbol ":", t, e]) =
  DefExpr x <$> sexpr2Type t <*> sexpr2Expr e
sexpr2Def _ = Nothing
