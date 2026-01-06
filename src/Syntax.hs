module Syntax where

import Sexpr (Sexpr)
import qualified Sexpr as S

data Kind
  = Type
  | KFun Kind Kind
  deriving Show

data Type
  = TVar String
  | TFun Type Type
  | TAll String Kind Type
  | TApp Type Type
  deriving Show

data TypeV
  = TVar String Int
  | TUnif Int
  | TFun TypeV TypeV
  | TAll Kind (TypeV -> TypeV)

-- Represents the syntax for an expression in the inital language
data Expr
  = Var String
  | Lambda String Expr
  | App Expr Expr
  deriving Show

data Def
  = DefExpr String Type Expr
  deriving Show

-- A program in the initial language is a list of definitions
data Program = Program [Def]
  deriving Show

sexprs2Program :: [Sexpr] -> Maybe Program
sexprs2Program l = Program <$> sequenceA (map sexpr2Def l)

sexpr2Def :: Sexpr -> Maybe Def
sexpr2Def (S.SList [S.SSymbol "def", S.SSymbol x, S.SSymbol ":", t, e]) =
  DefExpr x <$> sexpr2Type t <*> sexpr2Expr e
sexpr2Def _ = Nothing

sexpr2Type :: Sexpr -> Maybe Type
sexpr2Type (S.SSymbol x) = Just (TVar x)
sexpr2Type (S.SList [S.SSymbol "->", d, c]) =
  TFun <$> sexpr2Type d <*> sexpr2Type c
sexpr2Type (S.SList [S.SSymbol "forall", S.SList [S.SSymbol x, k], t]) =
  TAll x <$> sexpr2Kind k <*> sexpr2Type t
sexpr2Type (S.SList [f, a]) =
  TApp <$> sexpr2Type f <*> sexpr2Type a
sexpr2Type _ = Nothing

sexpr2Kind :: Sexpr -> Maybe Kind
sexpr2Kind (S.SSymbol "*") = Just Type
sexpr2Kind (S.SList [S.SSymbol "->", d, c]) =
  KFun <$> sexpr2Kind d <*> sexpr2Kind c
sexpr2Kind _ = Nothing

sexpr2Expr :: Sexpr -> Maybe Expr
sexpr2Expr (S.SSymbol x) = Just (Var x)
sexpr2Expr (S.SList [S.SSymbol "fun", S.SSymbol x, e]) =
  Lambda x <$> sexpr2Expr e
sexpr2Expr (S.SList [f, a]) =
  App <$> sexpr2Expr f <*> sexpr2Expr a
sexpr2Expr _ = Nothing
