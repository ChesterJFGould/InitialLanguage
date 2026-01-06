module TC where

import Data.Map (Map)
import Syntax

-- data Ctx
--   = CtxKind String Kind
--   | CtxType String Type
--   | CtxUnif String Kind

data TypeV
  = VVar String Int
  | VUnif Int
  | VFun TypeV TypeV
  | VAll String Kind Sub Type
  | VApp TypeV TypeV

data Ctx =
  Ctx
    { typeVars :: Map String Kind
    , vars :: Map String TypeV
    , level :: Int
    }

data Sub = 
  Sub
    { unifVals :: Map Int TypeV
    , typeVals :: Map String TypeV
    }

data TypeEnv = TypeEnv (Map String TypeV)

-- A lookup here needs to find the fixpoint, i.e. lookup l unifEnv = VUnif l
-- Not quite, consider the following situation:
-- u1, u2, u3, u4, u5
-- u5 |-> (u3 -> u4)
-- u3 |-> u2
-- u2 |-> u1
-- Looking up u5 here should return (u1 -> u4)
-- Sort of feels like an e-graph?
-- Don't really have equations between function applications though, just mapping variables to values
-- But basically results in these sorts of equations, consider if we added u6 |-> (u2 -> u4)
data UnifEnv = UnifEnv (Map Int TypeV)

checkLte :: Ctx -> TypeEnv -> TypeEnv -> UnifEnv -> TypeV -> TypeV -> Maybe UnifEnv
checkLte ctx sub lenv renv uenv (VVar _ l) (VVar l') | l == l' = Just sub
checkLte ctx sub lenv renv uenv (VUnif l) (VUnif l') = _
checkLte ctx sub lenv renv uenv (VFun d c) (VFun d' c') =
  do
  sub' <- checkLte ctx sub d' d
  checkLte ctx sub' c c'
checkLte ctx sub t (VAll x k bodySub body) =
  _
checkLte ctx sub (VAll x k bodySub body) t = _

{-
data Judg
  = JLte Type Type
  | JChk Expr Type
  | JSyn Expr (Type -> Judg)
  | JApp Type Expr (Type -> Judg)

data Wl
  = WlCtx Wl Ctx
  | WlJudg Wl Judg
  | WlMt

tcProgram :: Program -> Bool
tcProgram = _

tcDef :: Ctx -> Def -> Bool
tcDef = _

tcType :: Ctx -> Type -> Kind -> Bool
tcType = _

tcExpr :: Ctx -> Expr -> Type -> Def
tcExpr = _

tcWl :: Wl -> Bool
tcWl (WlMt) = True
tcWl wl
  | Just wl' <- step wl = tcWl wl'
  | otherwise = False

step :: Wl -> Maybe Wl
step (WlCtx wl _) = Just wl
step (WlJudg wl (JLte a b)) = stepLte wl a b
step (WlMt) = Nothing

stepLte :: Wl -> Type -> Type -> Maybe Wl
stepLte wl (TVar a) (TVar a') | a == a' = Just wl
stepLte wl (TUnif a) (TUnif a'))) | a == a' = Just wl
stepLte wl (TFun d c) (TFun d' c') = Just (WlJudg (WlJudg wl (JLte c c') (JLte d' d)))
stepLte wl a (TAll x k t) = Just _
-}
