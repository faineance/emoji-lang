{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
module Eval where
import           Data.Function
import           Data.Type.Equality
import           Syntax


eq :: Type u -> Type v -> Maybe (u :~: v)
eq TInt TInt = Just Refl
eq TBool TBool = Just Refl
eq (TArrow u u') (TArrow v v') = do
  Refl <- eq u  v
  Refl <- eq u' v'
  return Refl
eq _ _ = Nothing

subs :: Sym -> u -> Type u -> Expr t -> Expr t
subs _ _ _ (Lit b) = Lit b
subs x v u (Var y t)
  | x == y =
      case eq u t of
        Just Refl -> Lift v u
        Nothing   -> error "ill-typed substitution"
  | otherwise = Var y t

subs x v u (Op b e e') = Op b (subs x v u e) (subs x v u e')
subs x v u (If e e' e'') = If (subs x v u e) (subs x v u e') (subs x v u e'')
subs x v u (Lambda y t e)
  | x == y = Lambda y t e
  | otherwise = Lambda y t (subs x v u e)
subs x v u (App e e') = App (subs x v u e) (subs x v u e')
subs _ _ _ (Lift x t) = Lift x t


eval :: Expr t -> t
eval (Lit v) = v
eval (Var v _) = error ("Free variable " ++ v ++ " has no value")
eval (Lambda x t e) = \v -> eval $ subs x v t e
eval (App e e') = eval e (eval e')
eval (Op b e e') = (evalOp b `on` eval) e e'
eval (If b e e')
  | eval b = eval e
  | otherwise = eval e'
eval (Lift x _) = x



evalOp :: Op a b -> a -> a -> b
evalOp Add = (+)
evalOp Sub = (-)
evalOp Eq = (==)
evalOp Lt = (<)
evalOp Gt = (>)
evalOp And = (&&)
evalOp Or = (||)
