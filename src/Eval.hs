{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
module Eval where
import           Builtin
import           Data.Function

import           Data.Type.Equality
import           Expr



-- eq :: Type u -> Type v -> Maybe (u :~: v)
-- eq TInt TInt = Just Refl
-- eq TBool TBool = Just Refl
-- eq TChar TChar = Just Refl
-- eq TString TString = Just Refl
-- eq (TArrow u u') (TArrow v v') = do
--   Refl <- eq u  v
--   Refl <- eq u' v'
--   return Refl
-- eq _ _ = Nothing

subs :: TEq u t => Sym -> u -> Type u -> Expr t -> Expr t
subs _ _ _ (Lit b) = Lit b
subs x v u (Var y t)
  | x == y = Lift v u
    --   case eq u t of
    --     Just Refl -> Lift v u
    --     Nothing   -> error "ill-typed substitution"
  | otherwise = Var y t
subs x v u (BBuiltin b e e') = BBuiltin b (subs x v u e) (subs x v u e')
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
eval (BBuiltin b e e') = (evalBBuiltin b `on` eval) e e'
eval (If b e e')
  | eval b = eval e
  | otherwise = eval e'
eval (Lift x _) = x


evalBBuiltin :: BBuiltin a b -> a -> a -> b
evalBBuiltin Add = (+)
evalBBuiltin Sub = (-)
evalBBuiltin Eq = (==)
evalBBuiltin Lt = (<)
evalBBuiltin Gt = (>)
evalBBuiltin And = (&&)
evalBBuiltin Or = (||)
evalBBuiltin Concat = (++)
-- evalBBuiltin Cons = (:)

-- evalUBuiltin :: UBuiltin a -> a -> b
-- evalUBuiltin Succ = succ
-- evalUBuiltin Pred = pred
-- evalUBuiltin Not = not
-- evalUBuiltin Neg = negate
