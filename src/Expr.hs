
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Expr where
import           Builtin

type Sym = String

data Expr :: * -> * where
    Lit :: (TypeOf a, Show a) => a -> Expr a
    Var :: Sym -> Type a -> Expr a
    Lambda :: Sym -> Type a -> Expr b -> Expr (a -> b)
    App :: Expr (a -> b) -> Expr a -> Expr b
    Builtin :: Builtin a b -> Expr a -> Expr a -> Expr b
    If :: Expr Bool -> Expr a -> Expr a -> Expr a
    Lift :: a -> Type a -> Expr a
