{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Syntax where

import           Data.Function
import           Data.Type.Equality

type Sym = String

data Type :: * -> * where
        TInt :: Type Int
        TBool :: Type Bool
        TArrow :: Type a -> Type b -> Type (a -> b)

class TypeOf a where
  typeOf :: a -> Type a

instance TypeOf Int where
  typeOf _ = TInt

instance TypeOf Bool where
  typeOf _ = TBool

data Op :: * -> * -> * where
        Add :: Op Int Int
        Sub :: Op Int Int
        Eq :: Op Int Bool
        Lt :: Op Int Bool
        Gt :: Op Int Bool
        And :: Op Bool Bool
        Or :: Op Bool Bool

data Expr :: * -> * where
        Lit :: (TypeOf a, Show a) => a -> Expr a
        Var :: Sym -> Type a -> Expr a
        Lambda :: Sym -> Type a -> Expr b -> Expr (a -> b)
        App :: Expr (a -> b) -> Expr a -> Expr b
        Op :: Op a b -> Expr a -> Expr a -> Expr b
        If :: Expr Bool -> Expr a -> Expr a -> Expr a
        Lift :: a -> Type a -> Expr a

-- typeRep :: forall a. TypeOf a => Expr a -> Type a
-- typeRep _ = typeOf (undefined :: a)
