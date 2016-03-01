{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Expr where
import           Builtin
import           Data.Function
import           Data.Type.Equality

type Sym = String

data Type :: * -> * where
        TInt :: Type Int
        TBool :: Type Bool
        TChar :: Type Char
        TString :: Type String
        TList :: (TypeOf t) => Type [t]
        TArrow :: Type a -> Type b -> Type (a -> b)

class TypeOf a where
    typeOf :: a -> Type a

-- instance TypeOf t => TypeOf [t] where
--     typeOf _ = TList

instance TypeOf Int where
    typeOf _ = TInt

instance TypeOf Bool where
    typeOf _ = TBool

instance TypeOf String where
    typeOf _ = TString

data Expr :: * -> * where
        Lit :: (TypeOf a, Show a) => a -> Expr a
        Var :: Sym -> Type a -> Expr a
        Lambda :: Sym -> Type a -> Expr b -> Expr (a -> b)
        App :: Expr (a -> b) -> Expr a -> Expr b
        Builtin :: Builtin a b -> Expr a -> Expr a -> Expr b
        If :: Expr Bool -> Expr a -> Expr a -> Expr a
        Lift :: a -> Type a -> Expr a
