{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Builtin where

data Type :: * -> * where
        TInt :: Type Int
        TBool :: Type Bool
        TChar :: Type Char
        TString :: Type String
        TList :: (TypeOf t) => Type [t]
        TArrow :: Type a -> Type b -> Type (a -> b)

class TypeOf a where
    typeOf :: a -> Type a

instance TypeOf Int where
    typeOf _ = TInt

instance TypeOf Bool where
    typeOf _ = TBool
--
instance TypeOf Char where
    typeOf _ = TChar

instance TypeOf t => TypeOf [t] where
    typeOf _ = TList

data Builtin :: * -> * -> * where
        Add :: Builtin Int Int
        Sub :: Builtin Int Int
        Eq :: Builtin Int Bool
        Lt :: Builtin Int Bool
        Gt :: Builtin Int Bool
        And :: Builtin Bool Bool
        Or :: Builtin Bool Bool
        Concat :: Builtin String String
        Cons :: TypeOf t => Builtin [t] [t]
        
