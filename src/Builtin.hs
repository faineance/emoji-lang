{-# LANGUAGE GADTs          #-}
module Builtin where

data Type a where
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

data UBuiltin a where
        Succ :: UBuiltin Int
        Pred :: UBuiltin Int
        Not :: UBuiltin Bool
        Neg :: UBuiltin Int

data BBuiltin a b where
        Add :: BBuiltin Int Int
        Sub :: BBuiltin Int Int
        Eq :: BBuiltin Int Bool
        Lt :: BBuiltin Int Bool
        Gt :: BBuiltin Int Bool
        And :: BBuiltin Bool Bool
        Or :: BBuiltin Bool Bool
        Concat :: BBuiltin String String
        -- Cons :: TypeOf t => BBuiltin [t] [t]
