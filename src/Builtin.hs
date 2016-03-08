{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Builtin where
import           Data.Function

import           Data.Type.Equality

class TEq a b where
instance TEq (Type Int) (Type Int)
instance TEq (Type Bool) (Type Bool)
instance TEq (Type Char) (Type Char)
instance TEq (Type String) (Type String)
instance TypeOf t => TEq (Type [t]) (Type [t])
instance (TEq a b, TEq c d) => TEq (Type (a -> c)) (Type (b -> d))

-- class TEq a b where
--      eq  :: a -> b -> Maybe (a :~: b)
--
-- instance TEq a b => TEq (Type a) (Type b) where
--      eq TInt TInt = Just Refl
--      eq TBool TBool = Just Refl
--      eq TChar TChar = Just Refl
--      eq TString TString = Just Refl
--      eq (TArrow u u') (TArrow v v') = do
--                Refl <- eq u  v
--                Refl <- eq u' v'
--                return Refl
--      eq _ _ = Nothing

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
