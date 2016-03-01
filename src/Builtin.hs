{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Builtin where

data Builtin :: * -> * -> * where
        Add :: Builtin Int Int
        Sub :: Builtin Int Int
        Eq :: Builtin Int Bool
        Lt :: Builtin Int Bool
        Gt :: Builtin Int Bool
        And :: Builtin Bool Bool
        Or :: Builtin Bool Bool
        Concat :: Builtin String String
        Cons :: Builtin [t] [t]
