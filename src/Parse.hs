{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE Rank2Types        #-}
module Parse where
import           Builtin
import           Control.Applicative
import           Data.Attoparsec.Char8
import           Expr

--
expr :: (forall a. Expr a -> b) -> Parser a
expr = undefined




-- expr = bool
-- lit :: (forall a. Expr a -> b) -> Parser  b
-- lit = num <|> bool
--         where
--             num = do
--                 y <- string ""
--                 return (Lit (2 :: Int))
--             bool = (string "👆" >> return (Lit True)) <|> (string "👇" >> return (Lit False))


            -- list = Lit [] <$> (string "[" *> many lit <* string "]")


-- bool :: Parser (Expr Bool)
-- bool = (string "👆" >> return (Lit True)) <|> (string "👇" >> return (Lit False))

-- list = Lit [] <$> (string "[" *> many lit <* string "]")

-- bool = (string "👆" >> return (Lit True) ) <|> ( string "👇" >> return (Lit False) )
--
-- list :: TypeOf t =>  Parser (Expr [t])
-- list = Lit ([] $ (string "🌔" *> many lit <* string "🌖"))
-- --
-- lam :: TypeOf t => Parser (Expr (String -> t -> Expr (t -> t)))
-- lam = undefined

-- var :: Parser Expr
--
-- var :: Parser Expr
--
-- lam :: Parser Expr
--
-- app :: Parser Expr
--
-- builtin :: Parser Expr
--
-- ifthenelse :: Parser Expr
--
-- expr :: Parser Expr
