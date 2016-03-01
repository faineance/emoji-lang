
module Parse where
import           Builtin
import           Expr
import           Text.ParserCombinators.Parsec

-- lit :: Parser (Expr a)
-- lit = bool <|> list
--         where
--             bool = (string "👆" >> return Lit True) <|> (string "👇" >> return Lit False)
--             list = Lit [] <$> (string "[" *> many lit <* string "]")

-- lit :: TypeOf t => Parser (Expr t)
-- lit = bool

-- bool ::  Parser (Expr TBool)
-- bool = (char '👆' >> return (Lit True) ) <|> ( char '👇' >> return (Lit False) )

-- list :: TypeOf t =>  Parser (Expr [t])
-- list = Lit ([] $ (string "🌔" *> many lit <* string "🌖"))
--
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
