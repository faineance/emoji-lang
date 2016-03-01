module Main where
import           Builtin
import           Eval
import           Expr
import           Parse
main :: IO ()
main = putStrLn "Hello World"

increment :: Expr (Int -> Int)
increment = Lambda "x" TInt $ Builtin Add (Var "x" TInt) (Lit 1)

catify :: Expr (String -> String)
catify = Lambda "x" TString $ Builtin Concat (Var "x" TString) (Lit " meow")
