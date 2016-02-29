module Main where
import           Eval
import           Parse
import           Syntax
main :: IO ()
main = putStrLn "Hello World"

increment :: Expr (Int -> Int)
increment = Lambda "x" TInt $ Op Add (Var "x" TInt) (Lit 1)
