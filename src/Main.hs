module Main where
import           Builtin
import           Eval
import           Expr
import           Parse
main :: IO ()
main = putStrLn "Hello World"

succ :: Expr (Int -> Int)
succ = Lambda "x" TInt $ Builtin Add (Var "x" TInt) (Lit 1)

catify :: Expr (String -> String)
catify = Lambda "x" TString $ Builtin Concat (Var "x" TString) (Lit " meow")

plus :: Expr (Int -> Int -> Int)
plus = Lambda "x" TInt $ Lambda "y" TInt $ Builtin Add (Var "x" TInt) (Var "y" TInt)

cons :: Expr ([Int] -> [Int] -> [Int])
cons = Lambda "x" TList $ Lambda "y" TList $ Builtin Cons (Var "x" TList) (Var "y" TList)
