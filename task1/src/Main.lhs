\begin{code}

import Data.Word (Word8)
import qualified CakeDisplay as Di
import qualified CakeParser as Pa
import qualified CakeStack as Sk
import qualified CakeState as Se
import qualified CakeValue as V
import qualified CakeEvaluator as Ev

outputState :: Se.State -> IO ()
outputState st8 = do
  putStrLn $ Di.prettyFormatBox $ Se.display st8
  putStrLn $ show $ Se.stack st8

evalStk :: Sk.Stack V.Value -> IO ()
evalStk stk = do
  let disp = Di.emptyDisplay
  --st8  <- Ev.evaluateIO 0 $ Se.State disp stk
  st8  <- Ev.evaluateStepping $ Se.State disp stk
  --let st8 = Ev.evaluate $ Se.State disp stk
  either youFail outputState st8

outputStk :: Sk.Stack V.Value -> IO ()
outputStk stk = do
  putStrLn $ show stk

youFail :: String -> IO ()
youFail str = do
  putStrLn str

youWin :: (Show a) => a -> IO ()
youWin x = do
  putStrLn $ show x

main :: IO ()
main = do
  let disp  = Di.emptyDisplay
  --let ops   = "(4 5 6 + +)@"
  --let ops   = "0(9)(9~)(4!5#2+#@)@"
  let c     = "(4!5#2+#@)"
  let a     = "(3!3!1-2!1=()5!" ++ c ++ "@3#*)"
  let ops   = "3" ++ a ++ "3!4#3!@3#"
  --let ops   = "4 2 -"
  --let ops = "36 4 $"
  let mbstk = Pa.parse ops
  --let stk  = Sk.Stack [V.CAdd, V.CAdd, V.CInteger 10, V.CInteger 5, V.CInteger 0]
  --let stk  = Sk.Stack [V.CAnd, V.CAnd, V.CInteger 2, V.CInteger 1, V.CInteger 1]
  either youFail evalStk mbstk
  --either youFail outputStk mbstk

{-
main :: IO ()
main = do
  let disp = Di.emptyDisplay
  let expr = "4 5 6 + +"
  let e1 = Ev.evaluateOne (V.CEval) disp (Sk.Stack [V.CParen [toEnum $ fromEnum x | x <- expr]])
  either youFail youWin e1
-}

\end{code}
