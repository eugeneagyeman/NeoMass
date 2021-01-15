import Grammar
import Tokens
import Eval
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do putStrLn ("Parser Interactive Mode - enter an expression : ")
           sourceText <- getLine
           let parsedCalc = parseProg (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedCalc))
           result <- evaluateProgram (parsedCalc) [[1,2,3]]
           main' 

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               putStrLn("----------------")
               hPutStrLn stderr err
               putStrLn("----------------")
               return ()