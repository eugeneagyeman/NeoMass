import Grammar
import Tokens
import Eval
import System.Environment
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' = do (fileName: _) <- getArgs
           sourceText <- readFile (fileName)
           input <- getContents
           let tokens = alexScanTokens sourceText
           let parsedCalc = parseProg tokens
           let lines' = lines input
           --DEBUGGING
           --putStrLn input
           --putStrLn (show (lines input))
           --putStrLn ( show (readStreams lines') )
           --DEBUGGING
           compiledProg <- evaluateProgram (parsedCalc) (readStreams lines')
           return ()

readStreams :: [String] -> [[Int]]
readStreams input = multiZipL $ map (\x -> convertIntoInt x) input
        
convertIntoInt :: String -> [Int]
convertIntoInt line = map (\x -> read x :: Int) $ words line

multiZipL :: [[a]] -> [[a]]
multiZipL [] = []
multiZipL ([]   : xss) = multiZipL xss
multiZipL ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : multiZipL (xs : [ t | (_:t) <- xss])

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               putStrLn("----------------")
               hPutStrLn stderr err
               putStrLn("----------------")
               main