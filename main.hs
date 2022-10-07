import Turing.Parser
import Turing.Interpreter
import Turing.Preprocessor
import Common.Types 
import Common.Parser (parse)
import System.Environment (getArgs)

showUsage :: IO ()
showUsage = putStrLn $ unlines [
        "Usage: turing file"
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename:[]) -> do
            code <- readFile filename
            let p = parse programp $ preprocess code
            let state = State (Tape [] '#' []) "start"

            putStrLn $ show p
            case p of
              Just (p, _) -> putStrLn $ show $ run_program state p
              Nothing -> putStrLn "Syntax error"

        _ -> showUsage
