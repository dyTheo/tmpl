import Turing.Parser
import Turing.Interpreter
import Turing.Preprocessor
import Common.Types 
import Common.Parser (parse)
import System.Environment (getArgs)

showUsage :: IO ()
showUsage = putStrLn $ unlines [
        "Usage: turing [filename.tm] [tape.t]",
        "\tfilename.tm - Turing machine file",
        "\ttape.t - Tape to run the machine on"
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filename:tape_file:[]) -> do
            code <- readFile filename
            tape <- readFile tape_file

            let t = parse tapep $ preprocess tape 
            let p = parse programp $ preprocess code

            case (p, t) of
                (Just (p, _), Just (t, _)) -> do
                    let state = State t "start"
                    
                    putStrLn $ "[" ++ (show t) ++ "]"
                    putStrLn $ show (run_program state p)

                (Nothing, _) -> putStrLn "Error parsing input"
                (_, Nothing) -> putStrLn "Error parsing tape"

        _ -> showUsage
