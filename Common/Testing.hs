module Common.Testing where

assert :: (Eq a) => a -> a -> String -> IO ()
assert expected actual msg = do
  if expected == actual
    then putStrLn $ "PASSED: " ++ msg
    else putStrLn $ "FAILED: " ++ msg 
