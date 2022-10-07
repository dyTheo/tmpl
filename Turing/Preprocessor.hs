module Turing.Preprocessor (preprocess) where

convert_characters :: String -> String
convert_characters [] = []
convert_characters (x:xs) = case x of
                             '\n' -> ';':(convert_characters xs)
                             '\t' -> ' ':(convert_characters xs)
                             _ -> x:(convert_characters xs)

remove_comments :: String -> String
remove_comments [] = []
remove_comments ('/':'/':xs) = remove_comments (dropWhile (/= ';') xs)
remove_comments (x:xs) = x:(remove_comments xs)

preprocess :: String -> String
preprocess = remove_comments . convert_characters
