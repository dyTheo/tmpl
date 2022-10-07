module Common.Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isAlpha, isDigit, isSpace)

data Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Monad Parser where
    return a = Parser $ \s -> Just (a, s)
    p >>= f = Parser $ \xs -> case parse p xs of
                               Nothing -> Nothing
                               Just(a, ys) -> parse (f a) ys

instance Applicative Parser where
    pure = return
    fa <*> xa = do
        f <- fa
        x <- xa
        return $ f x

instance Functor Parser where
    fmap f x = x >>= return . f

instance Alternative Parser where
    empty = failp
    p <|> q = Parser $ \s -> case parse p s of
                              Nothing -> parse q s
                              Just(a, ys) -> Just(a, ys)

failp :: Parser a
failp = Parser $ \_ -> Nothing

movep :: Parser Char
movep = Parser p
    where p [] = Nothing
          p (x:xs) = Just (x, xs)

charp :: Char -> Parser Char
charp c = Parser $ \s -> case s of
                          (x:xs) | x == c -> Just (c, xs)
                          _ -> Nothing

predicatep :: (Char -> Bool) -> Parser Char
predicatep cond = Parser $ \s -> case s of 
                                  (x:xs) | cond x -> Just (x, xs)
                                  _ -> Nothing

stringp :: String -> Parser String
stringp [] = return []
stringp (x:xs) = do
        y <- charp x
        ys <- stringp xs
        return (y:ys)

starp :: Parser a -> Parser [a]
starp p = plusp p <|> return []

plusp :: Parser a -> Parser [a]
plusp p = do
    x <- p
    xs <- starp p
    return (x:xs)

tokenp :: Parser String
tokenp = plusp (predicatep (\x -> isAlpha x || isDigit x))

ignorep :: Parser a -> Int -> Parser [a]
ignorep _ 0 = return []
ignorep p n = do
            x <- p
            xs <- ignorep p (n-1)
            return (x:xs)

splitby :: Parser b -> Parser a -> Parser [a]
splitby delp tokenp = do
            x <- tokenp
            xs <- (starp (delp *> tokenp)) <|> return [] 
            return (x:xs)

spacefree :: Parser a -> Parser a
spacefree p = whitespacep *> p <* whitespacep
    where whitespacep :: Parser String
          whitespacep = starp (predicatep (\x -> isSpace x || (x == '\t')))
