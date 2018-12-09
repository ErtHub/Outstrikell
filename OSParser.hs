module OSParser where

import Data.Char
import System.IO

data Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap f m = m >>= pure . f

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Monad Parser where
  return = pure
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

item  :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

expr = do words <- (many1(word))
          table <- (many1(tablerow))
          return (words, table)

word = do res <- many1(capital_letter)
          char '\n'
          return res

tablerow = do char '['
              res <- (many1(capital_letter))
              char ']'
              char '\n'
              return res

capital_letter = sat isUpper

eval :: String -> ([[Char]], [[Char]])
eval inp = case parse expr inp of
              [(n, [])] -> n
              [(_, out)] -> error ("nieskonsumowane " ++ out)
              [] -> error "bledne wejscie"
