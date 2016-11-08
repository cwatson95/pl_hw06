-- Homework 6.0: Untyped lambda calculus
-- Charlie Watson, Nick Reminder
-- Prof Greenberg

{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Hw06 where

import Control.Applicative
import Data.Char

import qualified Data.Map as Map
import Data.Map (Map)

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
    fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a,s)
    f <*> a = Parser $ \s ->
        case parse f s of
             Just (g,s') -> parse (fmap g a) s'
             Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  l <|> r = Parser $ \s -> parse l s <|> parse r s

ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
    case parse parser s of
         Nothing -> Nothing
         Just (a,s') -> if p a then Just (a,s') else Nothing

lookahead :: Parser (Maybe Char)
lookahead = Parser f
    where f [] = Just (Nothing,[])
          f (c:s) = Just (Just c,c:s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where f [] = Nothing
          f (x:xs) = if p x then Just (x,xs) else Nothing

eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

ws :: Parser ()
ws = pure () <* many (satisfy isSpace)

char :: Char -> Parser Char
char c = ws *> satisfy (==c)

str :: String -> Parser String
str s = ws *> loop s
 where loop [] = pure []
       loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

type VarName = String

data Lexp =
  Var VarName
  | Lambda VarName Lexp
  | App Lexp Lexp
  deriving Show

keywords :: [String]
keywords = ["lambda","let"]

isKeyword = (`elem` keywords)

kw :: String -> Parser String
kw s = ws *> ((str s) <* (ensure isNotAlphaNumeric lookahead))
   where isNotAlphaNumeric Nothing = True
         isNotAlphaNumeric (Just (c)) = not (isAlphaNum c)

var, lam, atom, expr :: Parser Lexp
ids :: Parser String

ids = ws *> (ensure (\s -> not (isKeyword s)) ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)))
var = Var <$> ids
lam = Lambda <$> (kw "lambda" *> (ids <* str ".")) <*> expr
atom = lam <|> var <|> (parens expr)
expr = foldl1 App <$> some atom

parse :: Parser Lexp
