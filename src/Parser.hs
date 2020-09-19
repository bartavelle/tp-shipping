{-# LANGUAGE InstanceSigs #-}
module Parser where

import Control.Applicative

-- un parser consomme une chaine de caractères, et retourne potentiellement
-- un résultat, et le reste de la chaîne qui n'a pas été parsée.
newtype Parser a = Parser (String -> Maybe (a, String))

-- classe de ce qui se "mappe"
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = undefined

-- classe de ce qui peut "s'enchainer"
instance Applicative Parser where
  -- inject une valeur dans un parser, *sans modifier la chaîne de
  -- caractères*
  pure :: a -> Parser a
  pure = undefined

  -- permet de chainer des parsers! Difficile à expliquer, suivre les
  -- types
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser a = Parser (undefined f a)

-- classe de ce qui peut "s'enchainer en fonction des précédents résultats"
instance Monad Parser where
  -- permet de chainer des parsers en fonction de leurs résultats.
  -- encore une fois, suivre les types
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser (undefined p f)

-- classe de ce qui peut échouer
instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    Parser p1 <|> Parser p2 = Parser (\i -> p1 i <|> p2 i)

-- bloc de base
satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

-- une fois défini, on a facilement de nombreux combinateurs

int :: Parser Int
int = undefined

sepBy :: Parser sep -> Parser a -> Parser [a]
sepBy = undefined

string :: String -> Parser String
string = undefined

char :: Char -> Parser Char
char = undefined

-- | 

parseList :: Parser a -> Parser [a]
parseList = undefined

