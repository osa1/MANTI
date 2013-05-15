{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parser where

import Text.Parsec
import Text.Parsec.String

import Control.Applicative ((<*), (<$>), (<*>))

import Types

spStr :: String -> Parser String
spStr s = string s <* spaces

spChar :: Char -> Parser Char
spChar c = char c <* spaces

atomRest :: Parser String
atomRest = many $ oneOf $ concat [ ['a'..'z'], ['0'..'9'], "-_'" ]

termList :: Parser [Term]
termList = sepBy1 (term <* spaces) (spChar ',')

parens :: Parser a -> Parser a
parens p = do
    spChar '('
    ret <- p
    spChar ')'
    return ret

atom :: Parser String
atom = do
    f <- oneOf ['a'..'z']
    r <- atomRest
    spaces
    return $ f : r

var :: Parser String
var = do
    f <- oneOf ['A'..'Z']
    r <- atomRest
    spaces
    return $ f : r

compound :: Parser (String, [Term])
compound = do
    functor <- atom
    spaces
    terms <- parens termList
    return (functor, terms)

stat :: Parser Stat
stat = choice
    [ Query <$> try query
    , Rule <$> rhead <*> rbody
    ]

rhead :: Parser RHead
rhead = do
    name <- atom
    terms <- parens termList
    return $ RHead name terms

rbody :: Parser RBody
rbody = do
    isFact <- optionMaybe (spChar '.')
    case isFact of
      Just _ -> return $ RBody [Atom "true"]
      Nothing -> do
        spStr ":-"
        clauses <- termList
        return $ RBody clauses

query :: Parser Term
query = do
    name <- atom
    terms <- parens termList
    spChar '?'
    return $ Compound name terms

term :: Parser Term
term = choice
    [ uncurry Compound <$> try compound
    , Atom <$> try atom
    , Var <$> var
    ]
