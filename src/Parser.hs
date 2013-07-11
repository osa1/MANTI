{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parser
  ( term
  , query
  , rule
  , toplevel
  , queryOrRule
  , Toplevel(..)
  , Cmd(..)
  ) where

import           Text.Parsec
import           Text.Parsec.Language (haskell)
import           Text.Parsec.String   (Parser)
import           Text.Parsec.Token    (integer)

import           Control.Applicative  ((<$>), (<*), (<*>))
import           Control.Monad        (liftM)

import           Types

-- Helpers
-- -------------------------------------------------------------------

spStr :: String -> Parser String
spStr s = string s <* spaces

spChar :: Char -> Parser Char
spChar c = char c <* spaces

atomRest :: Parser String
atomRest = many $ oneOf $ concat [ ['a'..'z'], ['0'..'9'], "-_'" ]

parens :: Parser a -> Parser a
parens p = do
    spChar '('
    ret <- p
    spChar ')'
    return ret

listOf :: Parser a -> Parser [a]
listOf p = sepBy (p <* spaces) (spChar ',')

-- -------------------------------------------------------------------

atom :: Parser Atom
atom = do
    f <- oneOf ['a'..'z']
    r <- atomRest
    spaces
    return $ Atom (f:r)

var :: Parser Var
var = do
    f <- oneOf ['A'..'Z']
    r <- atomRest
    spaces
    return $ Var (f:r)

compound :: Parser Compound
compound = do
    functor <- atom
    spaces
    terms <- parens $ listOf term
    return $ Compound functor terms

int :: Parser Integer
int = integer haskell

query :: Parser Query
query = Query <$> (compound <* spChar '?')

rule :: Parser Rule
rule = Rule <$> rhead <*> rbody
  where
    rhead :: Parser RHead
    rhead = do
        name  <- atom
        terms <- parens $ listOf term
        return $ RHead name terms

    rbody :: Parser RBody
    rbody = do
        isFact  <- optionMaybe (spChar '.')
        clauses <- case isFact of
                     Just _  -> return [Compound (Atom "true") []]
                     Nothing -> spStr ":-" >> (listOf compound <* spChar '.')
        return (RBody clauses)

term :: Parser Term
term = choice
    [ TComp <$> try compound
    , TAtom <$> try atom
    , TVar <$> try var
    , TInt <$> int
    ]

data Toplevel
    = TRule Rule
    | TQuery Query
    | TCmd Cmd
    deriving Show

data Cmd
    = Edit
    deriving Show

cmd :: Parser Cmd
cmd = spStr "edit" >> return Edit

toplevel :: Parser Toplevel
toplevel = spaces >> choice
  [ TCmd <$> (spChar ':' >> cmd)
  , TQuery <$> try query
  , TRule <$> rule
  ]

queryOrRule :: Parser (Either Query Rule)
queryOrRule = liftM Left (try query) <|> liftM Right rule
