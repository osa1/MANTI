{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parser
  ( term
  , query
  , rule
  , toplevel
  , Toplevel(..)
  , Cmd(..)
  , parse
  ) where

import           Text.Parsec          as P hiding (spaces, parse)
import           Text.Parsec.Language (haskell)
import           Text.Parsec.Token    (natural)

import           Control.Applicative  ((<$>), (<*), (<*>))
import           Control.Monad        (void)

import           Types


type Parser = Parsec String Int

-- Helpers
-- -------------------------------------------------------------------

spaces :: Parser ()
spaces = skipMany (void space <|> (char '%' >> skipMany (noneOf "\n")))

spStr :: String -> Parser String
spStr s = string s <* spaces

spChar :: Char -> Parser Char
spChar c = char c <* spaces

atomRest :: Parser String
atomRest = many $ oneOf $ concat [ ['a'..'z'], ['A'..'Z'], ['0'..'9'], "-_'" ]

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
    f <- oneOf $ '#' : ['a'..'z']
    r <- atomRest
    spaces
    return $ Atom (f:r)

var, namedVar, annonVar :: Parser Var
var = namedVar <|> annonVar

namedVar = do
    f <- oneOf $ ['A'..'Z']
    r <- atomRest
    spaces
    return $ Var (f:r)

annonVar = do
    spChar '_'
    lastIdx <- getState
    putState $ lastIdx + 1
    return (Var $ "##" ++ show lastIdx)

compound :: Parser Compound
compound = do
    functor <- atom
    spaces
    terms <- parens $ listOf term
    return $ Compound functor terms

list :: Parser Term
list = do
    spChar '['
    terms <- term `sepBy` spChar ','
    spChar ']'
    return $ foldr mkList (TAtom $ Atom "nil") terms
  where
    mkList :: Term -> Term -> Term
    mkList t1 t2 = TComp $ Compound (Atom "#cons") [t1, t2]

int :: Parser Term
int = mkPeano <$> natural haskell
  where
    mkPeano :: Integer -> Term
    mkPeano 0 = TAtom $ Atom "o"
    mkPeano n = TComp $ Compound (Atom "s") [mkPeano (n-1)]

query :: Parser Query
query = Query <$> (compound <* spChar '?')

rule :: Parser Rule
rule = Rule <$> rhead <*> rbody
  where
    rhead :: Parser RHead
    rhead = do
      name <- atom
      terms <- parens $ listOf term
      return $ RHead name terms

    rbody :: Parser RBody
    rbody = do
      isFact <- optionMaybe (spChar '.')
      clauses <- case isFact of
                   Just _  -> return [Compound (Atom "true") []]
                   Nothing -> spStr ":-" >> (listOf compound <* spChar '.')
      return (RBody clauses)

term :: Parser Term
term = choice
    [ TComp <$> try compound
    , try list
    , try int
    , TAtom <$> try atom
    , TVar  <$> try var
    ]

data Toplevel
    = TRule Rule
    | TQuery Query
    | TCmd Cmd
    deriving Show

data Cmd
    = Edit
    | Load [FilePath]
    deriving Show

cmd :: Parser Cmd
cmd = choice [ editCmd, loadCmd ]

editCmd :: Parser Cmd
editCmd = spStr "edit" >> return Edit

loadCmd :: Parser Cmd
loadCmd = spStr "load" >> Load <$> many1 (noneOf "\n ") `sepBy` (many $ char ' ')

toplevel :: Parser Toplevel
toplevel = spaces >> choice
  [ TCmd <$> (spChar ':' >> cmd)
  , TQuery <$> try query
  , TRule <$> rule
  ]

parse :: Parser a -> SourceName -> String -> Either ParseError a
parse = flip runParser $ 0
