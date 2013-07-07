{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
module Print where

import           Types

import           Data.List
import qualified Data.Map  as M


class PPrint a where
    pprint :: a -> String

instance PPrint a => PPrint [a] where
    pprint = unlines . map pprint

instance PPrint Atom where
    pprint (Atom s) = s

instance PPrint Var where
    pprint (Var s) = s

instance PPrint Rule where
    pprint (Rule (RHead (Atom "true") []) (RBody [])) = ""
    pprint (Rule (RHead rname args) (RBody conjs)) =
      concat [ pprint rname, "(", intercalate ", " (map pprint args), ")"
             , if | null conjs -> "."
                  | conjs == [Compound (Atom "true") []] -> "."
                  | otherwise ->
                      concat [ " :- "
                             , intercalate ", " (map pprint conjs)
                             , "."
                             ]
             ]

instance PPrint Term where
    pprint (TAtom a) = pprint a
    pprint (TVar v) = pprint v
    pprint (TComp comp) = pprint comp
    pprint (TVGen i) = "_V" ++ show i

instance PPrint Compound where
    pprint (Compound fName args) =
      concat [ pprint fName, "(", intercalate ", " (map pprint args), ")" ]

instance PPrint (M.Map Var Term) where
    pprint = intercalate ", " . M.foldWithKey foldfn []
      where
        foldfn :: Var -> Term -> [String] -> [String]
        foldfn var term acc = acc ++ [ concat [ pprint var, " -> ", pprint term ] ]
