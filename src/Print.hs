{-# OPTIONS_GHC -Wall #-}
module Print where

import Types

import Data.List

class PPrint a where
    pprint :: a -> String

instance PPrint a => PPrint [a] where
    pprint = unlines . map pprint

instance PPrint Atom where
    pprint (Atom s) = s

instance PPrint Var where
    pprint (Var s) = s

instance PPrint Rule where
    pprint (Rule (RHead rname args) (RBody conjs)) =
      concat [ pprint rname, "(", intercalate ", " (map pprint args), ")"
             , if null conjs
                 then "."
                 else concat [ " :- "
                             , intercalate ", " (map pprint conjs)
                             , "."
                             ]
             ]

instance PPrint Term where
    pprint (TAtom a) = pprint a
    pprint (TVar v) = pprint v
    pprint (TComp comp) = pprint comp
    pprint TVGen{} = error "TVGen in pprint"

instance PPrint Compound where
    pprint (Compound fName args) =
      concat [ pprint fName, "(", intercalate ", " (map pprint args), ")" ]
