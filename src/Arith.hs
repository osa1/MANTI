{-# OPTIONS_GHC -Wall #-}

module Arith (evalArith) where

import           Types


evalArith :: Term -> Manti Term
evalArith t@TAtom{} = return t
evalArith t@TVar{}  = return t
evalArith (TComp (Compound (Atom arithFn) args)) = applyArithFn arithFn args
evalArith t@TInt{}  = return t
evalArith TVGen{}   = error "VGen in evalArith."

applyArithFn :: String -> [Term] -> Manti Term
applyArithFn "+" [TInt i1, TInt i2] = return $ TInt (i1 + i2)
applyArithFn "-" [TInt i1, TInt i2] = return $ TInt (i1 - i2)
