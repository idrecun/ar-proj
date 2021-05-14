module Tseytin where

import Logic


data LogicLiteral = Pure { varname :: String } | Neg { varname :: String } deriving (Show)
type LogicCNF = [[LogicLiteral]]

nameAtom :: Int -> String
nameAtom n = '$' : show n

tseytinSingle :: String -> Logic -> LogicCNF
tseytinSingle s (Binary And (Atom a) (Atom b)) = [[Neg s, Pure a], [Neg s, Pure b], [Pure s, Neg a, Neg b]]
tseytinSingle s (Binary Or (Atom a) (Atom b)) = [[Neg s, Pure a, Pure b], [Neg a, Pure s], [Neg b, Pure s]]
tseytinSingle s (Binary Impl (Atom a) (Atom b)) = [[Neg s, Neg a, Pure b], [Pure a, Pure s], [Neg b, Pure s]]
tseytinSingle s (Binary Eql (Atom a) (Atom b)) = [[Neg s, Neg a, Pure b], [Neg s, Neg b, Pure a], [Pure s, Pure a, Pure b], [Pure s, Neg a, Neg b]]
tseytinSingle s (Not (Atom a)) = [[Neg s, Neg a], [Pure s, Pure a]]

tseytin :: Int -> Logic -> (Int, String, LogicCNF)
tseytin n (Atom x) = (n, x, [])
tseytin n (Not x) = (n_x + 1, nameAtom n_x, cnf_x ++ tseytinSingle (nameAtom n_x) (Not (Atom name_x)))
  where (n_x, name_x, cnf_x) = tseytin n x
tseytin n (Binary op left right) = (n_r + 1, nameAtom n_r, cnf_l ++ cnf_r ++ tseytinSingle (nameAtom n_r) (Binary op (Atom name_l) (Atom name_r)))
  where (n_l, name_l, cnf_l) = tseytin n left
        (n_r, name_r, cnf_r) = tseytin n_l right

tseytinTransform :: Logic -> LogicCNF
tseytinTransform f = [Pure name] : cnf
  where (_, name, cnf) = tseytin 0 (simplify f)
