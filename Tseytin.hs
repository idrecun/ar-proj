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
tseytinSingle s LogicT = []
tseytinSingle s LogicF = [[Neg s]]

tseytin :: Int -> Logic -> LogicCNF -> (Int, String, LogicCNF)
tseytin n (Atom x) cnf = (n, x, cnf)
tseytin n (Not x)  cnf = (n_x + 1, nameAtom n_x, clauses ++ cnf_x)
  where (n_x, name_x, cnf_x) = tseytin n x cnf
        clauses = tseytinSingle (nameAtom n_x) (Not (Atom name_x))
tseytin n (Binary op left right) cnf = (n_r + 1, nameAtom n_r, clauses ++ cnf_l_r)
  where (n_l, name_l, cnf_l)   = tseytin n left cnf
        (n_r, name_r, cnf_l_r) = tseytin n_l right cnf_l
        clauses = tseytinSingle (nameAtom n_r) (Binary op (Atom name_l) (Atom name_r))
tseytin n constF cnf = (n + 1, nameAtom n, clauses ++ cnf)
  where clauses = tseytinSingle (nameAtom n) constF

tseytinTransform :: Logic -> LogicCNF
tseytinTransform f = [Pure name] : cnf
  where (_, name, cnf) = tseytin 1 (simplify f) []
