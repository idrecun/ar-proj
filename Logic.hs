module Logic where

data BinaryOp = And | Or | Impl | Eql

instance Show BinaryOp where
  show And  = "&"
  show Or   = "|"
  show Impl = "=>"
  show Eql  = "<=>"

data Logic = Binary BinaryOp Logic Logic
           | Not Logic
           | Atom String
           | LogicT
           | LogicF

instance Show Logic where
  show (Binary op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
  show (Not f) = '~' : (show f)
  show LogicT  = "true"
  show LogicF  = "false"
  show (Atom v)  = v

-- Removes all constants from the formula
-- No other modifications are made
simplify :: Logic -> Logic

simplify (Binary And LogicF _) = LogicF
simplify (Binary And _ LogicF) = LogicF
simplify (Binary And LogicT r) = simplify r
simplify (Binary And l LogicT) = simplify l
simplify (Binary And l r) = Binary And (simplify l) (simplify r)

simplify (Binary Or LogicT _) = LogicT
simplify (Binary Or _ LogicT) = LogicT
simplify (Binary Or LogicF r) = simplify r
simplify (Binary Or l LogicF) = simplify l
simplify (Binary Or l r) = Binary Or (simplify l) (simplify r)

simplify (Not LogicT) = LogicF
simplify (Not LogicF) = LogicT
simplify (Not f) = Not (simplify f)

simplify (Binary Impl LogicF _) = LogicT
simplify (Binary Impl _ LogicT) = LogicT
simplify (Binary Impl LogicT r) = simplify r
simplify (Binary Impl l LogicF) = Not (simplify l)
simplify (Binary Impl l r) = Binary Impl (simplify l) (simplify r)

simplify (Binary Eql LogicT r) = simplify r
simplify (Binary Eql l LogicT) = simplify l
simplify (Binary Eql LogicF r) = Not (simplify r)
simplify (Binary Eql l LogicF) = Not (simplify l)
simplify (Binary Eql l r) = Binary Eql (simplify l) (simplify r)

simplify (Atom s) = Atom s
simplify LogicT = LogicT
simplify LogicF = LogicF
