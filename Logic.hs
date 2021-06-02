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
