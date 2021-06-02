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

showAccumulate :: String -> Logic -> String
showAccumulate res (Binary op l r) = "(" ++ showAccumulate (" " ++ show op ++ " " ++ (showAccumulate (')' : res) r)) l
showAccumulate res (Not f)  = '~' : (showAccumulate res f)
showAccumulate res LogicT   = "true" ++ res
showAccumulate res LogicF   = "false" ++ res
showAccumulate res (Atom v) = v ++ res

instance Show Logic where
  show = showAccumulate ""
