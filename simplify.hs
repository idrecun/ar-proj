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

simplify (Not (Binary And l r)) = Binary Or (Not (simplify l)) (Not (simplify r))
simplify (Not (Binary Or l r)) = Binary And (Not (simplify l)) (Not (simplify r))
simplify (Not (Binary op l r)) = simplify (Not (simplify (Binary op l r)))
simplify (Not (Not f)) = simplify f
simplify (Not LogicT) = LogicF
simplify (Not LogicF) = LogicT
simplify (Not f) = Not (simplify f)

simplify (Binary Impl l r) = simplify (Binary Or (Not l) r)
simplify (Binary Eql l r) = simplify (Binary And (Binary Impl l r) (Binary Impl r l))
simplify (Atom s) = Atom s
simplify LogicT = LogicT
simplify LogicF = LogicF
