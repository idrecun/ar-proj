import Logic
import ParseLogic
import Tseytin
import Data.Map
import Data.Set

type VarMap = Map String Int

distinct list = Data.Set.toList (Data.Set.fromList list)

getVarList :: LogicCNF -> [String]
getVarList formula = distinct (Prelude.map varname (concat formula))

getVarMap :: LogicCNF -> VarMap
getVarMap formula = Data.Map.fromList (zip varlist [1..length varlist])
  where varlist = getVarList formula

literalString :: VarMap -> LogicLiteral -> String
literalString m (Pure v) = show (m ! v) ++ " "
literalString m (Neg v)  = "-" ++ show (m ! v) ++ " "

dimacs :: LogicCNF -> String
dimacs formula = "c " ++ show (Data.Map.toList varmap) ++ "\ncnf " ++ show (length varmap) ++ " " ++ show (length formula) ++ "\n" ++ clauses
  where varmap = getVarMap formula
        clauses = concat (Prelude.map ((++ "0\n") . concat . (Prelude.map (literalString varmap))) formula)


main :: IO ()
main = do
  str <- getLine
  let formula = ParseLogic.parser str
  let cnf = Tseytin.tseytinTransform formula
  putStrLn (dimacs cnf)