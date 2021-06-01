import Logic
import ParseLogic
import Tseytin
import Data.List
import Data.Ord
import Data.Map
import Data.Set
import System.IO

type VarMap = Map String Int

distinct list = Data.Set.toList (Data.Set.fromList list)

varComparator :: String -> (String, Int)
varComparator ('$' : t) = ("$", read t)
varComparator s = (s, 0)

getVarList :: LogicCNF -> [String]
getVarList formula = sortBy (comparing varComparator) (distinct (Prelude.map varname (concat formula)))

getVarMap :: LogicCNF -> VarMap
getVarMap formula = Data.Map.fromList (zip varlist [1..length varlist])
  where varlist = getVarList formula

literalString :: VarMap -> LogicLiteral -> String
literalString m (Pure v) = show (m ! v) ++ " "
literalString m (Neg v)  = "-" ++ show (m ! v) ++ " "

dimacs :: Logic -> LogicCNF -> String
dimacs formula cnf = "c " ++ (show $ formula) ++ "\nc " ++ show (sortBy (comparing snd) (Data.Map.toList varmap)) ++ "\np cnf " ++ show (length varmap) ++ " " ++ show (length cnf) ++ "\n" ++ clauses
  where varmap = getVarMap cnf
        clauses = concat (Prelude.map ((++ "0\n") . concat . (Prelude.map (literalString varmap))) cnf)


main :: IO ()
main = do
  str <- getLine
  let parsed = parse str
  if snd parsed /= ""
    then hPutStrLn stderr ("Error parsing " ++ (show $ snd parsed))
    else putStr $ dimacs (fst parsed) (tseytinTransform $ fst parsed)
