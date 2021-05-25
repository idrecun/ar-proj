module ParseLogic where

import Logic

import Data.Char
import Text.ParserCombinators.ReadP

readEql   = skipSpaces >> string   "<=>" >>  return (Binary Eql)
readImpl  = skipSpaces >> string    "=>" >>  return (Binary Impl)
readOr    = skipSpaces >> char       '|' >>  return (Binary Or)
readAnd   = skipSpaces >> char       '&' >>  return (Binary And)
readNot   = skipSpaces >> char       '~' >>  return (Not)
readT     = skipSpaces >> string  "true" >>  return LogicT
readF     = skipSpaces >> string "false" >>  return LogicF
readLBr   = skipSpaces >> char       '('
readRBr   = skipSpaces >> char       ')'
readAtom  = skipSpaces >> munch1 isAlpha >>= return <$> Atom


parseEql  = chainr1 parseImpl readEql
parseImpl = chainr1 parseOr   readImpl
parseOr   = chainl1 parseAnd  readOr
parseAnd  = chainl1 parseNot  readAnd
parseNot  = (readNot <*> parseNot)
        <++ parseBase
parseBase = between readLBr readRBr parseEql
        <++ readT
        <++ readF
        <++ readAtom

parse :: String -> (Logic, String)
parse str = last $ readP_to_S parseEql str
