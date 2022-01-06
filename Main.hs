{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char (isDigit)
import Data.List (group, groupBy)
import Text.Read ()
import Prelude

data CsvColumn = CsvInt Integer | CsvString String

instance Show CsvColumn where
  show (CsvInt val) = show val
  show (CsvString val) = show val

instance Eq CsvColumn where
  (CsvInt val) == (CsvInt val1) = val == val1
  (CsvString val) == (CsvString val1) = val == val1
  (CsvString val) == (CsvInt val1) = False
  (CsvInt val) == (CsvString val1) = False

instance Ord CsvColumn where
  (CsvInt val) `compare` (CsvInt val1) = val `compare` val1
  (CsvInt val) < (CsvInt val1) = val < val1
  (CsvInt val) > (CsvInt val1) = val > val1
  (CsvInt val) <= (CsvInt val1) = val <= val1
  (CsvInt val) >= (CsvInt val1) = val >= val1

instance Num CsvColumn where
  (+) (CsvInt a) (CsvString b) = CsvInt a
  (+) (CsvString a) (CsvInt b) = CsvInt b
  (+) (CsvInt a) (CsvInt b) = CsvInt $ a + b
  (+) (CsvString a) (CsvString b) = CsvString $ a ++ b
  (*) (CsvInt a) (CsvInt b) = CsvInt $ a * b
  abs (CsvInt a) = CsvInt $ abs a
  signum (CsvInt a) = CsvInt $ signum a
  fromInteger = CsvInt
  negate (CsvInt a) = CsvInt $ - a

data CsvRecord = CsvRecord
  { dataId :: CsvColumn,
    count :: CsvColumn,
    desc :: CsvColumn
  }
  deriving (Show)

splitCsv :: String -> [String]
splitCsv = lines

parseField :: String -> CsvColumn
parseField "" = CsvString ""
parseField field =
  if all isDigit field
    then CsvInt (read field :: Integer)
    else CsvString field

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

csvLineToRecord :: String -> CsvRecord
csvLineToRecord line =
  let items = wordsWhen (==',') line
      csvId = parseField $ head items
      csvCount = parseField $ items !! 1
      csvDesc = parseField $ items !! 2
   in CsvRecord {dataId = csvId, count = csvCount, desc = csvDesc}

mapCsvInfoToRecord :: [String] -> [CsvRecord]
mapCsvInfoToRecord = map csvLineToRecord

selectColumn :: CsvRecord -> String -> Maybe CsvColumn
selectColumn rec "dataId" = Just $ dataId rec
selectColumn rec "count" = Just $ count rec
selectColumn rec "desc" = Just $ desc rec
selectColumn rec _ = Nothing

selectWithCond :: String -> [CsvRecord] -> [Maybe CsvColumn]
selectWithCond col = map (`selectColumn` col)

myPrint :: Show a => Maybe a -> IO ()
myPrint (Just x) = print x
myPrint n = print n

sqlWhere :: (CsvColumn -> CsvColumn -> Bool) -> CsvColumn -> Maybe [CsvColumn] -> Maybe [CsvColumn]
sqlWhere op (CsvInt value) (Just records) = Just $ filter (`op` CsvInt value) records
sqlWhere (==) (CsvString value) (Just records) = Just $ filter (== CsvString value) records

sqlGroupBy :: String -> [CsvRecord] -> [[CsvRecord]]
sqlGroupBy column = groupBy (\x y -> parseInput column x == parseInput column y)

parseInput :: String -> (CsvRecord -> CsvColumn)
parseInput "dataId" = dataId
parseInput "count" = count
parseInput "desc" =  desc

mergeColumns :: String -> [CsvRecord] -> CsvRecord
mergeColumns "dataId" = foldr (\x y -> CsvRecord {dataId = dataId x, count = count x + count y, desc = desc x + desc y}) CsvRecord {dataId = 0, count = 0, desc = 0}
mergeColumns "count" = foldr (\x y -> CsvRecord {dataId = dataId x + dataId y, count = count x, desc = desc x + desc y}) CsvRecord {dataId = 0, count = 0, desc = 0}
mergeColumns "desc"= foldr (\x y -> CsvRecord {dataId = dataId x + dataId y, count = count x + count y, desc = desc x}) CsvRecord {dataId = 0, count = 0, desc = 0}

main :: IO ()
main = do
  dataCsv <- readFile "File.csv"
  let splitted = splitCsv dataCsv
  let items = tail splitted
  let colNames = head splitted

  let records = mapCsvInfoToRecord items
  -- print records
  let firstRecord = head records
  let col = "*"

  if col /= "*"
    then
			do
				let selected = sequence $ selectWithCond col records
				myPrint selected
				let where1 = sqlWhere (==) (CsvInt 5) selected
				myPrint where1
    else
			do
				let test = sqlGroupBy "dataId" records
				let folded = map (mergeColumns "dataId") test
				print folded
				print test
				-- print records

