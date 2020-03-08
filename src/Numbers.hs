module Numbers
  ( convertFromTo
  ) where

import           Data.Char
import           Data.List

getIntegerFromLetter :: Char -> Integer
getIntegerFromLetter char
  | ord char >= ord 'a' && ord char <= ord 'z' = toInteger $ ord char - ord 'a' + 10
  | ord char >= ord '0' && ord char <= ord '9' = toInteger $ ord char - ord '0'
  | ord char >= ord 'A' && ord char <= ord 'Z' = toInteger $ ord char - ord 'A' + 36
  | otherwise = error "this is invalid char"

getLetterFromInteger :: Integer -> Char
getLetterFromInteger integer
  | integer >= 0 && integer <= 9 = chr (fromIntegral integer + ord '0')
  | integer >= 10 && integer <= 35 = chr (fromIntegral integer + ord 'a' - 10)
  | integer >= 36 && integer <= 61 = chr (fromIntegral integer + ord 'A' - 36)
  | otherwise = error "this integer can\'t be represented as char"

toDecimal :: Integer -> String -> String
toDecimal 1 "" = "0"
toDecimal 1 snumber
  | all (== '1') snumber = show $ length snumber - 1
  | otherwise = error "invalid characters for this base"
toDecimal base snumber
  | base >= 1 && base <= 61 = show (answer intArr)
  | otherwise = error "base is invalid"
  where
    intArr = map getIntegerFromLetter snumber
    answer intArr
      | all (< base) intArr = foldl (\prev curr -> curr + prev * base) 0 intArr
      | otherwise = error "char is not valid for this base"

isNotDigit :: Char -> Bool
isNotDigit char = ord char < 48 || ord char > 57

fromDecimal :: Integer -> String -> String
fromDecimal 1 snumber
  | any isNotDigit snumber = error "incorrect number"
  | otherwise = replicate (read snumber) '1'
fromDecimal toBase snumber
  | any isNotDigit snumber = error "incorrect number"
  | toBase <= 61 && toBase > 1 = map getLetterFromInteger (answer number [])
  | otherwise = error "invalid base"
  where
    number = read snumber
    answer 0 acc = acc
    answer number acc = answer (number `div` toBase) (number `mod` toBase : acc)

convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber
