module Utilities.Number where
import System.Environment
import Numeric

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * (if x == '0' then 0 else 1) in
    bin2dig' old xs
