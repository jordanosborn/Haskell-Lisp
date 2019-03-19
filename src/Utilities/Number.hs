module Utilities.Number where
import System.Environment
import Numeric
import qualified Utilities.Types

oct2dig x = fst $ head (readOct x)
hex2dig x = fst $ head (readHex x)

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * (if x == '0' then 0 else 1) in
    bin2dig' old xs


toDouble :: Utilities.Types.LispVal -> Double
toDouble(Utilities.Types.Float f) = realToFrac f
toDouble(Utilities.Types.Number n) = fromIntegral n