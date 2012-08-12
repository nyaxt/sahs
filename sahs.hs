module Main(main) where

import Data.List( sort, tails )
import qualified Data.Array.IArray as IA
import System.Environment( getArgs )

type SA = IA.Array Int String
type PhiA = IA.Array Int Int

genSA :: String -> SA
genSA src = (IA.listArray (0, length src) . sort . tails) src

-- genPhiA :: SA -> PhiA
-- genPhiA sa = 

main :: IO ()
main = getArgs >>= (print . show . genSA . head)
