module Main(main) where

import Data.List( sortBy, tails )
import Data.Tuple( swap )
import qualified Data.Array.Unboxed as AU
import System.Environment( getArgs )

type Index = Int
type IntAry = AU.UArray Int Int
type SA = IntAry
type PhiA = IntAry

tailsI_ :: Int -> String -> [(Int, String)]
tailsI_ off "" = [(off, "")]
tailsI_ off xxs@(_:xs) = (off, xxs) : tailsI_ (off+1) xs

tailsI :: String -> [(Int, String)]
tailsI str = tailsI_ 0 str

genSAAry :: String -> [Int]
genSAAry str = map fst $ sortBy (\a b -> compare (snd a) (snd b)) $ tailsI str

genSA :: String -> SA
genSA src = AU.listArray (0, length src) (genSAAry src)

invArray :: IntAry -> IntAry
invArray a = AU.array (AU.bounds a) (map swap $ AU.assocs a)

arrayLen :: IntAry -> Int
arrayLen a = 1 + (snd $ AU.bounds a)

iterateAry :: (Index -> Int) -> Int -> IntAry
iterateAry f n = AU.array (0, n-1) $ (flip map) [0..n-1] $ \i -> (i, f i)

genPhiA :: SA -> PhiA
genPhiA sa =
  let n = arrayLen sa
      isa = invArray sa
  in (flip iterateAry) n $ \i -> isa AU.! (((sa AU.! i) + 1) `mod` n)

-- main :: IO ()
-- main = getArgs >>= (print . show . genSA . head)
main = putStrLn $ show $ AU.elems $ genPhiA $ genSA "abracadabra"
