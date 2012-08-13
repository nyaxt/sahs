module Main(main) where

import Data.List( sortBy, tails )
import Data.Tuple( swap )
import qualified Data.Array.Unboxed as AU
import System.Environment( getArgs )

type Index = Int
type IntAry = AU.UArray Int Int
type SA = IntAry
type PsiA = IntAry

-- str to list of (offset, suffix)
tailsI :: String -> [(Int, String)]
tailsI str = tailsI_ 0 str
  where tailsI_ :: Int -> String -> [(Int, String)]
	tailsI_ off "" = [(off, "")]
	tailsI_ off xxs@(_:xs) = (off, xxs) : tailsI_ (off+1) xs

-- construct suffix array in list
genSAList :: String -> [Int]
genSAList str = map fst $ sortBy (\a b -> compare (snd a) (snd b)) $ tailsI str

-- construct suffix array
genSA :: String -> SA
genSA src = AU.listArray (0, length src) (genSAList src)

-- a[b] -> b[a]
invArray :: IntAry -> IntAry
invArray a = AU.array (AU.bounds a) (map swap $ AU.assocs a)

-- return length of IntAry. assumes that IntAry start w/ index 0
arrayLen :: IntAry -> Int
arrayLen a = 1 + (snd $ AU.bounds a)

-- given function f(i) which produces value for index i, generate IntAry length n
iterateAry :: (Index -> Int) -> Int -> IntAry
iterateAry f n = AU.array (0, n-1) $ (flip map) [0..n-1] $ \i -> (i, f i)

-- generate \Psi array from SA in naive way
genPsiA :: SA -> PsiA
genPsiA sa =
  let n = arrayLen sa
      isa = invArray sa
  in (flip iterateAry) n $ \i -> isa AU.! (((sa AU.! i) + 1) `mod` n)

main :: IO ()
-- main = getArgs >>= (print . show . genSA . head)
main = putStrLn $ show $ AU.elems $ genPsiA $ genSA "abracadabra"
