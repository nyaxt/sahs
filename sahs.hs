module Main(main) where

import Control.Monad ( liftM )
import Data.List( sortBy, tails )
import Data.Tuple( swap )
import qualified Data.Array.Unboxed as AU
import System.Environment( getArgs )
import Debug.Trace( trace )

type Index = Int
type IntAry = AU.UArray Int Int
type BoolAry = AU.UArray Int Bool
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

-- construct B from A
genB :: SA -> BoolAry
genB a = AU.listArray (AU.bounds a) $ map odd (AU.elems a)

-- a[b] -> b[a]
invArray :: IntAry -> IntAry
invArray a = AU.array (AU.bounds a) (map swap $ AU.assocs a)

-- return length of IntAry. assumes that IntAry start w/ index 0
arrayLen :: (AU.IArray a e) => a Int e -> Int
arrayLen a = 1 + (snd $ AU.bounds a)

-- given function f(i) which produces value for index i, generate IntAry length n
iterateAry :: (Index -> Int) -> Int -> IntAry
iterateAry f n = AU.listArray (0, n-1) $ map f [0..n-1]

(!<) :: (AU.IArray a e) => a Int e -> Int -> e
(!<) a i = a AU.! (i `mod` n)
  where n = arrayLen a

psi :: Int -> SA -> IntAry -> Int -> Int
psi n sa isa i = isa !< ((sa AU.! i) + 1)

-- generate \Psi array from SA in naive way
genPsiA :: SA -> PsiA
genPsiA sa =
  let n = arrayLen sa
      isa = invArray sa
  in (flip iterateAry) n $ psi n sa isa
  
ceildiv :: Int -> Int -> Int
ceildiv a b = (a + b-1) `div` b
  
genPsiA0 :: SA -> PsiA
genPsiA0 sa =
  let n = arrayLen sa
      isa = invArray sa
  in AU.listArray (0, (n `ceildiv` 2) - 1) $ map (psi n sa isa) $ filter (\i -> even $ sa AU.! i) [0..n-1]
  
genA1 :: SA -> SA
genA1 sa =
  let n = arrayLen sa
  in AU.listArray (0, (n `div` 2) - 1) $ map (\x -> x `div` 2) $ filter odd $ AU.elems sa
  
toZeroOne :: Bool -> Int
toZeroOne True  = 1
toZeroOne False = 0

log2c :: Int -> Int
log2c x = ceiling $ (log (fromIntegral x)) / (log 2)

genNextLevel :: SA -> (PsiA, BoolAry, SA)
genNextLevel sa =
  let n   = arrayLen sa
      isa = invArray sa
      genNextLevel_ :: (Index, Int) -> ([Int], [Bool], [Int]) -> ([Int], [Bool], [Int])
      genNextLevel_ (i, sai) (pn, bn, san) | even sai  = ((psi n sa isa i):pn, False:bn, san)
                                           | otherwise = (pn, True:bn, (sai `div` 2):san)
      (pnl, bnl, sanl) = foldr genNextLevel_ ([], [], []) $ AU.assocs sa
      pn  = AU.listArray (0, (n `ceildiv` 2) - 1) pnl
      bn  = AU.listArray (0, n - 1) bnl
      san = AU.listArray (0, (n `div` 2) - 1) sanl
  in (pn, bn, san)
  
thd3 :: (a, b, c) -> c
thd3 (f, s, t) = t
  
compressGV :: SA -> [(PsiA, BoolAry, SA)]
compressGV sa =
  let n = arrayLen sa
      h = 1 -- log2c $ log2c n
      genNextLevel_  = genNextLevel
  in reverse $ take (h+1) $ iterate (genNextLevel . thd3) (AU.array (0, 0) [], AU.array (0, 0) [], sa)
  
rank0 :: BoolAry -> Index -> Int
rank0 _ (-1) = 0
rank0 ba i | i >= (arrayLen ba) = rank0 ba $ (arrayLen ba)-1
           | ba AU.! i = rank0 ba (i-1)
           | otherwise = 1 + rank0 ba (i-1)
  
rank1 :: BoolAry -> Index -> Int
rank1 _ (-1) = 0
rank1 ba i | i >= (arrayLen ba) = rank1 ba $ (arrayLen ba)-1
           | ba AU.! i = 1 + rank1 ba (i-1)
           | otherwise = rank1 ba (i-1)
	   
lookupGV :: [(PsiA, BoolAry, SA)] -> Index -> Int
lookupGV ((_, _, sa):[]) i = trace ("lookupGVL "++(show i)) $ sa !< i
lookupGV gv@((pa, ba, sa):gvn) i | (ba !< i) = trace ("lookupGV< i="++(show i)++" 2 * "++(show (lookupGV gvn (rank1 ba i)))) $ 2 * (lookupGV gvn (rank1 ba i))
                                 | otherwise = trace ("lookupGVo i="++(show i)++" -1 +"++(show ((lookupGV gv (pa !< (rank0 ba i))) - 1))) $ (lookupGV gv (pa !< (rank0 ba i))) - 1
  
{-
	  let psi0 = genPsiA0 sa
	  putStrLn $ "P0: " ++ (show $ AU.elems $ psi0)
	  let a1 = genA1 sa
	  putStrLn $ "A1: " ++ (show $ AU.elems $ a1j
 -}

main :: IO ()
-- main = getArgs >>= (print . show . genSA . head)
main = do src <- liftM head getArgs
          putStrLn $ "src: " ++ src
          putStrLn $ "len: " ++ (show $ length src)
	  let sa = genSA src
	  putStrLn $ "A: " ++ (show $ AU.elems $ sa)
	  let b = genB sa
	  putStrLn $ "B: " ++ (show $ map toZeroOne $ AU.elems $ b)
	  putStrLn $ "B rank0: " ++ (show $ map (rank0 b) $ [0..15])
	  putStrLn $ "B rank1: " ++ (show $ map (rank1 b) $ [0..15])
	  let psi = genPsiA sa
	  putStrLn $ "P: " ++ (show $ AU.elems $ psi)
	  let (psi0, b0, a1) = genNextLevel sa
	  putStrLn $ "P0: " ++ (show $ AU.elems $ psi0)
	  putStrLn $ "B0: " ++ (show $ map toZeroOne $ AU.elems $ b0)
	  putStrLn $ "A1: " ++ (show $ AU.elems $ a1)
	  let (psi1, b1, a2) = genNextLevel a1
	  putStrLn $ "P1: " ++ (show $ AU.elems $ psi1)
	  putStrLn $ "B1: " ++ (show $ map toZeroOne $ AU.elems $ b1)
	  putStrLn $ "A2: " ++ (show $ AU.elems $ a2)
	  let n = arrayLen sa
	  let h = log2c $ log2c n
	  putStrLn $ "h: " ++ (show h)
	  let gv = compressGV sa
	  putStrLn $ "A: " ++ (show $ map (lookupGV gv) $ [0..(n-1)])
