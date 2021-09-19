module Main where

import Data.List
import System.Random
import Data.Array.IO
import Control.Monad
import Control.Monad (when)
import Data.Char

type NumberOfGenes       = Int
type ObjFunction         = Genotype -> Phenotype

type MaxGeneration       = Int
type PopulationSize      = Int
type CrossProbability    = Double
type MutationProbability = Double

data ProblemData = ProblemData NumberOfGenes ObjFunction

data Algorithm = Algorithm CrossProbability 
                           MutationProbability 
                           PopulationSize 
                           MaxGeneration
                 deriving Show

type Gene      = Int
type Genotype  = [Gene]
type Phenotype = Double

data Solution = Solution Genotype Phenotype
                deriving Show

data Population = Population [Genotype] deriving (Eq, Show)

type Generation = Int

randPerc :: IO Double
randPerc = randomRIO (0, 1)

randBit :: IO Integer
randBit = randPerc >>= \p -> if p < 0.5 then
  return 1 else return 0

randomGenotype :: NumberOfGenes -> IO Genotype
randomGenotype n = sequence $ replicate n $ randomRIO (0,1 :: Int)

randomPopulation :: NumberOfGenes -> PopulationSize -> IO Population
randomPopulation n m = (sequence $ [randomGenotype n | _ <- [1..m]]) >>= (\xs -> return $ Population xs)

cross :: CrossProbability -> Population -> IO Population
cross cp (Population pop) = do
  shuffle1 <- shuffle pop
  shuffle2 <- shuffle shuffle1
  let ps1  = map g $ group' 2 shuffle1
      ps2  = map g $ group' 2 shuffle2
      ps   = zip ps1 ps2
      pop' = join <$> (sequence $ map simpleCrossover ps)
      in do p <- pop'; return (Population p)
  where g [a,b] = if f a > f b then a else b
        g [x]   = x
        simpleCrossover :: (Genotype,Genotype) -> IO [Genotype]
        simpleCrossover (g1,g2) = do n <- randPerc
                                     if cp > n then do
                                                    k <- randomRIO (1, (length gamma) -1)
                                                    let g1' = splitAt' k g1
                                                        g2' = splitAt' k g2
                                                        in return [fst g1' ++ snd g1', fst g2' ++ snd g1']
                                                else return [g1,g2]

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs     = ([], xs)
splitAt' _ []     = ([], [])
splitAt' n (x:xs) = (x:xs', xs'')
  where
    (xs', xs'') = splitAt' (n - 1) xs

group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' n l
  | n > 0 = (take n l) : (group' n (drop n l))
  | otherwise = error "Negative or zero n"

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

mut :: MutationProbability -> Population -> IO Population
mut mp (Population pop) = Population <$> (sequence $ sequence <$> map (\genotype -> map bitFlip genotype) pop)
  where bitFlip gene = do
                  n <- randPerc
                  if mp > n then return $ if gene == 0 then 1 else 0
                            else return gene

survivalSelection :: PopulationSize -> Population -> Population -> Population
survivalSelection ps (Population p1) (Population p2) = Population $ take ps $ sortBy sf (p1 ++ p2)
  where sf g1 g2
          | f g1 < f g2 = LT
          | f g1 > f g2 = GT
          | otherwise   = EQ

printPopulation :: Population -> IO ()
printPopulation (Population pop) = mapM_ print pop

gamma :: Genotype
gamma = [if even x then 0 else 1 | x <- [0,1..n] ]
  where n = 128

f :: Genotype -> Phenotype
f genotype = let xs = zip genotype gamma
             in  sum $ map (\(a,b) -> if a == b then 0 else 1) xs

problemData = ProblemData (length gamma) f
algorithm   = Algorithm 0.9 (1/(fromIntegral (length gamma))) 100 100

  -- (m+l) strategy
runAlgorithm :: Algorithm -> ProblemData -> IO ()
runAlgorithm (Algorithm cp mp ps mg) (ProblemData gn obj) = do
  population <- randomPopulation gn ps
  helper 0 population
    where helper :: Generation -> Population -> IO ()
          helper gen pop = do
                           q  <- cross cp pop
                           q'@(Population qs) <- mut mp q
                           nextPop@(Population p) <- return $ survivalSelection ps q' pop
                           Solution genotype phenotype <- return $ Solution (head p) (f (head p))
                           print $ "Generation " ++ (show gen) ++ ":  " ++ join (map show genotype) ++ " -> " ++ show phenotype
                           if gen < mg then helper (gen+1) nextPop
                                       else do putStrLn ""
                                               print $ "Solution: " ++ join (map show genotype) ++ " -> " ++ show phenotype

-- (m,l) strategy
runAlgorithm' :: Algorithm -> ProblemData -> IO ()
runAlgorithm' (Algorithm cp mp ps mg) (ProblemData gn obj) = do
  population <- randomPopulation gn ps
  helper 0 population
    where helper :: Generation -> Population -> IO ()
          helper gen pop = do
                           q  <- cross cp pop
                           q' <- mut mp q
                           nextPop@(Population p) <- return $ survivalSelection ps q' q'
                           Solution genotype phenotype <- return $ Solution (head p) (f (head p))
                           print $ "Generation " ++ (show gen) ++ ":  " ++ join (map show genotype) ++ " -> " ++ show phenotype
                           if gen < mg then helper (gen+1) nextPop
                                       else do putStrLn ""
                                               print $ "Solution: " ++ join (map show genotype) ++ " -> " ++ show phenotype

-- (m,l) strategy (with ellitism)
runAlgorithm'' :: Algorithm -> ProblemData -> IO ()
runAlgorithm'' (Algorithm cp mp ps mg) (ProblemData gn obj) = do
  population <- randomPopulation gn ps
  helper 0 population
    where helper :: Generation -> Population -> IO ()
          helper gen pop = do
                           q  <- cross cp pop
                           q' <- mut mp q
                           nextPop@(Population p) <- return $ survivalSelection ps q' q'
                           (Population pBest) <- return $ survivalSelection 1 pop pop
                           nextPop'@(Population p') <- return $ addBestFromP (pBest !! 0) $ removeWorst nextPop
                           nextPop''@(Population p'') <- return $ survivalSelection ps nextPop' nextPop'
                           Solution genotype phenotype <- return $ Solution (head p') (f (head p'))
                           print $ "Generation " ++ (show gen) ++ ":  " ++ join (map show genotype) ++ " -> " ++ show phenotype
                           if gen < mg then helper (gen+1) nextPop
                                       else do putStrLn ""
                                               print $ "Solution: " ++ join (map show genotype) ++ " -> " ++ show phenotype
          removeWorst :: Population -> Population
          removeWorst (Population p) = Population $ reverse $ tail $ sortBy sf p
            where sf g1 g2
                    | f g1 > f g2 = LT
                    | f g1 < f g2 = GT
                    | otherwise   = EQ
          addBestFromP :: Genotype -> Population -> Population
          addBestFromP genotype (Population p) = Population $ genotype:p

main :: IO ()
main = runAlgorithm algorithm problemData