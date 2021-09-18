{-# LANGUAGE GADTs #-}

module Main where

type NumberOfGenes       = Int
type MaxGeneration       = Int
type PopulationSize      = Int
type CrossProbability    = Double
type MutationProbability = Double

data Algorithm = Algorithm CrossProbability 
                           MutationProbability 
                           PopulationSize 
                           MaxGeneration
                 deriving Show

data Gene = Zero | One deriving Show

data Solution = Genotype [Gene] | Phenotype Double
                deriving Show

data Population = Population [Solution]
                  deriving Show

-- Returns a random number between 0 and 1
randInt = undefined

-- Returns a a random bit
randBit = undefined

-- Objective function
f = undefined

-- compute fitness
evalSolution :: Solution -> Solution
evalSolution = undefined


main :: IO ()
main = do
  putStrLn "hello world"