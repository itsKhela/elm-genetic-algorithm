module GeneticAlgorithm where

import Graphics.Element exposing (..)
import List
import Time
import Native.Randoms
import Random
import Array

----------------------------------------------------------------------------------------------------------------
-- SETTINGS ----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
chromosome_length = 4
pointToCross = 2
error_gen = 'E' -- This is for imediatly detect an error. Must be the same type of possible_gen_values
possible_gen_values = ['a','b','c','d']
chromosome_objective = 14
initial_population_number = 10
type alias Gen = -- Define your own model for type Gen
  Char -- In my case I would use a Integer value
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

initialNativeSeed = round (Native.Randoms.getFloat)

main = show (
  toString  initial_population ++ " (" ++ toString (List.sum (computeFitnessOfPopulation initial_population)) ++ ") <" ++  toString (bestChromosomeInPopulation initial_population) ++ "> "
  ++ toString  newgen ++ " -> " ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen)) ++ ") <" ++ toString (bestChromosomeInPopulation newgen) ++ "> "
  ++ toString  newgen2 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen2)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen2) ++ "> "
  ++ toString  newgen3 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen3)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen3) ++ "> "
  ++ toString  newgen4 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen4)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen4) ++ "> "
  ++ toString  newgen5 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen5)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen5) ++ "> "
  ++ toString  newgen6 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen6)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen6) ++ "> "
  ++ toString  newgen7 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen7)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen7) ++ "> "
  ++ toString  newgen8 ++ " -> "  ++ " (" ++ toString (List.sum (computeFitnessOfPopulation newgen8)) ++ ") <" ++  toString (bestChromosomeInPopulation newgen8) ++ "> "
  )

initial_population = initialization initial_population_number
newgen = newGeneration initial_population
newgen2 = newGeneration newgen
newgen3 = newGeneration newgen2
newgen4 = newGeneration newgen3
newgen5 = newGeneration newgen4
newgen6 = newGeneration newgen5
newgen7 = newGeneration newgen6
newgen8 = newGeneration newgen7
type alias Chromosome =
  List Gen -- Stores a list of genes

type alias Population =
  List Chromosome -- Stores a list of Chromosomes

type EvolutionMethod = Crossover | Mutation | None

getRandomEvolutionMethod :
  EvolutionMethod

getRandomEvolutionMethod =
  Crossover

getErrorChromosome :
  Chromosome

getErrorChromosome =
  List.foldl (\iteration acc -> error_gen :: acc) [] [0..(chromosome_length-1)]

initialization :
  Int -> -- Number of specimens in population
  Population -- resultant population

initialization numSpecimens =
  let
    population = List.foldl (\num acc -> (getRandomChromosome (num * initialNativeSeed)) :: acc ) [] [1..numSpecimens]
  in
    population

getRandomChromosome :
  Int -> -- This param is for get a effective random number
  Chromosome -- Returns a random Chromosome

getRandomChromosome preSeed =
  List.foldl (\x acc -> (
    let
      wrapped = Array.get (getEffectiveRandomInt x preSeed) (Array.fromList possible_gen_values)
      res =
        case wrapped of
          Just elem -> elem
          Maybe.Nothing -> error_gen
    in
      res
  )::acc ) [] [1..chromosome_length]

getEffectiveRandomInt x preSeed = (randomInt ( ( (x+1) + preSeed * 13  ) * preSeed * x * initialNativeSeed) 0 ((List.length possible_gen_values)-1))

randomInt seed ci cs =
  let
    rnd = Random.generate (Random.int ci cs) (Random.initialSeed seed)
    result =
    case rnd of
      (res,_) -> res
  in
    result

fitnessFunction :
  Chromosome ->
  Int

fitnessFunction chromosome =
  let
    evaluation =
      List.foldl (\index acc ->
        let
          wrappedGen = Array.get index (Array.fromList chromosome)
          gen =
            case wrappedGen of
              Just elem -> elem
              Nothing -> error_gen
          partialFitness =
            case index of
              0 ->
                case gen of
                  'a' -> 0
                  'b' -> 1
                  'c' -> 2
                  'd' -> 3
                  otherwise -> 1000
              1 ->
                case gen of
                  'a' -> 1
                  'b' -> 0
                  'c' -> 1
                  'd' -> 2
                  otherwise -> 1000
              2 ->
                case gen of
                  'a' -> 2
                  'b' -> 1
                  'c' -> 0
                  'd' -> 1
                  otherwise -> 1000
              3 ->
                case gen of
                  'a' -> 3
                  'b' -> 2
                  'c' -> 1
                  'd' -> 0
                  otherwise -> 1000
              otherwise -> 1000
        in
          partialFitness + acc
        ) 0 [0..((List.length chromosome)-1)]
  in
     evaluation

computeFitnessOfPopulation :
  Population ->
  List Int

computeFitnessOfPopulation population =
  List.foldl (\chromosome acc -> fitnessFunction chromosome :: acc) [] population

totalValueOfPopulation :
  Population ->
  Int

totalValueOfPopulation population =
  List.sum (computeFitnessOfPopulation population)

newGeneration :
  Population ->
  Population

newGeneration population =
  List.foldl (\iteration acc ->
    let
      wrappedElementFst =
        Array.get iteration (Array.fromList population)
      wrappedElementSnd =
        Array.get (iteration + (((List.length population)-1)//(2)))  (Array.fromList population)
      resFst =
        case wrappedElementFst of
          Just elem -> elem
          Maybe.Nothing -> getErrorChromosome
      resSnd =
        case wrappedElementSnd of
          Just elem -> elem
          Maybe.Nothing -> getErrorChromosome
      evolutionMethod =
        getRandomEvolutionMethod
      evolutionedFst =
        case evolutionMethod of
          Crossover -> crossover resFst resSnd
          Mutation -> mutation resFst
          None -> resFst
      evolutionedSnd=
        case evolutionMethod of
          Crossover -> crossover resSnd resFst
          Mutation -> mutation resSnd
          None -> resSnd
    in
      bestChromosome evolutionedFst resFst :: bestChromosome evolutionedSnd resSnd :: acc
  ) [] [0.. (((List.length population)-1)//2)]

crossover :
  Chromosome ->
  Chromosome ->
  Chromosome

crossover chromosome1 chromosome2 =
  let
    chromosome1Res = List.take pointToCross chromosome1 ++ List.drop pointToCross chromosome2
  in
    chromosome1Res

mutation :
  Chromosome ->
  Chromosome

mutation chromosome =
  chromosome -- TODO

bestChromosome :
  Chromosome ->
  Chromosome ->
  Chromosome

bestChromosome chromosome1 chromosome2 =
  if (fitnessFunction chromosome1) < (fitnessFunction chromosome2) then
    chromosome1
  else
    chromosome2

bestChromosomeInPopulation :
  List Chromosome ->
  Chromosome

bestChromosomeInPopulation chromosomes =
  List.foldl (\chromosome acc ->
      if acc == getErrorChromosome then
        chromosome
      else
        bestChromosome chromosome acc
    ) getErrorChromosome chromosomes


  --
