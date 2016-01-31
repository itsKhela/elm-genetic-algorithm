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
error_gen = 'E' -- This is for imediatly detect an error. Must be the same type of possible_gen_values
possible_gen_values = ['a','b','c','d']
chromosome_objective = 14
initial_population_number = 4
type alias Gen = -- Define your own model for type Gen
  Char -- In my case I would use a Integer value
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

initialNativeSeed = round (Native.Randoms.getFloat)

main = show ( toString (initial_population) ++ "\n" ++ toString (newGeneration initial_population) )

initial_population = initialization initial_population_number

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
      List.foldl (\gen acc ->
        case gen of
          'a' -> 1 + acc
          'b' -> 2 + acc
          'c' -> 3 + acc
          'd' -> 4 + acc
          otherwise -> 0 + acc
        ) 0 chromosome
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
          Crossover -> crossover resFst resSnd 2
          Mutation -> mutation resFst
          None -> resFst
      evolutionedSnd=
        case evolutionMethod of
          Crossover -> crossover resSnd resFst 2
          Mutation -> mutation resSnd
          None -> resSnd
    in
      evolutionedFst :: evolutionedSnd :: acc
  ) [] [0.. (((List.length population)-1)//2)]


crossover :
  Chromosome ->
  Chromosome ->
  Int ->
  Chromosome

crossover chromosome1 chromosome2 pointToCross =
  let
    chromosome1Res = List.take pointToCross chromosome1 ++ List.drop ((List.length chromosome2) - pointToCross) chromosome2
  in
    chromosome1Res

mutation :
  Chromosome ->
  Chromosome

mutation chromosome =
  chromosome -- TODO





  --
