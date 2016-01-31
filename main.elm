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
chromosome_length = 10
error_element = 'E' -- This is for imediatly detect an error. Must be the same type of possible_gen_values
possible_gen_values = ['a','b','c','d','2','M']
initial_population_number = 100
type alias Gen = -- Define your own model for type Gen
  Char -- In my case I would use a Integer value
----------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

initialNativeSeed = round (Native.Randoms.getFloat)

main = show (initialization initial_population_number)

type alias Chromosome =
  List Gen -- Stores a list of genes

type alias Population =
  List Chromosome -- Stores a list of Chromosomes

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
          Maybe.Nothing -> error_element
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



  --
