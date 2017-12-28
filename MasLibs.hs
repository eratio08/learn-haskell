module MadLibs where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Exclamation = String
type Noun = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he sais " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = 
  mconcat [e, "! he sais ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife"]
