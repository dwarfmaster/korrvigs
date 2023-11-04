-- TODO autogenerate this file
module Korrvigs.Classes.Generated where

import Data.Ix (Ix)
import Data.Text (Text)

data Class
  = Entity
  | Occurrent
  | Continuant
  | SpecificallyDependentContinuant
  | IndependentContinuant
  | GenericallyDependentContinuant
  | InformationContentEntity
  | DataItem
  | OntologySpecificationItem
  | OntologyClass
  | OntologyRelation
  deriving (Show, Eq, Enum, Bounded, Ord, Ix)

name :: Class -> Text
name Entity = "Entity"
name Occurrent = "Occurrent"
name Continuant = "Continuant"
name SpecificallyDependentContinuant = "Specifically dependent continuant"
name IndependentContinuant = "Independent continuant"
name GenericallyDependentContinuant = "Generically dependent continuant"
name InformationContentEntity = "Information content entity"
name DataItem = "Data item"
name OntologySpecificationItem = "Ontology specification item"
name OntologyClass = "Ontology class"
name OntologyRelation = "Ontology relation"

parse :: Text -> Maybe Class
parse "Entity" = Just Entity
parse "Occurrent" = Just Occurrent
parse "Continuant" = Just Continuant
parse "Specifically dependent continuant" = Just SpecificallyDependentContinuant
parse "Independent continuant" = Just IndependentContinuant
parse "Generically dependent continuant" = Just GenericallyDependentContinuant
parse "Information content entity" = Just InformationContentEntity
parse "Data item" = Just DataItem
parse "Ontology specification item" = Just OntologySpecificationItem
parse "Ontology class" = Just OntologyClass
parse "Ontology relation" = Just OntologyRelation
parse _ = Nothing

-- isA Entity = Entity, but for all others it gives it immediate parent
isA :: Class -> Class
isA Entity = Entity
isA Occurrent = Entity
isA Continuant = Entity
isA SpecificallyDependentContinuant = Continuant
isA IndependentContinuant = Continuant
isA GenericallyDependentContinuant = Continuant
isA InformationContentEntity = GenericallyDependentContinuant
isA DataItem = InformationContentEntity
isA OntologySpecificationItem = DataItem
isA OntologyClass = OntologySpecificationItem
isA OntologyRelation = OntologySpecificationItem
