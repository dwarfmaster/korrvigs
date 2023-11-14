-- This file has been auto-generated
module Korrvigs.Classes.Generated where

import Data.Ix (Ix)
import Data.Text (Text)

data Class = Entity | Continuant | ContinuousTimeRegion | DataFormatSpecification | DataItem | DirectiveInformationEntity | DiscontinuousTimeRegion | File | GenericallyDependentContinuant | Identifier | IndependentContinuant | InformationContentEntity | Namespace | NarrativeObject | Occurrent | OntologyClass | OntologyRelation | OntologySpecificationItem | Script | SpecificallyDependentContinuant | TemporalRegion | TextFile deriving (Show, Eq, Enum, Bounded, Ord, Ix)

name :: Class -> Text
name Entity = "Entity"
name Continuant = "Continuant"
name ContinuousTimeRegion = "Continuous time region"
name DataFormatSpecification = "Data format specification"
name DataItem = "Data item"
name DirectiveInformationEntity = "Directive information entity"
name DiscontinuousTimeRegion = "Discontinuous time region"
name File = "File"
name GenericallyDependentContinuant = "Generically dependent continuant"
name Identifier = "Identifier"
name IndependentContinuant = "Independent continuant"
name InformationContentEntity = "Information content entity"
name Namespace = "Namespace"
name NarrativeObject = "Narrative object"
name Occurrent = "Occurrent"
name OntologyClass = "Ontology class"
name OntologyRelation = "Ontology relation"
name OntologySpecificationItem = "Ontology specification item"
name Script = "Script"
name SpecificallyDependentContinuant = "Specifically dependent continuant"
name TemporalRegion = "Temporal region"
name TextFile = "Text file"

parse :: Text -> Maybe Class
parse "Entity" = Just Entity
parse "Continuant" = Just Continuant
parse "Continuous time region" = Just ContinuousTimeRegion
parse "Data format specification" = Just DataFormatSpecification
parse "Data item" = Just DataItem
parse "Directive information entity" = Just DirectiveInformationEntity
parse "Discontinuous time region" = Just DiscontinuousTimeRegion
parse "File" = Just File
parse "Generically dependent continuant" = Just GenericallyDependentContinuant
parse "Identifier" = Just Identifier
parse "Independent continuant" = Just IndependentContinuant
parse "Information content entity" = Just InformationContentEntity
parse "Namespace" = Just Namespace
parse "Narrative object" = Just NarrativeObject
parse "Occurrent" = Just Occurrent
parse "Ontology class" = Just OntologyClass
parse "Ontology relation" = Just OntologyRelation
parse "Ontology specification item" = Just OntologySpecificationItem
parse "Script" = Just Script
parse "Specifically dependent continuant" = Just SpecificallyDependentContinuant
parse "Temporal region" = Just TemporalRegion
parse "Text file" = Just TextFile
parse _ = Nothing

isA :: Class -> Class
isA Entity = Entity
isA Continuant = Entity
isA ContinuousTimeRegion = TemporalRegion
isA DataFormatSpecification = DirectiveInformationEntity
isA DataItem = InformationContentEntity
isA DirectiveInformationEntity = InformationContentEntity
isA DiscontinuousTimeRegion = TemporalRegion
isA File = SpecificallyDependentContinuant
isA GenericallyDependentContinuant = Continuant
isA Identifier = InformationContentEntity
isA IndependentContinuant = Continuant
isA InformationContentEntity = GenericallyDependentContinuant
isA Namespace = NarrativeObject
isA NarrativeObject = GenericallyDependentContinuant
isA Occurrent = Entity
isA OntologyClass = OntologySpecificationItem
isA OntologyRelation = OntologySpecificationItem
isA OntologySpecificationItem = DataItem
isA Script = TextFile
isA SpecificallyDependentContinuant = Continuant
isA TemporalRegion = Occurrent
isA TextFile = File
