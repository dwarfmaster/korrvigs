-- This file has been auto-generated
module Korrvigs.Classes.Generated where

import Data.Ix (Ix)
import Data.Text (Text)

data Class = Entity | ActionSpecification | Algorithm | Continuant | ContinuousTimeRegion | DataFormatSpecification | DataItem | DirectiveInformationEntity | DiscontinuousTimeRegion | File | GenericallyDependentContinuant | Identifier | ImmaterialEntity | IndependentContinuant | InformationContentEntity | Namespace | NarrativeObject | ObjectiveSpecification | Occurrent | OntologyClass | OntologyRelation | OntologySpecificationItem | PlanSpecification | Process | ProgrammingLanguage | Script | Software | SoftwareApplication | SoftwareLibrary | SoftwareModule | SpatialRegion | SpatiotemporalRegion | SpecificallyDependentContinuant | TemporalRegion | TextFile deriving (Show, Eq, Enum, Bounded, Ord, Ix)

name :: Class -> Text
name Entity = "Entity"
name ActionSpecification = "Action specification"
name Algorithm = "Algorithm"
name Continuant = "Continuant"
name ContinuousTimeRegion = "Continuous time region"
name DataFormatSpecification = "Data format specification"
name DataItem = "Data item"
name DirectiveInformationEntity = "Directive information entity"
name DiscontinuousTimeRegion = "Discontinuous time region"
name File = "File"
name GenericallyDependentContinuant = "Generically dependent continuant"
name Identifier = "Identifier"
name ImmaterialEntity = "Immaterial entity"
name IndependentContinuant = "Independent continuant"
name InformationContentEntity = "Information content entity"
name Namespace = "Namespace"
name NarrativeObject = "Narrative object"
name ObjectiveSpecification = "Objective specification"
name Occurrent = "Occurrent"
name OntologyClass = "Ontology class"
name OntologyRelation = "Ontology relation"
name OntologySpecificationItem = "Ontology specification item"
name PlanSpecification = "Plan specification"
name Process = "Process"
name ProgrammingLanguage = "Programming language"
name Script = "Script"
name Software = "Software"
name SoftwareApplication = "Software application"
name SoftwareLibrary = "Software library"
name SoftwareModule = "Software module"
name SpatialRegion = "Spatial region"
name SpatiotemporalRegion = "Spatiotemporal region"
name SpecificallyDependentContinuant = "Specifically dependent continuant"
name TemporalRegion = "Temporal region"
name TextFile = "Text file"

parse :: Text -> Maybe Class
parse "Entity" = Just Entity
parse "Action specification" = Just ActionSpecification
parse "Algorithm" = Just Algorithm
parse "Continuant" = Just Continuant
parse "Continuous time region" = Just ContinuousTimeRegion
parse "Data format specification" = Just DataFormatSpecification
parse "Data item" = Just DataItem
parse "Directive information entity" = Just DirectiveInformationEntity
parse "Discontinuous time region" = Just DiscontinuousTimeRegion
parse "File" = Just File
parse "Generically dependent continuant" = Just GenericallyDependentContinuant
parse "Identifier" = Just Identifier
parse "Immaterial entity" = Just ImmaterialEntity
parse "Independent continuant" = Just IndependentContinuant
parse "Information content entity" = Just InformationContentEntity
parse "Namespace" = Just Namespace
parse "Narrative object" = Just NarrativeObject
parse "Objective specification" = Just ObjectiveSpecification
parse "Occurrent" = Just Occurrent
parse "Ontology class" = Just OntologyClass
parse "Ontology relation" = Just OntologyRelation
parse "Ontology specification item" = Just OntologySpecificationItem
parse "Plan specification" = Just PlanSpecification
parse "Process" = Just Process
parse "Programming language" = Just ProgrammingLanguage
parse "Script" = Just Script
parse "Software" = Just Software
parse "Software application" = Just SoftwareApplication
parse "Software library" = Just SoftwareLibrary
parse "Software module" = Just SoftwareModule
parse "Spatial region" = Just SpatialRegion
parse "Spatiotemporal region" = Just SpatiotemporalRegion
parse "Specifically dependent continuant" = Just SpecificallyDependentContinuant
parse "Temporal region" = Just TemporalRegion
parse "Text file" = Just TextFile
parse _ = Nothing

isA :: Class -> Class
isA Entity = Entity
isA ActionSpecification = DirectiveInformationEntity
isA Algorithm = PlanSpecification
isA Continuant = Entity
isA ContinuousTimeRegion = TemporalRegion
isA DataFormatSpecification = DirectiveInformationEntity
isA DataItem = InformationContentEntity
isA DirectiveInformationEntity = InformationContentEntity
isA DiscontinuousTimeRegion = TemporalRegion
isA File = SpecificallyDependentContinuant
isA GenericallyDependentContinuant = Continuant
isA Identifier = InformationContentEntity
isA ImmaterialEntity = IndependentContinuant
isA IndependentContinuant = Continuant
isA InformationContentEntity = GenericallyDependentContinuant
isA Namespace = NarrativeObject
isA NarrativeObject = GenericallyDependentContinuant
isA ObjectiveSpecification = DirectiveInformationEntity
isA Occurrent = Entity
isA OntologyClass = OntologySpecificationItem
isA OntologyRelation = OntologySpecificationItem
isA OntologySpecificationItem = DataItem
isA PlanSpecification = DirectiveInformationEntity
isA Process = Occurrent
isA ProgrammingLanguage = DataFormatSpecification
isA Script = TextFile
isA Software = PlanSpecification
isA SoftwareApplication = Software
isA SoftwareLibrary = Software
isA SoftwareModule = SoftwareLibrary
isA SpatialRegion = ImmaterialEntity
isA SpatiotemporalRegion = Occurrent
isA SpecificallyDependentContinuant = Continuant
isA TemporalRegion = Occurrent
isA TextFile = File
