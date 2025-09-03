{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Metadata.Media where

import Data.Aeson
import Data.ISBN
import Data.Text (Text)
import Korrvigs.Metadata
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Metadata.TH

instance ToJSON ISBN where
  toJSON = toJSON . renderISBN

instance FromJSON ISBN where
  parseJSON = withText "ISBN" $ \txt -> case validateISBN txt of
    Left err -> fail $ show err
    Right isbn -> pure isbn

mkMtdt "MediaMtdt" "media" [t|MediaType|]
mkMtdt "Abstract" "abstract" [t|Text|]
mkMtdt "BibtexKey" "bibtex" [t|Text|]
mkMtdt "DOI" "doi" [t|[Text]|]
mkMtdt "ISBNMtdt" "isbn" [t|[ISBN]|]
mkMtdt "ISSN" "issn" [t|[Text]|]
mkMtdt "Url" "url" [t|Text|]
mkMtdt "Feed" "feed" [t|Text|]
mkMtdt "Source" "source" [t|[MediaSource]|]
mkMtdt "Journal" "journal" [t|Text|]
mkMtdt "Publisher" "publisher" [t|[Text]|]
mkMtdt "InContainer" "inbook" [t|MediaContainer|]
mkMtdt "InCollection" "incollection" [t|Text|]
mkMtdt "Institution" "institution" [t|[Text]|]
mkMtdt "License" "license" [t|[Text]|]
mkMtdt "Cover" "cover" [t|Text|]
mkMtdt "Discussions" "discussions" [t|[Text]|]
mkMtdt "TimeEstimation" "readtime" [t|Double|]
