{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Metadata.Media where

import Control.Lens
import Data.Aeson
import Data.CaseInsensitive (CI)
import Data.ISBN
import Data.List (singleton)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.Calendar
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Metadata.TH
import Korrvigs.Monad

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
mkMtdt "MedMonth" "month" [t|MonthOfYear|]
mkMtdt "MedYear" "year" [t|Year|]
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

rSelectMedia :: (MonadKorrvigs m) => Id -> m (Maybe Media)
rSelectMedia i =
  rSelectMtdt MediaMtdt (sqlId i) >>= \case
    Nothing -> pure Nothing
    Just mtype ->
      Just
        <$> ( Media mtype
                <$> rSelectMtdt BibtexKey (sqlId i)
                <*> rSelectMtdt Abstract (sqlId i)
                <*> rSelectListMtdt DOI (sqlId i)
                <*> rSelectListMtdt ISBNMtdt (sqlId i)
                <*> rSelectListMtdt ISSN (sqlId i)
                <*> rSelectMtdt Title (sqlId i)
                <*> rSelectListMtdt Authors (sqlId i)
                <*> rSelectMtdt MedMonth (sqlId i)
                <*> rSelectMtdt MedYear (sqlId i)
                <*> rSelectMtdt Url (sqlId i)
                <*> rSelectMtdt Feed (sqlId i)
                <*> rSelectListMtdt Source (sqlId i)
                <*> rSelectListMtdt Publisher (sqlId i)
                <*> rSelectMtdt InContainer (sqlId i)
                <*> rSelectListMtdt Institution (sqlId i)
                <*> rSelectListMtdt License (sqlId i)
                <*> (fmap MkId <$> rSelectMtdt Cover (sqlId i))
                <*> rSelectListMtdt Discussions (sqlId i)
            )

mediaMetadata :: Media -> Map (CI Text) Value
mediaMetadata med =
  M.fromList $
    mconcat
      [ [(mtdtName MediaMtdt, toJSON $ med ^. medType)],
        medRow Abstract medAbstract,
        medRow BibtexKey medBibtex,
        medLstRow DOI medDOI,
        medLstRow ISBNMtdt medISBN,
        medLstRow ISSN medISSN,
        medRow Title medTitle,
        medLstRow Authors medAuthors,
        medRow MedMonth medMonth,
        medRow MedYear medYear,
        medRow Url medUrl,
        medRow Feed medRSS,
        medLstRow Source medSource,
        medLstRow Publisher medPublisher,
        medRow InContainer medContainer,
        medLstRow Institution medInstitution,
        medLstRow License medLicense,
        medRow Cover (medCover . to (fmap unId)),
        medLstRow Discussions medDiscussion
      ]
  where
    medRow :: (ExtraMetadata mtdt, ToJSON a, MtdtType mtdt ~ a) => mtdt -> Getting (Maybe a) Media (Maybe a) -> [(CI Text, Value)]
    medRow mtdt get = maybe [] (singleton . (mtdtName mtdt,) . toJSON) $ med ^. get
    medLstRow :: (ExtraMetadata mtdt, ToJSON a, MtdtType mtdt ~ [a]) => mtdt -> Getting [a] Media [a] -> [(CI Text, Value)]
    medLstRow mtdt get = case med ^. get of
      [] -> []
      val -> [(mtdtName mtdt, toJSON val)]
