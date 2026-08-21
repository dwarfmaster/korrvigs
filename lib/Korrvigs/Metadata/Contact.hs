{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Metadata.Contact where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.TH
import Korrvigs.Monad
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye

data BirthDay = BirthDay MonthOfYear DayOfMonth
  deriving (Eq, Ord, Show)

renderBirthday :: BirthDay -> Text
renderBirthday (BirthDay month day) =
  T.pack $ formatTime defaultTimeLocale "%m-%d" $ fromGregorian 0 month day

instance ToJSON BirthDay where
  toJSON bday = String $ renderBirthday bday

instance FromJSON BirthDay where
  parseJSON = withText "BirthDay" $ \txt ->
    case parseTimeM True defaultTimeLocale "%m-%d" $ T.unpack txt of
      Nothing -> undefined
      Just date -> let (_, month, day) = toGregorian date in pure $ BirthDay month day

mkMtdt "FullName" "fullname" [t|Text|]
mkMtdt "BirthDayMtdt" "birthday" [t|BirthDay|]
mkMtdt "BirthYear" "birthyear" [t|Year|]
mkMtdt "Death" "death" [t|Day|]
mkMtdt "ContactMtdt" "contact" [t|Map Text [Text]|]
mkMtdt "Gender" "gender" [t|Text|]
mkMtdt "Pronouns" "pronouns" [t|Map Text Text|]
mkMtdt "Nicknames" "nicknames" [t|[Text]|]

data ContactDataImpl a b c d e f g h i j = ContactData
  { _contactName :: a,
    _contactBirthDay :: b,
    _contactBirthYear :: c,
    _contactDeath :: d,
    _contactContacts :: e,
    _contactGender :: f,
    _contactPronouns :: g,
    _contactNicknames :: h,
    _contactPicture :: i,
    _contactUrl :: j
  }
  deriving (Eq, Ord, Show)

type ContactData =
  ContactDataImpl
    Text
    (Maybe BirthDay)
    (Maybe Year)
    (Maybe Day)
    (Map Text [Text])
    (Maybe Text)
    (Map Text Text)
    [Text]
    (Maybe Id)
    (Maybe Text)

type ContactDataRow =
  ContactDataImpl
    Text
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)
    (Maybe Value)

type ContactDataSQL =
  ContactDataImpl
    (Field SqlText)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)
    (FieldNullable SqlJsonb)

makeLenses ''ContactDataImpl
makeAdaptorAndInstanceInferrable "pContactData" ''ContactDataImpl

selectContactData :: EntryRowSQLR -> Select ContactDataSQL
selectContactData entry =
  ContactData
    <$> selectName
    <*> selectMtdt BirthDayMtdt sqlI
    <*> selectMtdt BirthYear sqlI
    <*> selectMtdt Death sqlI
    <*> selectMtdt ContactMtdt sqlI
    <*> selectMtdt Gender sqlI
    <*> selectMtdt Pronouns sqlI
    <*> selectMtdt Nicknames sqlI
    <*> selectMtdt Cover sqlI
    <*> selectMtdt Url sqlI
  where
    sqlI = entry ^. sqlEntryId
    selectName = do
      title <- fromNullableSelect $ pure $ entry ^. sqlEntryTitle
      name <- fromNullable (sqlTextToJson title) <$> selectMtdt FullName sqlI
      fromNullableSelect $ pure $ sqlJsonToText $ toNullable name

reifyContactData :: ContactDataRow -> ContactData
reifyContactData row =
  ContactData
    { _contactName = row ^. contactName,
      _contactBirthDay = fromJSONM =<< row ^. contactBirthDay,
      _contactBirthYear = fromJSONM =<< row ^. contactBirthYear,
      _contactDeath = fromJSONM =<< row ^. contactDeath,
      _contactContacts = fromMaybe M.empty $ fromJSONM =<< row ^. contactContacts,
      _contactGender = fromJSONM =<< row ^. contactGender,
      _contactPronouns = fromMaybe M.empty $ fromJSONM =<< row ^. contactPronouns,
      _contactNicknames = fromMaybe [] $ fromJSONM =<< row ^. contactNicknames,
      _contactPicture = fromJSONM =<< row ^. contactPicture,
      _contactUrl = fromJSONM =<< row ^. contactUrl
    }

rSelectContact :: (MonadKorrvigs m) => Id -> m (Maybe ContactData)
rSelectContact i = do
  r <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    selectContactData entry
  pure $ case r of
    Nothing -> Nothing
    Just (dat :: ContactDataRow) -> Just $ reifyContactData dat

computeAge :: (MonadIO m) => Year -> Maybe BirthDay -> m (Bool, Integer)
computeAge yr mbday = computeAgeAt yr mbday . utctDay <$> liftIO getCurrentTime

computeAgeAt :: Year -> Maybe BirthDay -> Day -> (Bool, Integer)
computeAgeAt yr mbday currentDay = do
  case mbday of
    Nothing -> (False, diffYear)
    Just (BirthDay mth dy) ->
      (True,) $ if curMonth < mth || (curMonth == mth && curDay < dy) then diffYear - 1 else diffYear
  where
    (curYear, curMonth, curDay) = toGregorian currentDay
    diffYear = curYear - yr
