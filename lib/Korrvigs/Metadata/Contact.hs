module Korrvigs.Metadata.Contact where

import Control.Lens
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
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

instance ToJSON BirthDay where
  toJSON (BirthDay month day) =
    String $ T.pack $ formatTime defaultTimeLocale "%m-%d" $ fromGregorian 0 month day

instance FromJSON BirthDay where
  parseJSON = withText "BirthDay" $ \txt ->
    case parseTimeM True defaultTimeLocale "%m-%d" $ T.unpack txt of
      Nothing -> undefined
      Just date -> let (_, month, day) = toGregorian date in pure $ BirthDay month day

mkMtdt "FullName" "fullname" [t|Text|]
mkMtdt "BirthDayMtdt" "birthday" [t|BirthDay|]
mkMtdt "BirthYear" "birthyear" [t|Year|]
mkMtdt "Death" "death" [t|Day|]
mkMtdt "ContactMtdt" "contact" [t|Map Text Value|]
mkMtdt "Gender" "gender" [t|Text|]
mkMtdt "Pronouns" "pronouns" [t|Map Text Text|]
mkMtdt "Nicknames" "nicknames" [t|[Text]|]

data ContactData = ContactData
  { _contactName :: Text,
    _contactBirthDay :: Maybe BirthDay,
    _contactBirthYear :: Maybe Year,
    _contactDeath :: Maybe Day,
    _contactContacts :: Map Text Value,
    _contactGender :: Maybe Text,
    _contactPronouns :: Map Text Text,
    _contactNicknames :: [Text],
    _contactPicture :: Maybe Id,
    _contactUrl :: Maybe Text
  }
  deriving (Eq, Ord, Show)

makeLenses ''ContactData

rSelectContact :: (MonadKorrvigs m) => Id -> m (Maybe ContactData)
rSelectContact i = do
  r <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    let sqlI = entry ^. sqlEntryId
    title <- fromNullableSelect $ pure $ sqlCast $ entry ^. sqlEntryTitle
    name <- fromNullable title <$> selectMtdt FullName sqlI
    birthday <- selectMtdt BirthDayMtdt sqlI
    birthyear <- selectMtdt BirthYear sqlI
    death <- selectMtdt Death sqlI
    contacts <- selectMtdt ContactMtdt sqlI
    gender <- selectMtdt Gender sqlI
    pronouns <- selectMtdt Pronouns sqlI
    nicks <- selectMtdt Nicknames sqlI
    picture <- selectMtdt Cover sqlI
    url <- selectMtdt Url sqlI
    pure (name, birthday, birthyear, death, contacts, gender, pronouns, nicks, picture, url)
  case r of
    Nothing -> pure Nothing
    Just (name, birthday, birthyear, death, contacts, gender, pronouns, nicks, picture, url) -> do
      pure $
        Just $
          ContactData
            { _contactName = name,
              _contactBirthDay = fromJSONM =<< birthday,
              _contactBirthYear = fromJSONM =<< birthyear,
              _contactDeath = fromJSONM =<< death,
              _contactContacts = fromMaybe M.empty $ fromJSONM =<< contacts,
              _contactGender = fromJSONM =<< gender,
              _contactPronouns = fromMaybe M.empty $ fromJSONM =<< pronouns,
              _contactNicknames = fromMaybe [] $ fromJSONM =<< nicks,
              _contactPicture = fromJSONM =<< picture,
              _contactUrl = fromJSONM =<< url
            }
