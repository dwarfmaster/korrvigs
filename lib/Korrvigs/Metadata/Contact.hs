{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Metadata.Contact where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.VCard
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.TH
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Opaleye hiding (null)

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

rSelectContact :: (MonadKorrvigs m, EntrySelector s) => s -> m (Maybe ContactData)
rSelectContact i = do
  r <- rSelectOne $ do
    entry <- selectTable entriesTable
    selectEntry i entry
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

mergeFromVCard :: (MonadKorrvigs m) => Entry -> VCardFile -> m ()
mergeFromVCard entry vcard = do
  -- Atomic update metadata
  upd <- liftIO $ do
    upd <- newIORef M.empty
    forM_ (vcard ^. vcAnniversary) $ \day -> do
      let (yr, month, d) = toGregorian day
      modifyIORef upd $ M.insert (mtdtSqlName BirthDayMtdt) $ toJSON $ BirthDay month d
      modifyIORef upd $ M.insert (mtdtSqlName BirthYear) $ toJSON yr
    forM_ (vcard ^. vcBDay) $ \day -> do
      let (yr, month, d) = toGregorian day
      modifyIORef upd $ M.insert (mtdtSqlName BirthDayMtdt) $ toJSON $ BirthDay month d
      modifyIORef upd $ M.insert (mtdtSqlName BirthYear) $ toJSON yr
    forM_ (vcard ^. vcFullName) $ \fn ->
      modifyIORef upd $ M.insert (mtdtSqlName FullName) $ toJSON fn
    forM_ (vcard ^. vcUrl) $ \url ->
      modifyIORef upd $ M.insert (mtdtSqlName Url) $ toJSON url
    readIORef upd
  updateMetadata entry upd []
  -- Merge nicknames
  unless (null $ vcard ^. vcNicknames) $ do
    nicks <- rSelectMtdt Nicknames entry
    let oldnicks = S.fromList $ fromMaybe [] nicks
    let newnicks = S.fromList $ vcard ^. vcNicknames
    let cmpnicks = S.toList $ oldnicks <> newnicks
    updateMetadata entry (M.singleton (mtdtSqlName Nicknames) (toJSON cmpnicks)) []
  -- Merge contact info
  unless (null (vcard ^. vcEmail) && M.null (vcard ^. vcTel)) $ do
    contact <- fromMaybe M.empty <$> rSelectMtdt ContactMtdt entry
    let oldmails = S.fromList $ fromMaybe [] $ M.lookup "email" contact
    let newmails = S.toList $ S.fromList (vcard ^. vcEmail) <> oldmails
    let oldtels = S.fromList $ fromMaybe [] $ M.lookup "phone" contact
    let newtels = S.toList $ S.fromList (mconcat $ M.elems $ vcard ^. vcTel) <> oldtels
    let newcontacts = M.insert "email" newmails $ M.insert "phone" newtels contact
    updateMetadata entry (M.singleton (mtdtSqlName ContactMtdt) (toJSON newcontacts)) []

mergeToVCard :: (MonadKorrvigs m) => Entry -> VCardFile -> m VCardFile
mergeToVCard entry vcard =
  rSelectContact entry >>= \case
    Nothing -> pure vcard
    Just dat -> liftIO $ do
      nvcard <- newIORef vcard
      modifyIORef nvcard $ vcFullName ?~ dat ^. contactName
      forM_ ((,) <$> dat ^. contactBirthDay <*> dat ^. contactBirthYear) $
        \(BirthDay month day, year) -> do
          modifyIORef nvcard $ vcAnniversary ?~ fromGregorian year month day
      forM_ (M.lookup "email" $ dat ^. contactContacts) $ \emails -> do
        let oldmails = S.fromList $ vcard ^. vcEmail
        let newmails = S.toList $ oldmails <> S.fromList emails
        modifyIORef nvcard $ vcEmail .~ newmails
      unless (null $ dat ^. contactNicknames) $ do
        let oldnicks = S.fromList $ dat ^. contactNicknames
        let newnicks = S.toList $ oldnicks <> S.fromList (vcard ^. vcNicknames)
        modifyIORef nvcard $ vcNicknames .~ newnicks
      forM_ (M.lookup "phone" $ dat ^. contactContacts) $ \tels -> do
        let vctels = S.fromList $ mconcat $ M.elems $ vcard ^. vcTel
        let telstoinsert = S.toList $ S.difference (S.fromList tels) vctels
        modifyIORef nvcard $ vcTel %~ M.insertWith (<>) "CELL" telstoinsert
      forM_ (dat ^. contactUrl) $ \url -> modifyIORef nvcard $ vcUrl ?~ url
      readIORef nvcard
