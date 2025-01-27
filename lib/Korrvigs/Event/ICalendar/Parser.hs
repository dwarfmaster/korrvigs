module Korrvigs.Event.ICalendar.Parser where

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Aeson (Value)
import Data.Aeson.Decoding (decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Either
import Data.Functor
import Data.Map
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime
import Data.Word
import Korrvigs.Entry (Id (..))
import Korrvigs.Event.ICalendar.Defs
import Korrvigs.Event.ICalendar.Parser.Basic
import Korrvigs.Event.ICalendar.Parser.Types
import Korrvigs.Geometry
import Korrvigs.Geometry.WKB (readGeometry)
import Linear.V2
import Text.Parsec

parseICalFile :: FilePath -> IO (Either Text ICalFile)
parseICalFile path = do
  content <- BSL.readFile path
  let r = runParserT parser () path $ BSL.unpack content
  case runIdentity r of
    Left err ->
      pure . Left . T.pack $ "\"" <> show err <> "\" at " <> show (errorPos err)
    Right v -> pure $ Right v
  where
    parser :: ParsecT [Word8] () Identity ICalFile
    parser = icalFileP <* eof

-- Follows RFC5545:
--   https://datatracker.ietf.org/doc/html/rfc5545

quotedStringP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
quotedStringP = do
  charP '"'
  chars <- many $ charSpecP qSafeChar
  charP '"'
  pure $ T.pack chars

valueP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
valueP = T.pack <$> many (charSpecP valueChar)

paramTextP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
paramTextP = T.pack <$> many (charSpecP safeChar)

paramValueP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
paramValueP = quotedStringP <|> paramTextP

paramNameP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
paramNameP = T.pack <$> many1 (charSpecP nameChar)

paramP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, [Text])
paramP = do
  name <- paramNameP
  charP '='
  values <- sepBy1 paramValueP (charP ',')
  pure (name, values)

nameP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
nameP = paramNameP

crlfP :: (Monad m, Stream s m Word8) => ParsecT s u m ()
crlfP = try (byteSeqP [0x0D, 0x0A]) <|> byteSeqP [0x0A]

contentLineP ::
  (Monad m, Stream s m Word8) =>
  (Text -> Maybe (ParsecT s u m (Map Text [Text] -> a))) ->
  ParsecT s u m (Text, Either (ICalValue Text) a)
contentLineP parsers = do
  name <- nameP
  params <- many $ charP ';' >> paramP
  charP ':'
  let parmp = M.fromList params
  val <- case parsers name of
    Just parser -> parser <*> pure parmp <&> Right
    Nothing -> Left . ICValue parmp <$> valueP
  crlfP <|> eof
  pure (name, val)

contentLineDefP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, ICalValue Text)
contentLineDefP = do
  (name, val) <- contentLineP (const Nothing)
  pure (name, fromLeft (ICValue M.empty "") val)

revAGroup :: ICalAbstractGroup -> ICalAbstractGroup
revAGroup agroup =
  agroup
    & icValues . each %~ reverse
    & icGroups . each %~ reverse

icalFileP :: (Monad m, Stream s m Word8) => ParsecT s u m ICalFile
icalFileP = do
  (name, val) <- contentLineDefP
  unless (name == "BEGIN" && val ^. icValue == "VCALENDAR") mzero
  let start = ICFile "" def def def
  ical <- icalFileRecP start
  pure $ ical & icContent %~ revAGroup

onML :: ([a] -> [a]) -> Maybe [a] -> Maybe [a]
onML f Nothing = Just $ f []
onML f (Just l) = Just $ f l

mlPush :: a -> Maybe [a] -> Maybe [a]
mlPush v = onML (v :)

icalFileRecP :: (Monad m, Stream s m Word8) => ICalFile -> ParsecT s u m ICalFile
icalFileRecP ical = do
  (name, val) <- contentLineDefP
  case name of
    "BEGIN" ->
      let gname = val ^. icValue
       in case gname of
            "VTIMEZONE" -> do
              tz <- icalTZP
              icalFileRecP $ ical & icTimezones . at (tz ^. ictzId) ?~ tz
            "VEVENT" -> do
              ev <- icalEventP
              icalFileRecP $ ical & icEvent ?~ ev
            _ -> do
              group <- icalAbstractGroupP gname
              icalFileRecP $ ical & icContent . icGroups . at gname %~ mlPush group
    "END" -> do
      unless (val ^. icValue == "VCALENDAR") mzero
      pure ical
    "VERSION" -> icalFileRecP (ical & icVersion .~ val ^. icValue)
    _ -> icalFileRecP (ical & icContent . icValues . at name %~ mlPush val)

icalAbstractGroupP :: (Monad m, Stream s m Word8) => Text -> ParsecT s u m ICalAbstractGroup
icalAbstractGroupP tp = revAGroup <$> icalAbstractGroupRecP tp def

icalAbstractGroupRecP :: (Monad m, Stream s m Word8) => Text -> ICalAbstractGroup -> ParsecT s u m ICalAbstractGroup
icalAbstractGroupRecP tp icgroup = do
  (name, val) <- contentLineDefP
  case name of
    "BEGIN" -> do
      let gname = val ^. icValue
      group <- icalAbstractGroupP gname
      icalAbstractGroupRecP tp $ icgroup & icGroups . at gname %~ mlPush group
    "END" -> do
      unless (val ^. icValue == tp) mzero
      pure icgroup
    _ -> icalAbstractGroupRecP tp $ icgroup & icValues . at name %~ mlPush val

-- Timezones
icalTZP :: (Monad m, Stream s m Word8) => ParsecT s u m ICalTimeZone
icalTZP = do
  tz <- icalTZRecP $ ICTZ "" [] def
  pure $
    tz
      & ictzSpecs %~ reverse
      & ictzTopLevel %~ revAGroup

icalTZRecP :: (Monad m, Stream s m Word8) => ICalTimeZone -> ParsecT s u m ICalTimeZone
icalTZRecP tz = do
  (name, val) <- contentLineDefP
  case name of
    "BEGIN" -> case val ^. icValue of
      "STANDARD" -> icalTZSpecP True >>= \spec -> icalTZRecP $ tz & ictzSpecs %~ (spec :)
      "DAYLIGHT" -> icalTZSpecP False >>= \spec -> icalTZRecP $ tz & ictzSpecs %~ (spec :)
      _ -> do
        let gname = val ^. icValue
        group <- icalAbstractGroupP gname
        icalTZRecP $ tz & ictzTopLevel . icGroups . at gname %~ mlPush group
    "END" -> do
      unless (val ^. icValue == "VTIMEZONE") mzero
      pure tz
    _ -> case name of
      "TZID" -> icalTZRecP $ tz & ictzId .~ val ^. icValue
      _ -> icalTZRecP $ tz & ictzTopLevel . icValues . at name %~ mlPush val

defDay :: LocalTime
defDay = LocalTime (fromOrdinalDate 1971 1) midnight

icalTZSpecP :: (Monad m, Stream s m Word8) => Bool -> ParsecT s u m ICalTZSpec
icalTZSpecP isStandard = do
  tzs <- icalTZSpecRecP $ ICTZSpec isStandard defDay 0 0 Nothing Nothing Nothing def
  pure $ tzs & ictzContent %~ revAGroup

icalTZSpecRecP :: (Monad m, Stream s m Word8) => ICalTZSpec -> ParsecT s u m ICalTZSpec
icalTZSpecRecP tzs = do
  (name, val) <- contentLineP lineSpec
  case val of
    Right spec -> icalTZSpecRecP $ spec tzs
    Left v -> case name of
      "BEGIN" -> do
        let gname = v ^. icValue
        group <- icalAbstractGroupP gname
        icalTZSpecRecP $ tzs & ictzContent . icGroups . at gname %~ mlPush group
      "END" -> pure tzs
      _ -> icalTZSpecRecP $ tzs & ictzContent . icValues . at name %~ mlPush v
  where
    lineSpec :: (Monad m, Stream s m Word8) => Text -> Maybe (ParsecT s u m (Map Text [Text] -> ICalTZSpec -> ICalTZSpec))
    lineSpec key =
      M.lookup key $
        M.fromList $
          fmap
            (second (const <$>))
            [ ("DTSTART", (ictzStart .~) . snd <$> dateTimeP),
              ("TZOFFSETTO", (ictzOffsetTo .~) <$> utcOffsetP),
              ("TZOFFSETFROM", (ictzOffsetFrom .~) <$> utcOffsetP),
              ("RDATE", (ictzRdate ?~) . snd <$> dateTimeP),
              ("TZNAME", (ictzName ?~) <$> textP),
              ("RRULE", (ictzRRule ?~) <$> rruleP)
            ]

-- Events
icalEventP :: (Monad m, Stream s m Word8) => ParsecT s u m ICalEvent
icalEventP = icalEventRecP $ ICEvent "" [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing False Nothing [] Nothing M.empty def

icalEventRecP :: forall s u m. (Monad m, Stream s m Word8) => ICalEvent -> ParsecT s u m ICalEvent
icalEventRecP ev = do
  (name, val) <- contentLineP spec
  case val of
    Right spc -> icalEventRecP $ spc ev
    Left v -> case name of
      "BEGIN" -> do
        let gname = v ^. icValue
        group <- icalAbstractGroupP gname
        icalEventRecP $ ev & iceContent . icGroups . at gname %~ mlPush group
      "END" -> pure ev
      _ -> icalEventRecP $ ev & iceContent . icValues . at name %~ mlPush v
  where
    spec :: Text -> Maybe (ParsecT s u m (Map Text [Text] -> ICalEvent -> ICalEvent))
    spec key = mtdtSpec key >> lineSpec key
    lineSpec :: Text -> Maybe (ParsecT s u m (Map Text [Text] -> ICalEvent -> ICalEvent))
    lineSpec key =
      M.lookup key $
        M.fromList
          [ ("UID", const . (iceUid .~) <$> valueP),
            ("CATEGORIES", const . (iceCategories .~) <$> listOfP textP),
            ("COMMENT", const . (iceComment ?~) <$> textP),
            ("SUMMARY", const . (iceSummary ?~) <$> textP),
            ("DESCRIPTION", const . (iceDescription ?~) <$> textP),
            ("GEO", const . (iceGeometry ?~) <$> (GeoPoint . uncurry V2 <$> geoP)),
            ("X-KORRVIGS-GEOM", const . (iceGeometry ?~) <$> korrGeomP),
            ("X-KORRVIGS-PARENTS", const . (iceParents .~) <$> listOfP (MkId <$> textP)),
            ("X-KORRVIGS-NAME", const . (iceId ?~) <$> (MkId <$> textP)),
            ("LOCATION", const . (iceLocation ?~) <$> textP),
            ("DTSTART", mkTimeSpec (iceStart ?~) <$> dateMTimeP),
            ("DTEND", mkTimeSpec (iceEnd ?~) <$> dateMTimeP),
            ("DURATION", const . (iceDuration ?~) <$> durationP),
            ("TRANSP", const . (iceTransparent .~) <$> transpP)
          ]
    mtdtSpec :: Text -> Maybe (ParsecT s u m (Map Text [Text] -> ICalEvent -> ICalEvent))
    mtdtSpec key = case T.stripPrefix "X-KORRMTDT-" key of
      Nothing -> Nothing
      Just nm -> Just $ do
        js <- textP
        let v :: Maybe Value = decode $ LEnc.encodeUtf8 $ LT.fromStrict js
        case v of
          Nothing -> fail $ "Failed to parse json for " <> T.unpack key
          Just mtdt -> pure . const $ iceMtdt . at nm ?~ mtdt

geoP :: (Monad m, Stream s m Word8) => ParsecT s u m (Double, Double)
geoP = (,) <$> floatP <*> (charP ';' >> floatP)

korrGeomP :: (Monad m, Stream s m Word8) => ParsecT s u m Geometry
korrGeomP = readGeometry . LEnc.encodeUtf8 . LT.fromStrict <$> textP

transpP :: (Monad m, Stream s m Word8) => ParsecT s u m Bool
transpP = (stringP "TRANSPARENT" $> True) <|> (stringP "OPAQUE" $> False)

mkTimeSpec :: (ICalTimeSpec -> ICalEvent -> ICalEvent) -> (Bool, LocalTime) -> Map Text [Text] -> ICalEvent -> ICalEvent
mkTimeSpec bld (isUTC, time) params = bld spec
  where
    spec :: ICalTimeSpec
    spec =
      ICTmSpec
        { _ictmDate = time,
          _ictmUTC = isUTC,
          _ictmTimeZone = case M.lookup "TZID" params of
            Just (tz : _) -> Just tz
            _ -> Nothing
        }
