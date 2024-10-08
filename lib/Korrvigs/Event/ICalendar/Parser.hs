module Korrvigs.Event.ICalendar.Parser where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Map
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime
import Data.Word
import Korrvigs.Event.ICalendar.Defs
import Korrvigs.Event.ICalendar.Parser.Basic
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
paramValueP = paramTextP <|> quotedStringP

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

contentLineP :: (Monad m, Stream s m Word8) => Map Text (ParsecT s u m a) -> (Text -> a) -> ParsecT s u m (Text, ICalValue a)
contentLineP parsers defCase = do
  name <- nameP
  params <- many $ charP ';' >> paramP
  charP ':'
  val <- case M.lookup name parsers of
    Just parser -> parser
    Nothing -> defCase <$> valueP
  crlfP
  pure (name, ICValue (M.fromList params) val)

contentLineDefP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, ICalValue Text)
contentLineDefP = contentLineP M.empty id

revAGroup :: ICalAbstractGroup -> ICalAbstractGroup
revAGroup agroup =
  agroup
    & icValues . each %~ reverse
    & icGroups . each %~ reverse

icalFileP :: (Monad m, Stream s m Word8) => ParsecT s u m ICalFile
icalFileP = do
  (name, val) <- contentLineDefP
  unless (name == "BEGIN" && val ^. icValue == "VCALENDAR") mzero
  let start = ICFile "" def def
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
    "BEGIN" -> do
      let gname = val ^. icValue
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
    _ -> case val ^. icValue of
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
  pure undefined
