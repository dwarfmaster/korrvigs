module Data.VCard.Parser where

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Either
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Data.Time
import Data.VCard.Defs
import Data.Word
import qualified Korrvigs.Event.ICalendar.Parser as ICal
import qualified Korrvigs.Event.ICalendar.Parser.Basic as ICB
import qualified Korrvigs.Event.ICalendar.Parser.Types as ICT
import Text.Parsec

parseVCard :: Maybe FilePath -> BSL.ByteString -> IO (Either Text VCardFile)
parseVCard path content = do
  let r = runParserT parser () (fromMaybe "<vcf>" path) $ BSL.unpack content
  case runIdentity r of
    Left err ->
      pure . Left . T.pack $ "\"" <> show err <> "\" at " <> show (errorPos err)
    Right v -> pure $ Right v
  where
    parser :: ParsecT [Word8] () Identity VCardFile
    parser = vcardFileP <* eof

paramP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, Text)
paramP = do
  name <- ICal.paramNameP
  ICB.charP '='
  value <- ICal.paramValueP
  pure (name, value)

textCharSpecP :: (Monad m, Stream s m Word8) => ICB.CharSpec -> ParsecT s u m Text
textCharSpecP spec = toText <$> ICB.runCharSpecP spec
  where
    toText = LT.toStrict . LEnc.decodeASCII . BSL.pack

contentLineP ::
  (Monad m, Stream s m Word8) =>
  (Text -> Maybe (ParsecT s u m (Map Text Text -> a))) ->
  ParsecT s u m (Text, Either VCardAbstractValue a)
contentLineP parsers = do
  name <- ICal.nameP
  params <- many $ ICB.charP ';' >> paramP
  ICB.charP ':'
  let parmp = M.fromList params
  val <- case parsers name of
    Just parser -> parser <*> pure parmp <&> Right
    Nothing -> Left . VCAbstractValue parmp <$> sepBy1 ICal.valueP (ICB.charP ';')
  ICal.crlfP <|> eof
  pure (name, val)

lineDefP :: (Monad m, Stream s m Word8) => ParsecT s u m (Text, VCardAbstractValue)
lineDefP = do
  (name, val) <- contentLineP (const Nothing)
  pure (name, fromLeft (VCAbstractValue M.empty []) val)

vcardFileP :: (Monad m, Stream s m Word8) => ParsecT s u m VCardFile
vcardFileP = do
  (name, val) <- lineDefP
  unless (name == "BEGIN" && val ^. vcValValue == ["VCARD"]) mzero
  let start = VCFile "" "" def def def def def def def def
  vcardFileRecP start

vcardFileRecP :: (Monad m, Stream s m Word8) => VCardFile -> ParsecT s u m VCardFile
vcardFileRecP vcard = do
  (name, val) <- contentLineP lineSpec
  case val of
    Right f -> vcardFileRecP $ f vcard
    Left _ | name == "END" -> pure vcard
    Left abstract ->
      vcardFileRecP $ vcard & vcContent %~ M.insertWith (<>) name [abstract]
  where
    lineSpec :: (Monad m, Stream s m Word8) => Text -> Maybe (ParsecT s u m (Map Text Text -> VCardFile -> VCardFile))
    lineSpec key =
      M.lookup key $
        M.fromList
          [ ("VERSION", (\txt _ -> vcVersion .~ txt) <$> ICT.textP),
            ("UID", (\txt _ -> vcUID .~ txt) <$> ICT.textP),
            ("ANNIVERSARY", (\day _ -> vcAnniversary ?~ day) <$> dayP),
            ("BDAY", (\day _ -> vcBDay ?~ day) <$> dayP),
            ("EMAIL", (\email _ -> vcEmail %~ (email :)) <$> ICT.textP),
            ("FN", (\txt _ -> vcFullName ?~ txt) <$> ICT.textP),
            ("NICKNAME", (\txts _ -> vcNicknames %~ (txts ++)) <$> sepBy1 ICT.textP (ICB.charP ',')),
            ("TEL", addTel <$> telP),
            ("URL", (\txt _ -> vcUrl ?~ txt) <$> ICT.textP)
          ]

dayP :: (Monad m, Stream s m Word8) => ParsecT s u m Day
dayP = do
  year <- ICB.digitsP 4
  month <- ICB.digitsP 2
  day <- ICB.digitsP 2
  pure $ fromGregorian year month day

addTel :: Text -> Map Text Text -> VCardFile -> VCardFile
addTel tel params = case M.lookup "TYPE" params of
  Nothing -> id
  Just tp -> vcTel %~ M.insertWith (<>) tp [tel]

telP :: (Monad m, Stream s m Word8) => ParsecT s u m Text
telP = optional (try $ ICB.stringP "cell:") >> ICT.textP
