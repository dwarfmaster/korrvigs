{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Pandoc (writerOptions, readerOptions, displayLinks, renderUrl) where

import Data.Char (isAscii, isPrint)
import Data.Default (def)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import Korrvigs.Definition (EntityRef (..))
import Text.Pandoc (Inline (Link), Pandoc)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (ReaderOptions (..), ReferenceLocation (EndOfSection), WriterOptions (..))
import Text.Pandoc.Walk (walkM)
import Text.Parsec
import Text.Parsec.Text

writerOptions :: WriterOptions
writerOptions =
  def
    { writerExtensions = pandocExtensions,
      writerReferenceLinks = True,
      writerReferenceLocation = EndOfSection
    }

readerOptions :: ReaderOptions
readerOptions = def {readerExtensions = pandocExtensions}

-- When encountering a korr:// link, parse it into a reference, apply the
-- function to get an URL, and replace the previous link by the new one
displayLinks :: forall m. Monad m => (EntityRef -> m Text) -> Pandoc -> m Pandoc
displayLinks render = walkM doLink
  where
    doLink :: Inline -> m Inline
    doLink lk@(Link attr alt (url, title)) =
      case parseUrl url of
        Just ref -> Link attr alt . (,title) <$> render ref
        Nothing -> pure lk
    doLink inl = pure inl

renderUrl :: EntityRef -> Text
renderUrl (EntityRef uuid sub query) = "korr://" <> U.toText uuid <> subT <> queryT
  where
    subT = case sub of
      Nothing -> ""
      Just tsub -> "/" <> tsub
    queryT = case query of
      Nothing -> ""
      Just tquery -> "#" <> tquery

parseUrl :: Text -> Maybe EntityRef
parseUrl = either (const Nothing) Just . parse urlParser ""

urlParser :: Parser EntityRef
urlParser = string "korr://" >> refP
  where
    refP :: Parser EntityRef
    refP =
      EntityRef
        <$> uuidP
        <*> optionMaybe (char '/' >> elemP)
        <*> optionMaybe (char '#' >> elemP)
    uuidP :: Parser UUID
    uuidP =
      maybe (unexpected "Invalid UUID") pure . U.fromString . mconcat
        =<< (sequence . intersperse (string "-"))
          [ count 8 hexDigit,
            count 4 hexDigit,
            count 4 hexDigit,
            count 4 hexDigit,
            count 12 hexDigit
          ]
    elemP :: Parser Text
    elemP =
      fmap T.pack . many1 $
        satisfy (\c -> isAscii c && isPrint c && c /= '#' && c /= '/')
