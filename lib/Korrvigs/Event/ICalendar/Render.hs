module Korrvigs.Event.ICalendar.Render where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as B8
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Event.ICalendar.Defs

renderICalFile :: ICalFile -> BSL.ByteString
renderICalFile = toLazyByteString . buildICalFile

buildICalFile :: ICalFile -> Builder
buildICalFile = execWriter . flip evalStateT 0 . bldFile

type RenderM = StateT Int (Writer Builder)

bldChar :: Char -> RenderM ()
bldChar c = do
  pos <- get
  let utf8 = B8.fromChar c
  let l = BS.length utf8
  when (pos + l > 75) $ do
    tell $ word8 0x0A
    tell $ word8 0x20
    put 1
  tell $ byteString utf8
  modify (+ l)

bldNewline :: RenderM ()
bldNewline = tell (word8 0x0A) >> put 0

bldText :: Text -> RenderM ()
bldText = mapM_ bldChar . T.unpack

bldSepBy :: RenderM () -> [RenderM ()] -> RenderM ()
bldSepBy sep vals = sequence_ $ intersperse sep vals

shouldQuote :: Text -> Bool
shouldQuote = T.any (`elem` [':', ';', ','])

bldParamValue :: Text -> RenderM ()
bldParamValue v | shouldQuote v = bldChar '"' >> bldText v >> bldChar '"'
bldParamValue v = bldText v

bldLine :: Text -> ICalValue -> RenderM ()
bldLine name val = do
  bldText name
  forM_ (M.toList $ val ^. icParams) $ \(param, pvalue) -> do
    bldChar ';'
    bldText param
    bldChar '='
    bldSepBy (bldChar ',') $ bldParamValue <$> pvalue
  bldChar ':'
  bldText $ val ^. icValue
  bldNewline

bldType :: ICalType -> Text
bldType VEVENT = "VEVENT"
bldType VTIMEZONE = "VTIMEZONE"
bldType (VOTHER tp) = tp

bldGroup :: ICalGroup -> RenderM ()
bldGroup group = do
  bldLine "BEGIN" $ ICValue M.empty $ bldType $ group ^. icType
  mapM_ (uncurry bldLine) $ M.toList $ group ^. icValues
  mapM_ bldGroup $ group ^. icSubGroups
  bldLine "END" $ ICValue M.empty $ bldType $ group ^. icType

bldFile :: ICalFile -> RenderM ()
bldFile ical = do
  bldLine "BEGIN" $ ICValue M.empty "VCALENDAR"
  bldLine "VERSION" $ ICValue M.empty $ ical ^. icVersion
  mapM_ (uncurry bldLine) $ M.toList $ ical ^. icOther
  mapM_ bldGroup $ ical ^. icGroups
  bldLine "END" $ ICValue M.empty "VCALENDAR"
