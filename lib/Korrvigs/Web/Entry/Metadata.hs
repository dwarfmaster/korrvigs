module Korrvigs.Web.Entry.Metadata where

import Control.Lens
import Data.Aeson
import qualified Data.Aeson.Encoding as VEnc
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Enc
import Korrvigs.Entry
import Korrvigs.Monad (loadMetadata)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod

widget :: Entry -> Handler Widget
widget entry = do
  let i = entry ^. name
  mtdt <- loadMetadata i
  mtdts <- mapM (\(key, val) -> (key,val,) <$> newIdent) $ M.toList mtdt
  pure $ do
    Rcs.mtdtCode
    [whamlet|
  <details .common-details>
    <summary>Metadata
    <table .metadata-table>
      <tr>
        <th>Key
        <th>Value
      $forall (key,val,ident) <- mtdts
        <tr ##{ident}>
          <td .mtdt-key>#{CI.foldedCase key}
          <td .mtdt-value data-mtdt-value-for=#{CI.foldedCase key} data-mtdt-for=#{unId i}>
            #{prepareMtdtValue val}
          <td .mtdt-button-case>
            <button .mtdt-button .mtdt-edit-button data-mtdt-id=#{ident}>✎
          <td .mtdt-button-case>
            <button .mtdt-button .mtdt-rm-button data-mtdt-id=#{ident}>❌
      <tr>
        <td colspan=2>
          <button .mtdt-button #mtdt-add-button>➕
  |]
  where
    prepareMtdtValue :: Value -> LT.Text
    prepareMtdtValue val =
      let txt = Enc.decodeUtf8 $ VEnc.encodingToLazyByteString $ VEnc.value val
       in if LT.length txt < 50
            then txt
            else LT.take 47 txt <> "..."
