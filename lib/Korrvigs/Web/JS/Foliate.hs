module Korrvigs.Web.JS.Foliate where

import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod

viewer :: Route WebData -> Handler Widget
viewer docUrl =
  pure $ do
    Rcs.foliateJS StaticR
    toWidget
      [cassius|
      foliate-view
        width: 100%
        height: 100%
        display: inline-block
        background: var(--base07)
      .foliate-container
        width: 100%
        height: 50em
        display: inline-block
        position: relative
        background: var(--base07)
      .foliate-button
        position: absolute
        z-index: 10
        font-size: 10rem
        cursor: pointer
        user-select: none
        background: var(--base01)
        color: var(--base0B)
        opacity: 0
        top: 50%
        transform: translate(0%, -50%)
      .foliate-button:hover
        opacity: 1
      .foliate-next-button
        right: 0
      .foliate-prev-button
        left: 0
    |]
    [whamlet|
      <div .foliate-container>
        <div .foliate-button .foliate-next-button>
          ⇛
        <div .foliate-button .foliate-prev-button>
          ⇚
        <div foliate-url=@{docUrl}>
    |]
