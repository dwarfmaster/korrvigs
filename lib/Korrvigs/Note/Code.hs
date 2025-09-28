module Korrvigs.Note.Code where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Compute.Runnable
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Pandoc
import Korrvigs.Utils

fromId :: (Applicative f) => Text -> ((Attr, Text) -> f (Attr, Text)) -> Document -> f Document
fromId nm = docContent . each . bkSubBlocks . _CodeBlock . filtered ((== nm) . view (_1 . attrId))

knownLanguages :: Map Text Executable
knownLanguages =
  M.fromList
    [ ("bash", Bash),
      ("prolog", SwiProlog)
    ]

toRunnable :: Attr -> Text -> Maybe Runnable
toRunnable attr code = do
  let classes = attr ^. attrClasses
  language <- foldr ((<|>) . flip M.lookup knownLanguages) Nothing classes
  pure $
    Runnable
      { _runExecutable = language,
        _runCode = code,
        _runDeterministic = "nondeterministic" `notElem` classes,
        _runArgs = [],
        _runEnv = M.empty
      }

codeRunnable :: (MonadKorrvigs m) => Id -> Text -> m (Maybe Runnable)
codeRunnable i codeId = runMaybeT $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? _Note
  doc <- hoistEitherLift $ readNote $ note ^. notePath
  hoistMaybe $ doc ^? fromId codeId . to (uncurry toRunnable) . _Just
