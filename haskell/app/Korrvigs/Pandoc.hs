module Korrvigs.Pandoc where

import Data.Default (def)
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

writerOptions :: WriterOptions
writerOptions = def {writerExtensions = pandocExtensions, writerSectionDivs = True}

readerOptions :: ReaderOptions
readerOptions = def {readerExtensions = pandocExtensions}
