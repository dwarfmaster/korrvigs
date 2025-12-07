module Data.ERIS
  ( module Data.ERIS.Crypto,
    erisEncode,
    erisEncodeStreaming,
    erisDecode,
    erisDecodeStreaming,
    module Data.ERIS.DB.Class,
    ERISFileDB (..),
    dbRoot,
    runTests,
  )
where

import Data.ERIS.Crypto
import Data.ERIS.DB.Class
import Data.ERIS.DB.Plain
import Data.ERIS.Decode (erisDecode, erisDecodeStreaming)
import Data.ERIS.Encode (erisEncode)
import Data.ERIS.StreamingEncode (erisEncodeStreaming)
import Data.ERIS.Test (runTests)
