module Korrvigs.Metadata.TH where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH

mkMtdt :: Text -> Text -> Q Type -> Q [Dec]
mkMtdt tpNm nm getMtdttp = do
  mtdttp <- getMtdttp
  pure
    [ DataD [] tp [] Nothing [NormalC tp []] [],
      InstanceD
        Nothing
        []
        (AppT (ConT $ mkName "ExtraMetadata") (ConT tp))
        [ TySynInstD $ TySynEqn Nothing (AppT (ConT $ mkName "MtdtType") (ConT tp)) mtdttp,
          ValD (VarP $ mkName "mtdtName") (NormalB $ AppE (VarE 'const) $ LitE $ StringL $ T.unpack nm) []
        ]
    ]
  where
    tp = mkName $ T.unpack tpNm
