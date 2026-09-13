{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.AddressBook.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.Utils
import Korrvigs.Utils.Crypto ()
import Opaleye

-- Address books table

data AddressBookRowImpl a b c = AddressBookRow
  { _sqlAbookId :: a,
    _sqlAbookServer :: b,
    _sqlAbookUser :: c
  }

makeLenses ''AddressBookRowImpl
$(makeAdaptorAndInstanceInferrable "pAddressBookRow" ''AddressBookRowImpl)

type AddressBookRow = AddressBookRowImpl Int Text Text

type AddressBookRowSQL = AddressBookRowImpl (Field SqlInt4) (Field SqlText) (Field SqlText)

instance Default ToFields AddressBookRow AddressBookRowSQL where
  def = pAddressBookRow $ AddressBookRow def def def

addressBooksTable :: Table AddressBookRowSQL AddressBookRowSQL
addressBooksTable =
  table "addressbooks" $
    pAddressBookRow $
      AddressBookRow
        (nameKindField AddressBook)
        (tableField "server")
        (tableField "usr")

abookFromRow :: AddressBookRow -> Entry -> AddressBook
abookFromRow row entry =
  MkAddressBook
    { _abookEntry = entry,
      _abookServer = row ^. sqlAbookServer,
      _abookUser = row ^. sqlAbookUser
    }

sqlLoad :: (MonadKorrvigs m) => Int -> ((Entry -> AddressBook) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad addressBooksTable (view sqlAbookId) abookFromRow

sqlRemove :: Int -> [Delete Int64]
sqlRemove = genSqlRemove addressBooksTable $ view sqlAbookId
