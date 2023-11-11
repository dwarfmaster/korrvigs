module Korrvigs.Web.Method (Method, select, selectOpt, methodGet, methodPost) where

import Korrvigs.Web.Backend
import Network.HTTP.Types (Method, methodGet, methodPost)

select :: Bool -> String -> Handler Widget -> Handler [(String, Widget)]
select True nm widgetM =
  widgetM >>= \widget -> pure $ [(nm, widget)]
select _ _ _ = pure []

selectOpt :: Bool -> String -> Handler (Maybe Widget) -> Handler [(String, Widget)]
selectOpt True nm widgetM =
  widgetM >>= \widgetO -> pure $ case widgetO of
    Just widget -> [(nm, widget)]
    Nothing -> []
selectOpt _ _ _ = pure []
