{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web where

import Korrvigs.Web.Backend
import Korrvigs.Web.Collections
import Korrvigs.Web.Compute
import Korrvigs.Web.Download
import Korrvigs.Web.Entry
import Korrvigs.Web.Git
import Korrvigs.Web.Home
import Korrvigs.Web.Metadata
import Korrvigs.Web.Note
import Korrvigs.Web.Public
import Korrvigs.Web.Routes
import Korrvigs.Web.Search
import Yesod

mkYesodDispatch "WebData" korrvigsRoutes
