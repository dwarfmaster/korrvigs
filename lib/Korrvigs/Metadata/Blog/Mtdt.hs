module Korrvigs.Metadata.Blog.Mtdt where

import Data.Map (Map)
import Data.Text (Text)
import Korrvigs.Metadata
import Korrvigs.Metadata.TH

mkMtdt "BlogFiles" "blog-files" [t|Map Text Text|]
mkMtdt "BlogMtdt" "blog-meta" [t|Map Text Text|]
mkMtdt "BlogPost" "blogpost" [t|Text|]
mkMtdt "BlogFile" "blogfile" [t|Text|]
mkMtdt "BlogTags" "blogpost-tags" [t|[Text]|]
mkMtdt "PublishedDate" "published-date" [t|Text|]
mkMtdt "BlogTitle" "blogtitle" [t|Text|]
