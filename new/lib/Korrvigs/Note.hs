module Korrvigs.Note
  ( Document (..),
    docMtdt,
    docContent,
    docTitle,
    docRefTo,
    Header (..),
    hdAttr,
    hdTitle,
    hdRefTo,
    hdLevel,
    hdContent,
    hdParent,
    hdDocument,
    Block (..),
    Inline (..),
    Style (..),
    Attr (..),
    attrId,
    attrClasses,
    attrMtdt,
  )
where

import Korrvigs.Note.AST
