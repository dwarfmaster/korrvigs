module Individual where

import Data.UUID (UUID)

-- A class is a unary predicate. Each individual belong to a class, which form a
-- hierarchy with a top element.
data Class = Class
  { class_name :: String,
    class_parent :: Maybe Class,
    class_children :: [Class]
  }
  deriving (Show, Eq)

-- Type of values accepted as arguments to predicates
data ValueType
  = VTBool
  | VTNumber
  | VTString
  deriving (Show, Eq)

-- A predicate is a binary relations between individuals. Predicates are typed
-- with classes or value types. Subtyping along class hierarchy is assumed.
data Pred = Pred
  { pred_name :: String,
    pred_src :: Either ValueType Class,
    pred_dst :: Either ValueType Class
  }
  deriving (Show, Eq)

-- An Uniform Resource Name, it is an accepted link to any individual, in any
-- namespace. URN may link to individuals that do not have an UUID yet. The
-- class of the linked individual must be deducible from the namespace.
data URN = URN
  { namespace :: String,
    link :: String
  }
  deriving (Show, Eq)

data Value
  = VBool Bool
  | VNumber Rational
  | VString String
  deriving (Show, Eq)

-- An edge if a predicate along with a target. This is used when the first
-- argument to the predicate is implicit in the context.
type Edge = (Pred, Either Value URN)

-- Individuals are the basic elements of our knowledge management system. Each
-- individual has a class and an UUID. Individuals are backed by a directory
-- which name include the UUID. In this directory there must be a individual.csv
-- file, that has two columns. The first one is a predicate name, and the second
-- one an arbitrary value or URN. Furthermore, additional predicates can be
-- automatically extracted from the present files. The class must be specified
-- as a specific class predicate.
data Individual = Individual
  { ind_uuid :: UUID,
    ind_class :: Class,
    ind_rel :: [Edge],
    ind_files :: [(FilePath, [Edge])]
  }
  deriving (Show, Eq)
