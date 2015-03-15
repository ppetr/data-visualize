module Data.Visualize
    ( Node(..)
    , Field(..)
    , toTree
    ) where

import Data.Data
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data Node = Node
    { dnType :: Text
    , dnConstr :: Text
    , dnFields :: [Field Node]
    }
  deriving (Eq, Ord, Show, Read)

data Field a = Field 
    { fLabel :: Maybe Text
    , fNode :: a
    }
  deriving (Eq, Ord, Show, Read)

toTree :: (Data a) => a -> Node
toTree x
    | AlgConstr _n <- constrRep (toConstr x) =
        Node typ con (gmapQr (:) [] (Field Nothing .toTree) x)
        -- TODO: Include record labels as well
    | otherwise =
        Node typ con []
  where
    typ = T.pack . dataTypeName . dataTypeOf $ x
    con = T.pack . showConstr . toConstr $ x
