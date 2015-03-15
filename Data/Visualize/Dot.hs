{-# LANGUAGE OverloadedStrings #-}

module Data.Visualize.Dot
    ( NodeId
    , mkDotGraph
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Text.Lazy as T
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic

import Data.Visualize

type NodeId = Int

type BuildM a = StateT NodeId (DotM NodeId) a

newId :: BuildM NodeId
newId = state (\i -> (i, i + 1))

treeToDot :: Node -> BuildM NodeId
treeToDot (Node t c fs) = do
    -- TODO: Use record labels as well
    subs <- mapM subtree fs
    let indices = map (T.pack . show) [(0 :: Int)..]
        subsWI = zip indices subs
    nid <- newId
    lift $ do
        node nid [ Label . toLabelValue $
                    [ FlipFields $
                        [ FieldLabel (snd $ T.breakOnEnd "." t)
                        , FieldLabel c
                        ]
                        ++
                        if null subsWI then []
                            else [FlipFields (map (PortName . PN . fst) subsWI)]
                    ]
                ]
        forM_ subsWI $ \(idx, Field _label nid2) ->
            edge nid nid2 [TailPort (LabelledPort (PN idx) Nothing)]
    return nid
  where
    subtree :: Field Node -> BuildM (Field NodeId)
    subtree (Field label n) = Field label <$> treeToDot n


mkDotGraph :: Node -> DotGraph NodeId
mkDotGraph x = digraph' . flip evalStateT 0 $ do
    lift . nodeAttrs $ [Shape Record]
    void . treeToDot $ x
