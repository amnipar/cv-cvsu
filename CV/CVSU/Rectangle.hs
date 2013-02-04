module CV.CVSU.Rectangle
( treeToRectangle
, segmentToRectangle
, componentToRectangle
) where

import Utils.Rectangle
import CVSU.ConnectedComponents
import CVSU.QuadForest

treeToRectangle t =
  mkRectangle (quadTreeX t, quadTreeY t) (quadTreeSize t, quadTreeSize t)

segmentToRectangle s =
  mkRectangle (segmentX s, segmentY s) (segmentW s, segmentH s)

componentToRectangle c =
  mkRectangle (componentX c, componentY c) (componentW c, componentH c)
