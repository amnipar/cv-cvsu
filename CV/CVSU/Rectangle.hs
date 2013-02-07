module CV.CVSU.Rectangle
( treeToRect
, treeToRectAndVal
, segToRect
, segToRectAndVal
, compToRect
, compToRectAndVal
, compToRectAndColor
, minCoveringRect
, getIntersectingRects
) where

import Utils.Rectangle
import CVSU.ConnectedComponents
import CVSU.QuadForest

-- | Extracts the rectangle representing the image region covered by the tree.
treeToRect :: QuadTree -> Rectangle Int
treeToRect t = mkRectangle (quadTreeX t, quadTreeY t) (quadTreeSize t, quadTreeSize t)

-- | Combines the tree rectangle and a value extracted from the tree.
treeToRectAndVal :: (QuadTree -> Float) -> QuadTree -> (Rectangle Int, Float)
treeToRectAndVal getVal t = (treeToRect t, getVal t)

-- | Extracts the segment bounding box as a rectangle.
segToRect :: ForestSegment -> Rectangle Int
segToRect s = mkRectangle (segmentX s, segmentY s) (segmentW s, segmentH s)

-- | Combines the segment rectangle and a value extracted from the segment.
segToRectAndVal :: (ForestSegment -> Float) -> ForestSegment -> (Rectangle Int, Float)
segToRectAndVal getVal s = (segToRect s, getVal s)

-- | Extracts the component bounding box as a rectangle.
compToRect :: ConnectedComponent -> Rectangle Int
compToRect c = mkRectangle (componentX c, componentY c) (componentW c, componentH c)

-- | Combines the component rectangle and a value extracted from the component.
compToRectAndVal :: (ConnectedComponent -> Float) -> ConnectedComponent -> (Rectangle Int, Float)
compToRectAndVal getVal c = (compToRect c, getVal c)

-- | Combines the component rectangle and the color assigned to the component.
compToRectAndColor :: ConnectedComponent -> (Rectangle Int, (Float,Float,Float))
compToRectAndColor c = (compToRect c, componentColor c)

-- | Find the minimum covering rectangle for a collection of rectangles
minCoveringRect :: [Rectangle Int] -> Rectangle Int
minCoveringRect [] = Rectangle 0 0 0 0
minCoveringRect rects = mkRectCorners (x1,y1) (x2,y2)
  where
    x1 = minimum $ map left rects
    y1 = minimum $ map top rects
    x2 = maximum $ map right rects
    y2 = maximum $ map bottom rects

-- | Find the collection of rectangles that intersect the given rectangle by
--   more than the given ratio.
getIntersectingRects :: Float -> [Rectangle Int] -> Rectangle Int
    -> (Rectangle Int, [Rectangle Int])
getIntersectingRects _ [] r = (r, [])
getIntersectingRects ratio rects r@(Rectangle rx ry rw rh) =
  (r, rects')
  where
    rects' = filter (intersecting rx ry rw rh) rects
    -- Calculate the area of a rectangle by the coordinates of two corners. The
    -- first corner should be the top left corner; if it isn't, the corners
    -- correspond to an empty intersection of rectangles and the area is 0.
    area (x1,y1) (x2,y2)
      | x2 > x1 && y2 > y1 = (x2-x1)*(y2-y1)
      | otherwise = 0
    intersecting rx ry rw rh (Rectangle x y w h)
      | area (max rx x, max ry y) (min (rx+rw) (x+w), min (ry+rh) (y+h))
          > (round $ ratio * (fromIntegral $ w * h)) = True
      | otherwise = False
