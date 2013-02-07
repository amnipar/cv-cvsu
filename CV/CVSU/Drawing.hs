module CV.CVSU.Drawing
( valueToGrey
, valueToColor
, drawBoxes
, drawBoxesWithColors
, drawRectsWithVals
, drawCompRects
, drawCompRectsWithColors
, drawTreeValues
, drawEdges
, drawHEdges
, drawVEdges
) where

import CVSU.ConnectedComponents
import CVSU.QuadForest
import CV.Image
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle
import CV.CVSU.Rectangle

-- | Normalizes a value to range [0..1] using a minimum and maximum value. If
--   the given value is smaller than minV, minV is used; likewise, if it is
--   larger than maxV, then maxV is used.
normalizeValue :: Float -> Float -> Float -> Float
normalizeValue minV maxV v =
  ((min maxV (max minV v)) - minV) / (maxV - minV)

-- | Turns a value into a greyscale value used in drawing by normalizing it to
--   range [0..1].
valueToGrey = normalizeValue

-- | Turns a value into a color used in drawing by normalizing it to range
--   [0..1] and replicating the same value for all color components.
valueToColor :: Float -> Float -> Float -> (Float,Float,Float)
valueToColor minV maxV v =
  let c = normalizeValue minV maxV v in (c,c,c)

-- | Draws boxes (rectangle outlines) over an image using the given color.
drawBoxes :: (D32,D32,D32) -> Int -> [Rectangle Int] -> Image RGB D32 -> Image RGB D32
drawBoxes color size rs img =
  img <## [rectOp color size r | r <- rs]

-- | Draws boxes (rectangle outlines) over an image using the color associated
--   with the rectangles.
drawBoxesWithColors :: Int -> [(Rectangle Int, (D32,D32,D32))] -> Image RGB D32 -> Image RGB D32
drawBoxesWithColors size rs img =
  img <## [rectOp c size r | (r,c) <- rs]

-- | Draws filled rectangles over an image using the values associated with the
--   rectangles.
drawRectsWithVals :: [(Rectangle Int, Float)] -> Image RGB D32 -> Image RGB D32
drawRectsWithVals rs img =
  img <## [rectOp (valueToColor minV maxV v) (-1) r | (r,v) <- rs]
  where
    minV = minimum $ map snd rs
    maxV = maximum $ map snd rs

-- | Draws rectangles around connected components using the bounding box and the
--   given color.
drawCompRects :: (D32,D32,D32) -> Int -> ConnectedComponents -> Image RGB D32 -> Image RGB D32
drawCompRects color size comp =
  drawBoxes color size (map compToRect $ connectedComponents comp)

-- | Draws rectangles around connected components using the bounding box and the
--   color assigned to each component.
drawCompRectsWithColors :: Int -> ConnectedComponents -> Image RGB D32 -> Image RGB D32
drawCompRectsWithColors size comp =
  drawBoxesWithColors size (map compToRectAndColor $ connectedComponents comp)

-- | Visualizes the values of a collection of trees over an image.
--   A function is given for extracting the desired value from the trees.
drawTreeValues :: (QuadTree -> Float) -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawTreeValues getValue ts img =
  drawRectsWithVals (map (treeToRectAndVal getValue) ts) img

-- | Draws magnitude edges over an image. Optionally, also the edge response
--   values can be visualized over the image (if drawResp is true). The edge
--   direction is visualized as well, using the dx and dy components of the
--   edge response.
drawEdges :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawEdges drawResp color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2)
    | ((x1,y1),(x2,y2)) <- map toLine $ filter quadTreeHasEdge ts]
  where
    img' | drawResp  = drawTreeValues (realToFrac.edgeMag.quadTreeEdge) ts img
         | otherwise = img
    toLine (QuadTree _ x y s _ _ e _ _ _ _) =
      ((x+d+dx,y+d-dy),(x+d-dx,y+d+dy))
      where
        dx = round $ (edgeDY e / m) * fromIntegral d
        dy = round $ (edgeDX e / m) * fromIntegral d
        d = s `div` 2
        m = max (abs $ edgeDX e) (abs $ edgeDY e)

-- | Draws the detected horizontal edges over an image. Optionally, also the
--   edge response values can be visualized over the image (if drawResp is true).
drawHEdges :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawHEdges drawResp color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2)
    | ((x1,y1),(x2,y2)) <- map toHLine $ filter quadTreeHasHEdge ts]
  where
    img' | drawResp  = drawTreeValues (realToFrac.edgeDY.quadTreeEdge) ts img
         | otherwise = img
    toHLine (QuadTree _ x y s _ _ e _ _ _ _) = ((x,y+s`div`2),(x+s,y+s`div`2))

-- | Draws the detected vertical edges over an image. Optionally, also the
--   edge response values can be visualized over the image (if drawResp is true).
drawVEdges :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawVEdges drawResp color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2)
    | ((x1,y1),(x2,y2)) <- map toVLine $ filter quadTreeHasVEdge ts]
  where
    img' | drawResp  = drawTreeValues (realToFrac.edgeDX.quadTreeEdge) ts img
         | otherwise = img
    toVLine (QuadTree _ x y s _ _ _ _ _ _ _) = ((x+s`div`2,y),(x+s`div`2,y+s))
