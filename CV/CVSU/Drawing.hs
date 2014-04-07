module CV.CVSU.Drawing
( valueToGrey
, valueToColor
, drawBoxes
, drawBoxesWithColors
, drawRectsWithVals
, drawCompRects
, drawCompRectsWithColors
, drawTreeValues
--, drawBoundaries
--, drawEdges
--, drawHEdges
--, drawVEdges
, drawLines
, drawWeightedLines
, drawGraphGray
) where

import CVSU.TypedPointer
import CVSU.ConnectedComponents
import CVSU.QuadForest
import CVSU.Graph
import CV.Image
import CV.Drawing hiding (drawLines)
import CV.ImageOp
import Utils.Rectangle
import CV.CVSU.Rectangle

import Debug.Trace

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
{-
toHLine (QuadTree _ x y s _ _ e _ _ _ _ _) = ((x,y+s`div`2),(x+s,y+s`div`2))

toVLine (QuadTree _ x y s _ _ _ _ _ _ _ _) = ((x+s`div`2,y),(x+s`div`2,y+s))

toLine (QuadTree _ x y s _ _ e _ _ _ _ _) = ((x+d+dx,y+d-dy),(x+d-dx,y+d+dy))
  where
    dx = round $ (edgeDY e / m) * fromIntegral d
    dy = round $ (edgeDX e / m) * fromIntegral d
    d = s `div` 2
    m = max (abs $ edgeDX e) (abs $ edgeDY e)
    -}
    
-- | Draws magnitude edges over an image. Optionally, also the edge response
--   values can be visualized over the image (if drawResp is true). The edge
--   direction is visualized as well, using the dx and dy components of the
--   edge response.
{-
drawEdges :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawEdges drawResp color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2)
    | ((x1,y1),(x2,y2)) <- map toLine $ filter quadTreeHasEdge ts]
  where
    img' | drawResp  = drawTreeValues (realToFrac.edgeMag.quadTreeEdge) ts img
         | otherwise = img
         -}
{-
drawBoundaries :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawBoundaries drawDev color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toHLine ts']
  <## [lineOp color size (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toVLine ts']
  where
    img' | drawDev  = drawTreeValues (realToFrac.segmentDevDev.quadTreeSegment) ts img
         | otherwise = img
    ts' = filter (segmentHasBoundary.quadTreeSegment) ts
    -}
-- | Draws the detected horizontal edges over an image. Optionally, also the
--   edge response values can be visualized over the image (if drawResp is true).
{-
drawHEdges :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawHEdges drawResp color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2)
    | ((x1,y1),(x2,y2)) <- map toHLine $ filter quadTreeHasHEdge ts]
  where
    img' | drawResp  = drawTreeValues (realToFrac.edgeDY.quadTreeEdge) ts img
         | otherwise = img
         -}
-- | Draws the detected vertical edges over an image. Optionally, also the
--   edge response values can be visualized over the image (if drawResp is true).
{-
drawVEdges :: Bool -> (D32,D32,D32) -> Int -> [QuadTree] -> Image RGB D32 -> Image RGB D32
drawVEdges drawResp color size ts img =
  img'
  <## [lineOp color size (x1,y1) (x2,y2)
    | ((x1,y1),(x2,y2)) <- map toVLine $ filter quadTreeHasVEdge ts]
  where
    img' | drawResp  = drawTreeValues (realToFrac.edgeDX.quadTreeEdge) ts img
         | otherwise = img
         -}
drawLines :: (D32,D32,D32) -> Int -> [((Int,Int),(Int,Int))] -> Image RGB D32 -> Image RGB D32
drawLines color size bs img =
  img <## [lineOp color size (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- bs]

drawWeightedLines :: (D32,D32,D32) -> Int -> [(((Int,Int),(Int,Int)),Double)] -> Image RGB D32 -> Image RGB D32
drawWeightedLines color size ls img =
  img
    <## [lineOp (weightColor color $ realToFrac weight) size (x1,y1) (x2,y2)
      | (((x1,y1),(x2,y2)),weight) <- ls]
  where
    adjust c w = (max 0 (min 1 (c + w - 1)))
    weightColor (c1,c2,c3) w = (adjust c1 w, adjust c2 w, adjust c3 w)

drawGraphGray :: (Integral a, Pointable a) => Graph a -> Image GrayScale Float
    -> Image GrayScale Float
drawGraphGray graph image =
  image
    <## [lineOp w 1 (x1,y1) (x2,y2)
      | (x1,y1,x2,y2,w) <- map linkToLine $ links graph]
    <## [circleOp v (x,y) 2 Filled
      | (x,y,v) <- map nodeToPoint $ nodes graph]
            -- filter (((==)0).val) $
  where
    iToF = fromIntegral
    linkToLine l = (x1,y1,x2,y2,w)
      where
        (x1,y1) = roundP $ nodePosition $ linkFrom l
        (x2,y2) = roundP $ nodePosition $ linkTo l
        w = ((realToFrac $ linkWeight l) - minweight) / scaleweight
        roundP (a,b) = (round a, round b)
    nodeToPoint n = (x,y,v)
      where
        x = round $ fst $ nodePosition n
        y = round $ snd $ nodePosition n
        v = ((iToF $ val n) - minval) / scaleval
    val = attributeValue.nodeAttribute
      -- | nodeAttribute n == NoSuchAttribute = 0
      -- | otherwise = (attributeValue.nodeAttribute) n
    --f = filter (((/=)NoSuchAttribute).nodeAttribute)
    weights = map linkWeight $ links graph
    minweight = realToFrac $ minimum weights
    maxweight = realToFrac $ maximum weights
    scaleweight = maxweight - minweight
    vals = map val $ nodes graph
    minval = iToF $ minimum vals
    maxval = iToF $ maximum vals
    scaleval = maxval - minval
