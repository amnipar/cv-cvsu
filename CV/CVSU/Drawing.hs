{-#LANGUAGE TypeFamilies#-}
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
, Colorable(..)
, drawGraphGray
, drawGraphColor
) where

import CVSU.TypedPointer
import CVSU.ConnectedComponents
import CVSU.QuadForest
import CVSU.Graph
import CVSU.Set

import CV.Image
import CV.Drawing hiding (drawLines)
import CV.ImageOp
import Utils.Rectangle
import CV.CVSU.Rectangle

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import System.Random
import System.IO.Unsafe

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

iToF = fromIntegral
    
class Colorable c where
  type ColorPickerParams c
  data ColorPicker c
  type GrayPickerParams c
  data GrayPicker c
  createColorPicker :: ColorPickerParams c -> [c] -> ColorPicker c
  createGrayPicker :: GrayPickerParams c-> [c] -> GrayPicker c
  pickColor :: ColorPicker c -> c -> (Float,Float,Float)
  pickGray :: GrayPicker c -> c -> Float

instance Colorable Int where
  type ColorPickerParams Int = (Bool,(Float,Float,Float),(Float,Float,Float))
  data ColorPicker Int =
    ColorPickerInt
    { cpIntMin :: Float
    , cpIntScale :: Float
    , cpIntInvert :: Bool
    , cpIntColorBase :: (Float,Float,Float)
    , cpIntColorScale :: (Float,Float,Float)
    }
  type GrayPickerParams Int = Bool
  data GrayPicker Int = 
    GrayPickerInt
    { gpIntMin :: Float
    , gpIntScale :: Float
    , gpInvert :: Bool
    }
  createColorPicker (inv,cbase,cscale) vs = 
    ColorPickerInt vmin vscale inv cbase cscale
    where
      vmin = iToF $ minimum vs
      vmax = iToF $ maximum vs
      vscale = vmax - vmin
  createGrayPicker inv vs =
    GrayPickerInt vmin vscale inv
    where
      vmin = iToF $ minimum vs
      vmax = iToF $ maximum vs
      vscale = vmax - vmin
  pickColor picker v
    | cpIntInvert picker = 
        (bblue - sblue * c, bgreen - sgreen * c, bred - sred * c)
    | otherwise          =
        (bblue + sblue * c, bgreen + sgreen * c, bred + sred * c)
    where 
      (bblue,bgreen,bred) = cpIntColorBase picker
      (sblue,sgreen,sred) = cpIntColorScale picker
      vmin = cpIntMin picker
      vscale = cpIntScale picker
      c = ((iToF v) - vmin) / vscale
  pickGray (GrayPickerInt vmin vscale inv) v
    | inv       = 1 - c 
    | otherwise = c
    where
      c = ((iToF v) - vmin) / vscale

instance Colorable Set where
  type ColorPickerParams Set = ()
  data ColorPicker Set =
    ColorPickerSet
    {
      cpSetColors :: Map.Map Int (Float,Float,Float)
    }
  type GrayPickerParams Set = ()
  data GrayPicker Set =
    GrayPickerSet
    {
      gpSetValues :: Map.Map Int Float
    }
  createColorPicker () vs = ColorPickerSet colors
    where
      (_,colors) = List.foldl' getColor (mkStdGen 1234,Map.empty) vs
      getColor (gen,col) (Set _ v) = (gen3,Map.insertWith skip v (b,g,r) col)
        where
          skip a b = b
          (b,gen1) = randomR (0,1) gen
          (g,gen2) = randomR (0,1) gen1
          (r,gen3) = randomR (0,1) gen2
      
  createGrayPicker () vs = GrayPickerSet values
    where
      (_,values) = List.foldl' getValue (mkStdGen 1234,Map.empty) vs
      getValue (gen,val) (Set _ v) = (gen',Map.insertWith skip v g val)
        where
          skip a b = b
          (g,gen') = randomR (0,1) gen
  pickColor (ColorPickerSet colors) (Set _ v) = c
    where 
      c = maybe (0,0,0) id $ Map.lookup v colors
  pickGray (GrayPickerSet values) (Set _ v) = g
    where
      g = maybe 0 id $ Map.lookup v values

drawGraphGray :: (Colorable a, AttribValue a, AttribValue b) =>
    GrayPicker a -> Attribute a -> Graph b -> Image GrayScale Float 
    -> Image GrayScale Float
drawGraphGray picker attrib graph image =
  image
    <## [lineOp w 1 (x1,y1) (x2,y2)
      | (x1,y1,x2,y2,w) <- map linkToLine $ links graph]
    <## [circleOp v (x,y) 2 Filled
      | (x,y,v) <- map nodeToPoint $ nodes graph]
            -- filter (((==)0).val) $
  where
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
        v = pickGray picker $ val n
    val n = unsafePerformIO $ getAttribute attrib n
    weights = map linkWeight $ links graph
    minweight = realToFrac $ minimum weights
    maxweight = realToFrac $ maximum weights
    scaleweight = maxweight - minweight

drawGraphColor :: (Colorable a, AttribValue a, AttribValue b) =>
    ColorPicker a -> Attribute a -> Graph b -> Image RGB Float
    -> Image RGB Float
drawGraphColor picker attrib graph image =
  image
    <## [lineOp (w,w,w) 1 (x1,y1) (x2,y2)
      | (x1,y1,x2,y2,w) <- map linkToLine $ links graph]
    <## [circleOp c (x,y) 2 Filled
      | (x,y,c) <- map nodeToPoint $ nodes graph]
            -- filter (((==)0).val) $
  where
    linkToLine l = (x1,y1,x2,y2,w)
      where
        (x1,y1) = roundP $ nodePosition $ linkFrom l
        (x2,y2) = roundP $ nodePosition $ linkTo l
        w = ((realToFrac $ linkWeight l) - minweight) / scaleweight
        roundP (a,b) = (round a, round b)
    nodeToPoint n = (x,y,c)
      where
        x = round $ fst $ nodePosition n
        y = round $ snd $ nodePosition n
        c = pickColor picker $ val n
    val n = unsafePerformIO $ getAttribute attrib n
    weights = map linkWeight $ links graph
    minweight = realToFrac $ minimum weights
    maxweight = realToFrac $ maximum weights
    scaleweight = maxweight - minweight
