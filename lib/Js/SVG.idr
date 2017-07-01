module SVG

import Js.VirtualDom
import Js.Html
import Js.HtmlStyle
import Data.Fin

public export
SVG : Type -> Type
SVG a = Node 'SVG a

export
interface SVGAllAttribute (o : Type) where

export
interface SVGShapeAttribute (o : Type) where

svgNS : String
svgNS = "http://www.w3.org/2000/svg"

xlinkNS : String
xlinkNS = "http://www.w3.org/1999/xlink"

export
svg : List (HtmlAttribute a) -> List (SVG a) -> Html a
svg attrs childs = nodeNS svgNS "svg" attrs (believe_me childs)

export
transform : SVGAllAttribute o => Transform -> Attribute o a
transform x = stringAttribute "transform" $ show x

export
fill : SVGShapeAttribute o => String -> Attribute o a
fill p = stringAttribute "fill" p

export
stroke : SVGShapeAttribute o => String -> Attribute o a
stroke p = stringAttribute "stroke" p

export
strokeWidth : SVGShapeAttribute o => Double -> Attribute o a
strokeWidth p = stringAttribute "stroke-width" $ show p

namespace Rect
  export
  implementation SVGAllAttribute 'Rect where

  export
  implementation SVGShapeAttribute 'Rect where

  export
  x : Double -> Attribute 'Rect a
  x p = stringAttribute "x" $ show p

  export
  y : Double -> Attribute 'Rect a
  y p = stringAttribute "y" $ show p

  export
  width : Double -> Attribute 'Rect a
  width p = stringAttribute "width" $ show $ max 0 p

  export
  height : Double -> Attribute 'Rect a
  height p = stringAttribute "height" $ show $ max 0 p

  export
  rect : List (Attribute 'Rect a) -> SVG a
  rect attrs = nodeNS svgNS "rect" (attrs ++ [x (-1), y (-1), width 2, height 2]) []

namespace Image
  export
  implementation SVGAllAttribute 'Image where

  export
  x : Double -> Attribute 'Image a
  x p = stringAttribute "x" $ show p

  export
  y : Double -> Attribute 'Image a
  y p = stringAttribute "y" $ show p

  export
  width : Double -> Attribute 'Image a
  width p = stringAttribute "width" $ show $ max 0 p

  export
  height : Double -> Attribute 'Image a
  height p = stringAttribute "height" $ show $ max 0 p

  export
  image : List (Attribute 'Image a) -> String -> SVG a
  image attrs url =
    nodeNS
      svgNS
      "image"
      ( attrs
      ++ [ stringAttributeNS xlinkNS "xlink:href" url
         , x (-1)
         , y (-1)
         , width 2
         , height 2
         ]
      )
      []

namespace Text
  export
  implementation SVGAllAttribute 'Text where

  export
  x : Double -> Attribute 'Text a
  x p = stringAttribute "x" $ show $ max 0 p

  export
  y : Double -> Attribute 'Text a
  y p = stringAttribute "y" $ show $ max 0 p

  export
  fontSize : Double -> Attribute 'Text a
  fontSize x = stringAttribute "font-size" $ show x

  public export
  data TextAnchor = Start | End | Middle

  anchorToString : TextAnchor -> String
  anchorToString Start = "start"
  anchorToString End = "end"
  anchorToString Middle = "middle"

  export
  textAnchor : TextAnchor -> Attribute 'Text a
  textAnchor p = stringAttribute "text-anchor" $ anchorToString p

  export
  text : List (Attribute 'Text a) -> String -> SVG a
  text opts t = nodeNS svgNS "text" opts [text t]

namespace Circle
  export
  implementation SVGAllAttribute 'Circle where

  export
  implementation SVGShapeAttribute 'Circle where

  export
  r : Double -> Attribute 'Circle a
  r x = stringAttribute "r" $ show x

  export
  cx : Double -> Attribute 'Circle a
  cx x = stringAttribute "cx" $ show x

  export
  cy : Double -> Attribute 'Circle a
  cy x = stringAttribute "cy" $ show x

  export
  circle : List (Attribute 'Circle a) -> SVG a
  circle attrs = nodeNS svgNS "circle" (attrs ++ [r 1]) []

namespace Group
  export
  implementation SVGAllAttribute 'Group where

  export
  g : List (Attribute 'Group a) -> List (SVG a) -> SVG a
  g = nodeNS svgNS "g"


export
onclick : SVGAllAttribute o => a -> Attribute o a
onclick x = eventListenerAttribute "click" (\_ => pure x)


export
Cast (Fin n) Double where
  cast x = cast (the Nat $ cast x)

export
Cast Double Nat where
  cast x = cast (the Integer $ cast x)

public export
record Cell where
  constructor MkCellBase
  cleft : Double
  ctop : Double
  cwidth : Double
  cheight : Double
  cmarginLeft : Double
  cmarginRight : Double
  cmarginTop : Double
  cmarginBottom : Double

export
cbottom : Cell -> Double
cbottom x = ctop x + cheight x

export
cright : Cell -> Double
cright x = cleft x + cwidth x

export
cXcenter : Cell -> Double
cXcenter x = cleft x + cmarginLeft x + (cwidth x - cmarginLeft x - cmarginRight x) /2

export
cYcenter : Cell -> Double
cYcenter x = ctop x + cmarginTop x + (cheight x - cmarginTop x - cmarginBottom x) /2

export
cinnerWidth : Cell -> Double
cinnerWidth x = cwidth x - cmarginLeft x - cmarginRight x

export
cinnerHeigth : Cell -> Double
cinnerHeigth x = cheight x - cmarginTop x - cmarginBottom x

export
fitToCellTransform : Cell -> Transform
fitToCellTransform x = translate (cXcenter x) (cYcenter x) <+> scale (cinnerWidth x / 2) (cinnerHeigth x / 2)

public export
mkCell : Double -> Double -> Double -> Double -> Double -> Cell
mkCell left top width height margin = MkCellBase left top width height margin margin margin margin


public export
Grid : Nat -> Nat -> Type
Grid i j = Fin i -> Fin j -> Cell

public export
Flow : Type
Flow = Nat -> Cell

export total
bestFitFlowCentered : Double -> Double -> Double -> Double -> Double -> Double -> Flow
bestFitFlowCentered cellWidth cellHeight margin top left right z =
  let cols = cast $ ((right - left) / cellHeight) - 1
      leftstart = left + (right - left - (cast $ S cols) * cellWidth)/2
  in mkCell
            (leftstart + (cast $ modNatNZ z (S cols) SIsNotZ) * cellWidth)
            (top + (cast $ divNatNZ z (S cols) SIsNotZ) * cellHeight)
            cellWidth
            cellHeight
            margin


export
centeredSquaresGrid : (i:Nat) -> (j:Nat) -> Double -> Double -> Double -> Double -> Double -> Grid i j
centeredSquaresGrid i j cellMargin left right top bottom =
  let side = min ( (bottom - top ) / cast i ) ( ( right - left) / cast j )
      margin' = if cellMargin * 3 < side then cellMargin else 0.0
      topstart = top + (bottom - top - cast i * side)/2
      leftstart = left + (right - left - cast j * side)/2
  in (\x,y => mkCell (cast y * side + leftstart) (cast x * side + topstart) side side margin')
