module SVG

import Js.Browser

svgNS : String
svgNS = "http://www.w3.org/2000/svg"

xlinkNS : String
xlinkNS = "http://www.w3.org/1999/xlink"

export
interface ElemOption (o:(a:Type) -> (f:a->Type) -> (g:a->Type) -> Type) where
  elemAttribute : (a:Type) -> (f:a->Type) -> (g:a->Type) -> Attribute a f g -> o a f g

export
interface ShapeOption (o:(a:Type) -> (f:a->Type) -> (g:a->Type) -> Type) where
  shapeAttribute : (a:Type) -> (f:a->Type) -> (g:a->Type) -> Attribute a f g -> o a f g

export
data BCircleOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  MkCircleOption : Attribute a f g -> BCircleOption a f g

export
data BRectOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  MkRectOption : Attribute a f g -> BRectOption a f g

export
data BImageOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  MkImageOption : Attribute a f g -> BImageOption a f g

export
data BTextOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  MkTextOption : Attribute a f g -> BTextOption a f g

export
data BGOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  MkGOption : Attribute a f g -> BGOption a f g

export
ShapeOption BRectOption where
  shapeAttribute _ _ _ x = MkRectOption x

export
ShapeOption BCircleOption where
  shapeAttribute _ _ _ x = MkCircleOption x

export
ShapeOption BTextOption where
  shapeAttribute _ _ _ x = MkTextOption x

export
ElemOption BRectOption where
  elemAttribute _ _ _ x = MkRectOption x

export
ElemOption BImageOption where
  elemAttribute _ _ _ x = MkImageOption x

export
ElemOption BCircleOption where
  elemAttribute _ _ _ x = MkCircleOption x

export
ElemOption BTextOption where
  elemAttribute _ _ _ x = MkTextOption x

export
ElemOption BGOption where
  elemAttribute _ _ _ x = MkGOption x

{-
TransformD : (a:Type) -> (a -> Type) -> Type
TransformD a f = (x:a) -> (f x) -> Transform

joinTrfD: TransformD a f -> TransformD a f -> TransformD a f
joinTrfD a b = \x, v => a x v <+> b x v
-}
export
data BSVGElem : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  CMapElem : ((x:a) -> h x -> f x) -> BSVGElem a f g -> BSVGElem a h g
  Circle : List (BCircleOption a f g) -> BSVGElem a f g
  Rect : List (BRectOption a f g) -> BSVGElem a f g
  Image : List (BImageOption a f g) -> Dyn (DPair a f) String ->
            BSVGElem a f g
  G : List (BGOption a f g) ->
        ((x:a) -> f x -> List (h x)) ->
              BSVGElem a h g -> BSVGElem a f g
  SG : List (BGOption a f g) ->
        List (BSVGElem a f g) -> BSVGElem a f g
  Text : List (BTextOption a f g) -> Dyn (DPair a f) String ->
            BSVGElem a f g
  SVGTransform :  Attribute a f g -> BSVGElem a f g -> BSVGElem a f g

namespace Dependent

  public export
  SVGElem : (a:Type) -> (a->Type) -> (a->Type) -> Type
  SVGElem = BSVGElem

  public export
  CircleOption : (a:Type) -> (a->Type) -> (a->Type) -> Type
  CircleOption = BCircleOption

  public export
  RectOption : (a:Type) -> (a->Type) -> (a->Type) -> Type
  RectOption = BRectOption

  public export
  ImageOption : (a:Type) -> (a->Type) -> (a->Type) -> Type
  ImageOption = BImageOption

  public export
  TextOption : (a:Type) -> (a->Type) -> (a->Type) -> Type
  TextOption = BTextOption

  public export
  GOption : (a:Type) -> (a->Type) -> (a->Type) -> Type
  GOption = BGOption

  export
  (>$<) : ((x:a) -> h x -> f x) -> BSVGElem a f g -> BSVGElem a h g
  (>$<) = CMapElem

namespace Simple
  public export
  SVGElem : {t:Type} -> Type -> Type -> Type
  SVGElem {t} b c = BSVGElem t (const b) (const c)

  public export
  CircleOption : {t:Type} -> Type -> Type -> Type
  CircleOption {t} b c = BCircleOption t (const b) (const c)

  public export
  RectOption : {t:Type} -> Type -> Type -> Type
  RectOption {t} b c = BRectOption t (const b) (const c)

  public export
  ImageOption : {t:Type} -> Type -> Type -> Type
  ImageOption {t} b c = BImageOption t (const b) (const c)

  public export
  TextOption : {t:Type} -> Type -> Type -> Type
  TextOption {t} b c = BTextOption t (const b) (const c)

  public export
  GOption : {t:Type} -> Type -> Type -> Type
  GOption {t} b c = BGOption t (const b) (const c)

  export
  (>$<) : {t:Type} -> (c -> b) -> BSVGElem t (const b) (const d) -> BSVGElem t (const c) (const d)
  (>$<) fn = CMapElem (\_=>fn)

namespace Circle
  export
  circle : List (CircleOption a f g) -> SVGElem a f g
  circle = Circle

  export
  r : Double -> CircleOption a f g
  r x = MkCircleOption $ customStrAttr "r" (DynConst $ show x)

  export
  rF : (a->Double) -> CircleOption a b
  rF f = MkCircleOption $ customStrAttr "r" (DynA $ \(_**x)=> show $ f x)

  export
  cx : Double -> CircleOption a f g
  cx x = MkCircleOption $ customStrAttr "cx" (DynConst $ show x)

  export
  cxF : (a->Double) -> CircleOption a b
  cxF f = MkCircleOption $ customStrAttr "cx" (DynA $ \(_**x)=> show $ f x)

  export
  cy : Double -> CircleOption a f g
  cy x = MkCircleOption $ customStrAttr "cy" (DynConst $ show x)

  export
  cyF : (a->Double) -> CircleOption a b
  cyF f = MkCircleOption $ customStrAttr "cy" (DynA $ \(_**x)=> show $ f x)

namespace Rect
  export
  rect : List (RectOption a f g) -> SVGElem a f g
  rect = Rect

  export
  x : Double -> RectOption a f g
  x p = MkRectOption $ customStrAttr "x" (DynConst $ show p)

  export
  xF : (a->Double) -> RectOption a b
  xF f = MkRectOption $ customStrAttr "x" (DynA $ \(_**x)=> show $ f x)

  export
  y : Double -> RectOption a f g
  y p = MkRectOption $ customStrAttr "y" (DynConst $ show p)

  export
  yF : (a->Double) -> RectOption a b
  yF f = MkRectOption $ customStrAttr "y" (DynA $ \(_**x)=> show $ f x)

  export
  width : Double -> RectOption a f g
  width p = MkRectOption $ customStrAttr "width" (DynConst $ show $ max 0 p)

  export
  widthF : (a->Double) -> RectOption a b
  widthF f = MkRectOption $ customStrAttr "width" (DynA $ \(_**x)=> show $ max 0 $ f x)

  export
  height : Double -> RectOption a f g
  height p = MkRectOption $ customStrAttr "height" (DynConst $ show $ max 0 p)

  export
  heightF : (a->Double) -> RectOption a b
  heightF f = MkRectOption $ customStrAttr "height" (DynA $ \(_**x)=> show $ max 0 $ f x)

namespace Image
  export
  image : List (ImageOption a f g) -> String -> SVGElem a f g
  image opts s = Image opts (DynConst s)

  export
  imageF : {t:Type} -> List (ImageOption t (const a) (const b)) -> (a->String) -> SVGElem t (const a) (const b)
  imageF opts s = Image opts (DynA $ \(_**x) => s x)

  export
  x : Double -> ImageOption a f g
  x p = MkImageOption $ customStrAttr "x" (DynConst $ show  p)

  export
  xF : (a->Double) -> ImageOption a b
  xF f = MkImageOption $ customStrAttr "x" (DynA $ \(_**x)=> show $ f x)

  export
  y : Double -> ImageOption a f g
  y p = MkImageOption $ customStrAttr "y" (DynConst $ show p)

  export
  yF : (a->Double) -> ImageOption a b
  yF f = MkImageOption $ customStrAttr "y" (DynA $ \(_**x)=> show $ f x)

  export
  width : Double -> ImageOption a f g
  width p = MkImageOption $ customStrAttr "width" (DynConst $ show $ max 0 p)

  export
  widthF : (a->Double) -> ImageOption a b
  widthF f = MkImageOption $ customStrAttr "width" (DynA $ \(_**x)=> show $ max 0 $ f x)

  export
  height : Double -> ImageOption a f g
  height p = MkImageOption $ customStrAttr "height" (DynConst $ show $ max 0 p)

  export
  heightF : (a->Double) -> ImageOption a b
  heightF f = MkImageOption $ customStrAttr "height" (DynA $ \(_**x)=> show $ max 0 $ f x)

namespace Text
  export
  text : List (TextOption a f g) -> String -> SVGElem a f g
  text opts t = Text opts (DynConst t)

  export
  textF : {t:Type} -> List (TextOption t (const a) (const b)) -> (a->String) -> SVGElem t (const a) (const b)
  textF opts t = Text opts (DynA $ \(_**x) => t x)

  export
  x : Double -> TextOption a f g
  x p = MkTextOption $ customStrAttr "x" (DynConst $ show p)

  export
  xF : (a->Double) -> TextOption a b
  xF f = MkTextOption $ customStrAttr "x" (DynA $ \(_**x)=> show $ f x)

  export
  y : Double -> TextOption a f g
  y p = MkTextOption $ customStrAttr "y" (DynConst $ show p)

  export
  yF : (a->Double) -> TextOption a b
  yF f = MkTextOption $ customStrAttr "y" (DynA $ \(_**x)=> show $ f x)

  export
  fontSize : Double -> TextOption a f g
  fontSize x = MkTextOption $ customStrAttr "font-size" (DynConst $ show x)

  public export
  data TextAnchor = Start | End | Middle

  anchorToString : TextAnchor -> String
  anchorToString Start = "start"
  anchorToString End = "end"
  anchorToString Middle = "middle"

  export
  textAnchor : TextAnchor -> TextOption a f g
  textAnchor p = MkTextOption $ customStrAttr "text-anchor" (DynConst $ anchorToString p)

export
fill : ShapeOption o => String -> o a f g
fill {a} {f} {g} p = shapeAttribute a f g $ CSSAttribute "fill" (DynConst p)

{-
export
transformSVGD : ((x:a) -> f x -> Transform) -> BSVGElem a f g -> BSVGElem a f g
transformSVGD h x = SVGTransform (transformD h) x
-}
export
transformSVGF : {t : Type} -> (a -> Transform) -> BSVGElem t (const a) (const b) -> BSVGElem t (const a) (const b)
transformSVGF h x = SVGTransform (customStrAttr "transform" (DynA $ \(_**z) => show $ h z )) x
{-
export
transformSVG : Transform -> BSVGElem a f g -> BSVGElem a f g
transformSVG t x = SVGTransform (transform t) x
-}

export
onclickD : ElemOption o => ((x:a) -> f x -> g x) -> o a f g
onclickD {a} {f} {g} fn = elemAttribute a f g $ onclickD fn

export
onclick : ElemOption o => {t:Type} -> (b -> c) -> o t (const b) (const c)
onclick {t} {b} {c} fn = elemAttribute t (const b) (const c) $ onclick fn

namespace Group
  export
  gD : List (GOption a f g) ->
        ((x:a) -> f x -> List (h x)) ->
              SVGElem a h g -> SVGElem a f g
  gD = G

  export
  g : {t:Type} -> List (GOption t (const b) (const c)) ->
        (b -> List d) ->
              SVGElem t (const d) (const c) -> SVGElem t (const b) (const c)
  g o f e = G o (\_,z=>f z) e


export
sG :  List (GOption a f g) -> List (SVGElem a f g) -> SVGElem a f g
sG = SG

circleOptToAttr : CircleOption a f g -> Attribute a f g
circleOptToAttr (MkCircleOption x) = x

rectOptToAttr : RectOption a f g -> Attribute a f g
rectOptToAttr (MkRectOption x) = x

imageOptToAttr : ImageOption a f g -> Attribute a f g
imageOptToAttr (MkImageOption x) = x

gOptToAttr : GOption a f g -> Attribute a f g
gOptToAttr (MkGOption x) = x

textOptToAttr : TextOption a f g -> Attribute a f g
textOptToAttr (MkTextOption x) = x


covering
svgToTempl : SVGElem a f g -> Template a f g
svgToTempl (CMapElem f x) =
  f >$< svgToTempl x
svgToTempl (Circle opts) =
  customNodeNS svgNS "circle" (map circleOptToAttr (r 1::opts)) []
svgToTempl (Rect opts) =
  customNodeNS svgNS "rect" (map rectOptToAttr (width 2 :: height 2 :: x (-1) :: y (-1) :: opts)) []
svgToTempl (Image opts url) =
  customNodeNS
    svgNS
    "image"
    (customStrAttrNS xlinkNS "xlink:href" url :: map imageOptToAttr ( width 2 :: height 2 :: x (-1) :: y (-1) :: opts) )
    []
svgToTempl (G opts fn childT) =
  listCustomNS svgNS "g" (map gOptToAttr opts) fn (svgToTempl childT)
svgToTempl (SG opts childs) =
  customNodeNS svgNS "g" (map gOptToAttr opts) (map svgToTempl childs)
svgToTempl (Text opts str) =
  customTextNS svgNS "text" (map textOptToAttr opts) str
svgToTempl (SVGTransform a c) =
  customNodeNS svgNS "g" [a] [svgToTempl c]

export
svg : List (Attribute a f g) -> List (SVGElem a f g) -> Template a f g
svg attrs childs =
  customNodeNS svgNS "svg" attrs
    (map svgToTempl childs)

export
Cast (Fin n) Double where
  cast x = cast (the Nat $ cast x)

public export
record Grid (i : Nat) (j : Nat) where
  constructor MkGrid
  left : Fin j -> Double
  top : Fin i -> Double
  cellWidth : Fin j -> Double
  cellHeight : Fin i -> Double
  marginLeft : Fin j -> Double
  marginRight : Fin j -> Double
  marginTop : Fin i -> Double
  marginBottom : Fin i -> Double

export
centeredSquaresGrid : (i:Nat) -> (j:Nat) -> Double -> Double -> Double -> Double -> Double -> Grid i j
centeredSquaresGrid i j cellMargin left right top bottom =
  let side = min ( (bottom - top ) / cast i ) ( ( right - left) / cast j )
      margin' = if cellMargin < side then cellMargin else 0.0
      topstart = top + (bottom - top - cast i * side)/2
      leftstart = left + (right - left - cast j * side)/2
  in MkGrid
      (\x => cast x * side + leftstart)
      (\x => cast x * side + topstart)
      (const side)
      (const side)
      (const margin')
      (const margin')
      (const margin')
      (const margin')

export
right : Grid i j -> Fin j -> Double
right g x = left g x + cellWidth g x

export
bottom : Grid i j -> Fin i -> Double
bottom g x = top g x + cellHeight g x

export
xcenter : Grid i j -> Fin j -> Double
xcenter g x = left g x + cellWidth g x / 2.0

export
ycenter : Grid i j -> Fin i -> Double
ycenter g x = top g x + cellHeight g x / 2.0

export
cellInnerWidth : Grid i j -> Fin j -> Double
cellInnerWidth g x = cellWidth g x - marginLeft g x - marginRight g x

export
cellInnerHeigth : Grid i j -> Fin i -> Double
cellInnerHeigth g x = cellHeight g x - marginTop g x - marginBottom g x

export
fitToCellTransform : Grid i j -> Fin i -> Fin j -> Transform
fitToCellTransform g m n = translate (xcenter g n) (ycenter g m) <+> scale (cellInnerWidth g n / 2) (cellInnerHeigth g m / 2)
