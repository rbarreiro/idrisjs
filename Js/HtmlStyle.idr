module HtmlStyle


import public Js.VirtualDom
import Js.Html

%access export

private
listStyleToStyle : List Style -> Style
listStyleToStyle [] = Nil
listStyleToStyle (x::xs) = x :: listStyleToStyle xs


private
pixels : Double -> String
pixels x = show x ++ "px"

height : Double -> Style
height x = mkStyle "height" (pixels x)

width: Double -> Style
width x = mkStyle "width" (pixels x)

margin : Double -> Style
margin x = mkStyle "margin" (pixels x)

fill : String -> Style
fill x = mkStyle "fill" x

data Transform = MkTransform String

Show Transform where
  show (MkTransform s) = s

Semigroup Transform where
  (<+>) (MkTransform a) (MkTransform b) = MkTransform (a ++ b)

Monoid Transform where
  neutral = MkTransform ""


translate : Double -> Double -> Transform
translate x y =
  MkTransform $ "translate(" ++ pixels x ++ "," ++ pixels y ++ ")"

scale : Double -> Double -> Transform
scale x y =
  MkTransform $ "scale(" ++ show x ++ "," ++ show y ++ ")"

transform : Transform -> Style
transform (MkTransform x) = mkStyle "transform" x


public export
data FlexDirection = Row | Column

flexDirectionToString : FlexDirection -> String
flexDirectionToString Row = "row"
flexDirectionToString Column = "column"

export
data FlexOption = MkFlexOption Style

export
direction : FlexDirection -> FlexOption
direction x = MkFlexOption $  mkStyle "flex-direction" (flexDirectionToString x)

wrap : FlexOption
wrap = MkFlexOption $ mkStyle "flex-wrap" "wrap"


flex : List FlexOption -> Style
flex opts =
  [mkStyle "display" "flex", listStyleToStyle $ map (\(MkFlexOption x) => x) opts]

public export
data Position = Static | Fixed Double Double

position : Position -> Style
position Static = mkStyle "position" "static"
position (Fixed x y) = [mkStyle "position" "fixed", mkStyle "left" (pixels x), mkStyle "top" (pixels y)]

zIndex : Double -> Style
zIndex x = mkStyle "z-index" (show x)

backgroundColor : String -> Style
backgroundColor = mkStyle "background-color"

namespace UserSelect

  public export
  data UserSelectType = Auto | None | Text | All

  userSelect : UserSelectType -> Style
  userSelect Auto = mkStyle "user-select" "auto"
  userSelect None = mkStyle "user-select" "none"
  userSelect Text = mkStyle "user-select" "text"
  userSelect All = mkStyle "user-select" "all"
{-


padding : Double -> Attribute a f g
padding x = CSSAttribute "padding" (DynConst $ pixels x)

paddingTop : Double -> Attribute a f g
paddingTop x = CSSAttribute "paddingTop" (DynConst $ pixels x)

paddingTopF : (a -> Double) -> Attribute a b
paddingTopF f = CSSAttribute "paddingTop" (DynA $ \(_**x) => pixels $ f x)


data BoxShadowOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  Blur : Dyn (DPair a f) Double -> BoxShadowOption a f g
  HShadow : Dyn (DPair a f) Double -> BoxShadowOption a f g
  VShadow : Dyn (DPair a f) Double -> BoxShadowOption a f g

blur : Double -> BoxShadowOption a f g
blur x = Blur $ DynConst x

hShadow : Double -> BoxShadowOption a f g
hShadow x = HShadow $ DynConst x

vShadow : Double -> BoxShadowOption a f g
vShadow x = VShadow $ DynConst x

private
record BoxShadowArguments (a:Type) (f : a->Type) where
  constructor MkBoxShadowArguments
  hShadow : Dyn (DPair a f) Double
  vShadow : Dyn (DPair a f) Double
  blur : Dyn (DPair a f) Double
  spread : Dyn (DPair a f) Double
  color : Dyn (DPair a f) String

private
boxShadowOptionsToArgs : List (BoxShadowOption a f g) -> BoxShadowArguments a f
boxShadowOptionsToArgs x =
  foldl opt (MkBoxShadowArguments (pure 0) (pure 0) (pure 0) (pure 0) (pure "black")) x
  where
    opt : BoxShadowArguments a f -> BoxShadowOption a f g -> BoxShadowArguments a f
    opt y (Blur x) = record{blur = x} y
    opt y (HShadow x) = record{hShadow = x} y
    opt y (VShadow x) = record{vShadow = x} y

private
boxShadowArgsToString : Double -> Double -> Double -> Double -> String -> String
boxShadowArgsToString hsh vsh blr spr clr =
  unwords
      [pixels hsh, pixels vsh, pixels blr, pixels spr, clr]

boxShadow :  List (BoxShadowOption a f g) -> Attribute a f g
boxShadow {a} {f} x =
  CSSAttribute "box-shadow" (boxShadowArgsToString <$> hShadow args <*> vShadow args <*> blur args <*> spread args <*> color args)
  where
    args : BoxShadowArguments a f
    args = boxShadowOptionsToArgs x



  -}
