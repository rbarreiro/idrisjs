module Html

import Js.VirtualDom
import Js.Utils

%include JavaScript "js/html.js"

public export
Html : Type -> Type
Html a = Node 'Html a

export
interface HtmlAllAttribute (o : Type) where

public export
HtmlAttribute : Type -> Type
HtmlAttribute a = Attribute 'HtmlAttribute a

public export
InputAttribute : Type -> Type
InputAttribute a = Attribute 'InputAttribute a

export
implementation HtmlAllAttribute 'HtmlAttribute where

export
implementation  HtmlAllAttribute 'InputAttribute where


-------- Nodes --------

export
div : List (HtmlAttribute a) -> List (Html a) -> Html a
div = node "div"

export
span : List (HtmlAttribute a) -> List (Html a) -> Html a
span = node "span"

export
emptySpan : Html a
emptySpan = span [] []


export
input : List (InputAttribute a) -> Html a
input attrs = node "input" attrs []

export
form : List (HtmlAttribute a) -> a -> List (Html a) -> Html a
form attrs val childs =
  node
  "form"
  (eventListenerAttributePreventDefault "submit" (\_ => pure val) :: attrs)
  childs

export
button : List (HtmlAttribute a) -> String -> Html a
button attrs txt = node "button" attrs [text txt]

export
img : List (HtmlAttribute a) -> String -> Html a
img attrs url =
  node
    "img"
    (stringAttribute "src" url :: attrs)
    []

-------- Attributes --------

export
value : String -> Attribute 'InputAttribute a
value v = propertyAttribute "value" v

export
cssClass : HtmlAllAttribute o => String -> Attribute o a
cssClass s = stringAttribute "class" s

-------- Events --------

export
onclick : HtmlAllAttribute o => a -> Attribute o a
onclick x = eventListenerAttribute "click" (\_ => pure x)

export
onchange : (String -> a) -> Attribute 'InputAttribute a
onchange convert =
  eventListenerAttribute
  "change"
  (\x => convert <$> jscall "%0.target.value" (Ptr -> JS_IO String) x)

export
onlongpress : HtmlAllAttribute o => a -> Attribute o a
onlongpress x = customEventListenerAttribute "longpress" (jscall "$JSLIB$html.addLongPressEventListener(%0)" (Ptr -> JS_IO ())) (\_ => pure x)

export
onshortpress : HtmlAllAttribute o => a -> Attribute o a
onshortpress x = customEventListenerAttribute "shortpress" (jscall "$JSLIB$html.addShortPressEventListener(%0)" (Ptr -> JS_IO ())) (\_ => pure x)
