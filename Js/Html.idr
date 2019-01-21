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

public export
KeyEvtAttribute : Type -> Type
KeyEvtAttribute a = Attribute 'KeyEvtAttribute a

export
implementation HtmlAllAttribute 'HtmlAttribute where

export
implementation HtmlAllAttribute 'InputAttribute where

export
implementation HtmlAllAttribute 'KeyEvtAttribute where

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

export
tabindex : HtmlAllAttribute o => Int -> Attribute o a
tabindex idx = stringAttribute "tabindex" (show idx)


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

public export
record KeyEvent where
  constructor MkKeyEvent
  altKey : Bool
  char : String
  code : String
  ctrlKey : Bool
  isComposing : Bool
  key : String
  keyCode : Int
  locale : String
  location : Int
  metaKey : Bool
  repeat : Bool
  shiftKey : Bool  

keyRecordOfEvent : Ptr -> JS_IO KeyEvent
keyRecordOfEvent x =
  do
    altKey <- jscall "%0.altKey" (Ptr -> JS_IO Int) x
    char <- jscall "%0.char" (Ptr -> JS_IO String) x
    code <- jscall "%0.code" (Ptr -> JS_IO String) x
    ctrlKey <- jscall "%0.ctrlKey" (Ptr -> JS_IO Int) x
    isComposing <- jscall "%0.isComposing" (Ptr -> JS_IO Int) x
    key <- jscall "%0.key" (Ptr -> JS_IO String) x
    keyCode <- jscall "%0.keyCode" (Ptr -> JS_IO Int) x
    locale <- jscall "%0.locale" (Ptr -> JS_IO String) x
    location <- jscall "%0.location" (Ptr -> JS_IO Int) x
    metaKey <- jscall "%0.metaKey" (Ptr -> JS_IO Int) x
    repeat <- jscall "%0.repeat" (Ptr -> JS_IO Int) x
    shiftKey <- jscall "%0.shiftKey" (Ptr -> JS_IO Int) x
    pure (MkKeyEvent (altKey /= 0) char code (ctrlKey /= 0) (isComposing /= 0) key keyCode locale location (metaKey /= 0) (repeat /= 0) (shiftKey /= 0))

export
onkeydown : HtmlAllAttribute o => (KeyEvent -> a) -> Attribute o a
onkeydown convert = 
  eventListenerAttribute "keydown" 
  (\x =>
    do
      evt <- keyRecordOfEvent x
      pure (convert evt)
  )

export
onkeyup : HtmlAllAttribute o => (KeyEvent -> a) -> Attribute o a
onkeyup convert = 
  eventListenerAttribute "keyup"
  (\x =>
    do
      evt <- keyRecordOfEvent x
      pure (convert evt)
  )
