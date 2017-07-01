module Html

import Js.VirtualDom

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

export
div : List (HtmlAttribute a) -> List (Html a) -> Html a
div = node "div"

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
