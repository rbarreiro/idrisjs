module HtmlEvents

import Js.VirtualDom
import Js.Html
import Js.Utils

export
onclick : HtmlAllAttribute o => a -> Attribute o a
onclick x = eventListenerAttribute "click" (\_ => pure x)

export
onChange : (String -> a) -> Attribute 'InputAttribute a
onChange convert =
  eventListenerAttribute
  "change"
  (\x => convert <$> jscall "%0.target.value" (Ptr -> JS_IO String) x)
