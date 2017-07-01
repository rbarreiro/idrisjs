module HtmlAttributes

import Js.VirtualDom
import Js.Html

export
value : String -> Attribute 'InputAttribute a
value v = propertyAttribute "value" v

export
cssClass : HtmlAllAttribute o => String -> Attribute o a
cssClass s = stringAttribute "class" s
