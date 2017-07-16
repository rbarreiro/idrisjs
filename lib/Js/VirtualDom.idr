module VirtualDom

import Js.ASync

%include JavaScript "js/virtualdom.js"

export
data Node : Type -> Type -> Type where
  MkNode : Ptr -> Node a b

export
data Attribute : Type -> Type -> Type where
  MkAttribute : Ptr -> Attribute a b

export
node : String -> List (Attribute b a) -> List (Node c a) -> Node c a
node tag attrs childs =
  let attrsPtr =
      unsafePerformIO $
        (makeJSList $ map (\(MkAttribute x) => x) attrs) >>=
          jscall "$JSLIB$virtualdom.attributesListToObj(%0)" (Ptr -> JS_IO Ptr)
      childsPtr = unsafePerformIO $ makeJSList $ map (\(MkNode x) => x) childs
  in MkNode $ unsafePerformIO $ jscall "{type: 'n', tag: %0, attrs: %1, childs: %2}" (String -> Ptr -> Ptr -> JS_IO Ptr) tag attrsPtr childsPtr

export
nodeNS : String -> String -> List (Attribute b a) -> List (Node c a) -> Node c a
nodeNS ns tag attrs childs =
  let attrsPtr =
      unsafePerformIO $
        (makeJSList $ map (\(MkAttribute x) => x) attrs) >>=
          jscall "$JSLIB$virtualdom.attributesListToObj(%0)" (Ptr -> JS_IO Ptr)
      childsPtr = unsafePerformIO $ makeJSList $ map (\(MkNode x) => x) childs
  in MkNode $ unsafePerformIO $ jscall "{type: 'nn', ns: %0 , tag: %1, attrs: %2, childs: %3}" (String -> String -> Ptr -> Ptr -> JS_IO Ptr) ns tag attrsPtr childsPtr

export
text : String -> Node c a
text txt =
  MkNode $ unsafePerformIO $ jscall "{type: 't', text: %0}" (String -> JS_IO Ptr) txt

export
stringAttribute : String -> String -> Attribute a b
stringAttribute name value =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'r',name:%0, value:%1}"
      (String -> String -> JS_IO Ptr)
      name
      value

export
stringAttributeNS : String -> String -> String -> Attribute a b
stringAttributeNS ns name value =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'rn', ns: %0, n:%1, name:%0+':'+%1, value:%2}"
      (String -> String -> String -> JS_IO Ptr)
      ns
      name
      value

export
propertyAttribute : String -> String -> Attribute a b
propertyAttribute name value =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'p',name:%0, value:%1}"
      (String -> String -> JS_IO Ptr)
      name
      value

export
data Style = MkStyle Ptr

export
mkStyle : String -> String -> Style
mkStyle n v =
  MkStyle $ unsafePerformIO $
    jscall
      "$JSLIB$virtualdom.mkStyle(%0,%1)"
      (String -> String -> JS_IO Ptr)
      n
      v

export
Nil : Style
Nil =
  MkStyle $ unsafePerformIO $
    jscall
      "{}"
      (JS_IO Ptr)

export
(::) : Style -> Style -> Style
(::) (MkStyle x) (MkStyle y) =
  MkStyle $ unsafePerformIO $
    jscall
      "Object.assign(%0,%1)"
      (Ptr -> Ptr -> JS_IO Ptr)
      y
      x

export
style : Style -> Attribute a b
style (MkStyle stl) =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'s', name: 'style', value:%0}"
      (Ptr -> JS_IO Ptr)
      stl

export
eventListenerAttribute : String -> (Ptr -> JS_IO b) -> Attribute a b
eventListenerAttribute name read =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'e', name:%0, read: %1}"
      (String -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO Ptr)
      name
      (MkJsFn $ believe_me read)

export
customEventListenerAttribute : String -> (Ptr -> JS_IO ()) -> (Ptr -> JS_IO b) -> Attribute a b
customEventListenerAttribute name initE read =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'ec', name:%0, init: %1 , read: %2}"
      (String -> JsFn (Ptr -> JS_IO ()) -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO Ptr)
      name
      (MkJsFn $ initE)
      (MkJsFn $ believe_me read)


export
eventListenerAttributePreventDefault : String -> (Ptr -> JS_IO b) -> Attribute a b
eventListenerAttributePreventDefault event convert =
  MkAttribute $ unsafePerformIO $
    jscall
      "{type:'ep', name:%0, read: %1}"
      (String -> JsFn (Ptr -> JS_IO Ptr) -> JS_IO Ptr)
      event
      (MkJsFn $ believe_me convert)


export
initialyzeBody : JSIOFifoQueue b -> Node a b -> JS_IO ()
initialyzeBody q (MkNode x) =
  jscall
   "$JSLIB$virtualdom.initialyzeBody(%0, %1)"
   (Ptr -> Ptr -> JS_IO ())
   (getQueuePtr q)
   x

export
updateNode : Node a b -> Node a b -> JS_IO ()
updateNode (MkNode x) (MkNode y) =
  jscall
    "$JSLIB$virtualdom.update(%0,%1)"
    (Ptr -> Ptr -> JS_IO ())
    x
    y

export
updateNodeM : JSIOFifoQueue b -> Node a c -> Node a b -> JS_IO (Node a b)
updateNodeM q (MkNode x) (MkNode y) =
  do
    jscall
      "$JSLIB$virtualdom.updateQueue(%0, %1)"
      (Ptr -> Ptr -> JS_IO ())
      (getQueuePtr q)
      x
    jscall
      "$JSLIB$virtualdom.update(%0,%1)"
      (Ptr -> Ptr -> JS_IO ())
      x
      y
    pure $ MkNode x

export
clearNode : Node a b -> JS_IO ()
clearNode (MkNode x) = jscall "$JSLIB$virtualdom.clear(%0)" (Ptr -> JS_IO ()) x


public export
data AnimationOption : Type where
  Duration : Nat -> AnimationOption

readAnimationOptions : List AnimationOption -> JS_IO Ptr
readAnimationOptions [] =
  jscall "{'fill':'both'}" (JS_IO Ptr)
readAnimationOptions ((Duration d)::r) =
  readAnimationOptions r >>= \x => jscall "Object.assign(%0,{'duration':%1})" (Ptr -> Int -> JS_IO Ptr) x (cast d)

export
animateNode : List AnimationOption -> Node a b -> Node a b -> JS_IO ()
animateNode animOpts (MkNode x) (MkNode y) =
  do
    opts <- readAnimationOptions animOpts
    jscall
      "$JSLIB$virtualdom.animate(%0,%1,%2)"
      (Ptr -> Ptr -> Ptr -> JS_IO ())
      opts
      x
      y

export
duration : Nat -> AnimationOption
duration = Duration

export
availableHeight_ : JS_IO Double
availableHeight_ = jscall "$JSLIB$virtualdom.availableHeight()" (JS_IO Double)

export
availableWidth_ : JS_IO Double
availableWidth_ = jscall "$JSLIB$virtualdom.availableWidth()" (JS_IO Double)
