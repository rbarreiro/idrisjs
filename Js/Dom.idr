module Dom

import Control.ST
import public Js.Html
import public Js.VirtualDom
import public Js.ASync
import public Js.Utils

data GuiRefData : (a : Type) -> (f : a -> Type) -> (g : a -> Type) -> a -> Type where
  MkGuiRefData : ((z:a) -> f z -> Html (g z)) -> JSIORef (y:a ** JSIOFifoQueue (g y)) -> JSIOFifoQueue (g x) -> f x -> Html (g x) -> GuiRefData a f g x

export
data DomOption : (a:Type) -> (a->Type) -> (a->Type) -> Type where
  OnResize : ((x:a) -> g x) -> DomOption a f g

export
onresize : b -> DomOption () (const a) (const b)
onresize x = OnResize $ \() => x

record DomOptionRec (a : Type) (f:a->Type) (g:a->Type) where
  constructor MkDomOptionRec
  onresizeOption : Maybe ((x:a) -> g x)

domOptionsToRec : List (DomOption a f g) -> DomOptionRec a f g
domOptionsToRec [] = MkDomOptionRec Nothing
domOptionsToRec (OnResize x :: r) = record {onresizeOption = Just x} $ domOptionsToRec r

public export
interface Dom  (m : Type -> Type) where
  DomRef : (a:Type) -> (f : a -> Type) -> (g : a -> Type) -> a -> Type

  initBody : List (DomOption a f g) -> ((x:a) -> f x -> Html (g x)) -> (z:a) -> f z -> ST m Var [add (DomRef a f g z)]
  clearDom : (dom : Var) -> ST m () [remove dom (DomRef a f g z)]
  domPut : (dom : Var) -> {x:a} -> f x -> ST m () [dom ::: (DomRef a f g x)]
  domPutAnimated : (dom : Var) -> {x:a} -> List AnimationOption -> f x -> ST m () [dom ::: (DomRef a f g x)]
  domPutM : (dom : Var) -> (y:a) -> f y -> ST m () [dom ::: (DomRef a f g x) :-> (DomRef a f g y)]
  domGet : (dom : Var) -> {x:a} -> ST m (f x) [dom ::: DomRef a f g x]
  getInput : (dom : Var) -> {x:a} -> ST m (g x) [dom ::: DomRef a f g x]
  schedule : (dom : Var) -> {x:a} -> (t : Int) -> (i : g x) -> ST m () [dom ::: (DomRef a f g x)]

export
domUpdate : Dom m => (dom : Var) -> {x:a} -> (f x -> f x) -> ST m () [dom ::: (DomRef {m} a f g x)]
domUpdate v h =
  do
    s <- domGet v
    domPut v (h s)

export
domUpdateAnimated : Dom m => (dom : Var) -> {x:a} -> List AnimationOption -> (f x -> f x) -> ST m () [dom ::: (DomRef {m} a f g x)]
domUpdateAnimated v opts h =
  do
    s <- domGet v
    domPutAnimated v opts (h s)

export
domUpdateM : Dom m => (dom : Var) -> (x:a) -> (y:a) -> (f x -> f y) -> ST m () [dom ::: (DomRef {m} a f g x) :-> (DomRef {m} a f g y)]
domUpdateM v x y h =
  do
    s <- domGet v
    domPutM v y (h s)

setOnResizeOpt : (g : a -> Type) -> Maybe ((x:a) -> g x) -> JSIORef (y:a ** JSIOFifoQueue (g y)) -> JS_IO ()
setOnResizeOpt g Nothing ref =
  jscall
    "window.onresize = undefined"
    (JS_IO ())
setOnResizeOpt g (Just h) ref =
  jscall
    "window.onresize = %0"
    (JsFn (() -> JS_IO ()) -> JS_IO ())
    (MkJsFn $ \() =>
      do
        (x ** q) <- readJSIORef ref
        let z = h x
        putInQueue z q
    )

setOpts : DomOptionRec a f g -> JSIORef (y:a ** JSIOFifoQueue (g y)) -> JS_IO ()
setOpts {g} x q = setOnResizeOpt g (onresizeOption x) q

initBodyRaw : List (DomOption a f g) -> ((x:a) -> f x -> Html (g x) ) -> f z -> JS_IO (GuiRefData a f g z)
initBodyRaw {a} {f} {g} {z} opts render st0 =
  do
    let n0 = render z st0
    let opts' = domOptionsToRec opts
    queue <- newJSIOFifoQueue (g z)
    ref <- newJSIORef (z ** queue)
    setOpts opts' ref
    initialyzeBody queue n0
    pure $ MkGuiRefData render ref queue st0 n0


export
implementation Dom ASync where
  DomRef a f g z =
    State (GuiRefData a f g z)
  initBody opts render a0 start =
    do
      guiR <- lift $ liftJS_IO $ initBodyRaw opts render start
      r <- new guiR
      pure r
  clearDom v =
    do
      (MkGuiRefData _ _ _ _ n) <- read v
      lift $ liftJS_IO $ clearNode n
      delete v

  domPut v {x} newSt =
    do
      (MkGuiRefData render ref queue st n) <- read v
      lift $ liftJS_IO $ updateNode n (render x newSt)
      write v $ MkGuiRefData render ref queue newSt n
      pure ()
  domPutAnimated v {x} opts newSt =
    do
      (MkGuiRefData render ref queue st n) <- read v
      lift $ liftJS_IO $ animateNode opts n (render x newSt)
      write v $ MkGuiRefData render ref queue newSt n
      pure ()
  domPutM {g} v x newSt =
    do
      (MkGuiRefData render ref queue st n) <- read v
      queue <- lift $ liftJS_IO $ newJSIOFifoQueue (g x)
      lift $ liftJS_IO $ writeJSIORef ref (x**queue)
      n' <- lift $ liftJS_IO $ updateNodeM queue n (render x newSt)
      write v $ MkGuiRefData render ref queue newSt n'
      pure ()
  domGet v =
    do
      (MkGuiRefData render ref queue st n) <- read v
      pure st
  getInput v =
    do
      (MkGuiRefData render ref queue st n) <- read v
      r <- lift $ getFromQueue queue
      pure r
  schedule v {x} t i =
    do
      (MkGuiRefData render ref queue st n) <- read v
      lift $ liftJS_IO $ Utils.setTimeout 
        (do
          putInQueue i queue
        ) 
        t
      pure ()

public export
interface Window (m:Type -> Type) where
  getLocationSearch : STrans m String xs (const xs)
  innerWidth : STrans m Double xs (const xs)
  innerHeight : STrans m Double xs (const xs)
  availableWidth : STrans m Double xs (const xs)
  availableHeight : STrans m Double xs (const xs)

export
implementation Window ASync where
  getLocationSearch = lift $ liftJS_IO $ jscall "decodeURI(window.location.search)" (JS_IO String)
  innerWidth = lift $ liftJS_IO (jscall "window.innerWidth" (JS_IO Double))
  innerHeight = lift $ liftJS_IO (jscall "window.innerHeight" (JS_IO Double))
  availableWidth = lift $ liftJS_IO $ availableWidth_
  availableHeight = lift $ liftJS_IO $ availableHeight_
