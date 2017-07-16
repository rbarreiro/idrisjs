module Dom

import Control.ST
import public Js.Html
import public Js.VirtualDom
import public Js.ASync


data GuiRefData : (a : Type) -> (f : a -> Type) -> (g : a -> Type) -> a -> Type where
  MkGuiRefData : ((z:a) -> f z -> Html (g z)) -> JSIOFifoQueue (g x) -> f x -> Html (g x) -> GuiRefData a f g x

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

  initBody : ((x:a) -> f x -> Html (g x)) -> (z:a) -> f z -> ST m Var [add (DomRef a f g z)]
  clearDom : (dom : Var) -> ST m () [remove dom (DomRef a f g z)]
  domPut : (dom : Var) -> {x:a} -> f x -> ST m () [dom ::: (DomRef a f g x)]
  domPutM : (dom : Var) -> (y:a) -> f y -> ST m () [dom ::: (DomRef a f g x) :-> (DomRef a f g y)]
  domGet : (dom : Var) -> {x:a} -> ST m (f x) [dom ::: DomRef a f g x]
  getInput : (dom : Var) -> {x:a} -> ST m (g x) [dom ::: DomRef a f g x]

export
domUpdate : Dom m => (dom : Var) -> {x:a} -> (f x -> f x) -> ST m () [dom ::: (DomRef {m} a f g x)]
domUpdate v h =
  do
    s <- domGet v
    domPut v (h s)

export
domUpdateM : Dom m => (dom : Var) -> (x:a) -> (y:a) -> (f x -> f y) -> ST m () [dom ::: (DomRef {m} a f g x) :-> (DomRef {m} a f g y)]
domUpdateM v x y h =
  do
    s <- domGet v
    domPutM v y (h s)

setOnResizeOpt : (g : a -> Type) -> Maybe ((x:a) -> g x) -> JSIOFifoQueue (g x) -> JS_IO ()
setOnResizeOpt g {x} Nothing q =
  jscall
    "window.onresize = undefined"
    (JS_IO ())
setOnResizeOpt g {x} (Just h) q =
  let z = h x
  in jscall
        "window.onresize = %0"
        (JsFn (() -> JS_IO ()) -> JS_IO ())
        (MkJsFn $ \() => putInQueue z q)

setOpts : DomOptionRec a f g -> JSIOFifoQueue (g x) -> JS_IO ()
setOpts {g} x q = setOnResizeOpt g (onresizeOption x) q

initBodyRaw : List (DomOption a f g) -> ((x:a) -> f x -> Html (g x) ) -> f z -> JS_IO (GuiRefData a f g z)
initBodyRaw {a} {f} {g} {z} opts render st0 =
  do
    let n0 = render z st0
    let opts' = domOptionsToRec opts
    queue <- newJSIOFifoQueue (g z)
    setOpts opts' queue
    initialyzeBody queue n0
    pure $ MkGuiRefData render queue st0 n0


export
implementation Dom ASync where
  DomRef a f g z =
    State (GuiRefData a f g z)
  initBody render a0 start =
    do
      guiR <- lift $ liftJS_IO $ initBodyRaw [] render start
      r <- new guiR
      pure r
  clearDom v =
    do
      (MkGuiRefData _ _ _ n) <- read v
      lift $ liftJS_IO $ clearNode n
      delete v

  domPut v {x} newSt =
    do
      (MkGuiRefData render queue st n) <- read v
      lift $ liftJS_IO $ updateNode n (render x newSt)
      write v $ MkGuiRefData render queue newSt n
      pure ()
  domPutM {g} v x newSt =
    do
      (MkGuiRefData render queue st n) <- read v
      queue <- lift $ liftJS_IO $ newJSIOFifoQueue (g x)
      n' <- lift $ liftJS_IO $ updateNodeM queue n (render x newSt)
      write v $ MkGuiRefData render queue newSt n'
      pure ()
  domGet v =
    do
      (MkGuiRefData render queue st n) <- read v
      pure st
  getInput v =
    do
      (MkGuiRefData render queue st n) <- read v
      r <- lift $ getFromQueue queue
      pure r

{-
export
implementation Handler Dom ASync where
  handle (MkDGuiRef render queue st n) (DomPutM x g newSt) k =
    do
      queue' <- liftJS_IO $ newJSIOFifoQueue (g x)
      n' <- liftJS_IO $ updateNodeM queue' n (render x newSt)
      k () (MkDGuiRef render queue' newSt n')
  handle (MkDGuiRef render queue st n) DomGet k =
    k st (MkDGuiRef render queue st n)
  handle (MkDGuiRef render queue st n) DomClear k =
    do
      k () ()
  handle (MkDGuiRef render queue st n) (DomPutAnimated opts x newSt) k =
    do
      liftJS_IO $ animateNode opts n (render x newSt)
      k () (MkDGuiRef render queue newSt n)
  handle r (ConsoleLog s) k = do liftJS_IO $ putStr' s; k () r
  handle r (Wait millis) k = do setTimeout (cast millis) (); k () r
  handle r InnerWidth k = do x <- liftJS_IO (jscall "window.innerWidth" (JS_IO Double)) ; k x r
  handle r InnerHeight k = do x <- liftJS_IO (jscall "window.innerHeight" (JS_IO Double)) ; k x r
  handle r AvailableWidth k =
    do
      x <- liftJS_IO availableWidth_
      k x r
  handle r AvailableHeight k =
    do
      x <- liftJS_IO availableHeight_
      k x r


export
domPutAnimated : List AnimationOption -> f z -> Eff () [DOM (DGuiRef a f g z)]
domPutAnimated {z} opts x = call $ DomPutAnimated opts z x

export
domPutM : f x -> Eff () [DOM (DGuiRef a f g z)] [DOM (DGuiRef a f g x)]
domPutM {x} {g} y = call $ DomPutM x g y

export
domGet : Eff (f z) [DOM (DGuiRef a f g z)]
domGet = call DomGet

export
domUpdate : (f z -> f z) -> Eff () [DOM (DGuiRef a f g z)]
domUpdate h =
  do
    y <- domGet
    domPut $ h y

export
domUpdateAnimated : List AnimationOption -> (f z -> f z) -> Eff () [DOM (DGuiRef a f g z)]
domUpdateAnimated opts h =
  do
    y <- domGet
    domPutAnimated opts $ h y

export
domUpdateM : (f z -> f x) -> Eff () [DOM (DGuiRef a f g z)] [DOM (DGuiRef a f g x)]
domUpdateM {x} h =
  do
    y <- domGet
    domPutM $ h y

export
domClear : Eff () [DOM (DGuiRef a f g z)] [DOM ()]
domClear = call DomClear

export
consoleLog : String -> Eff () [DOM a]
consoleLog s = call $ ConsoleLog s


export
innerWidth : Eff Double [DOM a]
innerWidth = call InnerWidth

export
innerHeight : Eff Double [DOM a]
innerHeight = call InnerHeight

export
availableWidth : Eff Double [DOM a]
availableWidth = call AvailableWidth

export
availableHeight : Eff Double [DOM a]
availableHeight = call AvailableHeight
-}
{-
export
DomRef : Dom m  =>  Type -> Type -> Type
DomRef {m} a b = DDomRef {m} () (const a) (const b) ()
-}
-- Utils -------------

export
getLocationSearch : JS_IO String
getLocationSearch = jscall "decodeURI(window.location.search)" (() -> JS_IO String) ()
