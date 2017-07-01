module Dom

import Effects
import public Js.Html
import public Js.VirtualDom
import public Js.ASync

export
data DGuiRef : (a : Type) -> (f : a -> Type) -> (g : a -> Type) -> a -> Type where
  MkDGuiRef : ((z:a) -> f z -> Html (g z)) -> JSIOFifoQueue (g x) -> f x -> Html (g x) -> DGuiRef a f g x

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


export
data Dom : Effect where
  InitBody : List (DomOption () (const a) (const b)) -> (a -> Html b) -> a -> sig Dom () () (DGuiRef () (const a) (const b) ())
  InitBodyM : ((x:a)-> f x -> Html (g x)) -> f z -> sig Dom () () (DGuiRef a f g z)
  GetInput : sig Dom (g x) (DGuiRef a f g x)
  DomPut : (x:a) -> f x -> sig Dom () (DGuiRef a f g x)
  DomPutM : (x:a) -> (g : a->Type) -> f x -> sig Dom () (DGuiRef a f g z) (DGuiRef a f g x)
  DomGet : sig Dom (f z) (DGuiRef a f g z)
  DomClear : sig Dom () (DGuiRef a f g z) ()
  DomPutAnimated : List AnimationOption -> (x:a) -> f x -> sig Dom () (DGuiRef a f g x)
  ConsoleLog : String -> sig Dom () a
  Wait : Nat -> sig Dom () a
  InnerWidth : sig Dom Double a
  InnerHeight : sig Dom Double a
  AvailableWidth : sig Dom Double a
  AvailableHeight : sig Dom Double a

public export
DOM : (ty : Type) -> EFFECT
DOM t = MkEff t Dom

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

initBodyRaw : List (DomOption a f g) -> ((x:a) -> f x -> Html (g x) ) -> f z -> JS_IO (DGuiRef a f g z)
initBodyRaw {a} {f} {g} {z} opts render st0 =
  do
    let n0 = render z st0
    let opts' = domOptionsToRec opts
    queue <- newJSIOFifoQueue (g z)
    setOpts opts' queue
    initialyzeBody queue n0
    pure $ MkDGuiRef render queue st0 n0

export
implementation Handler Dom ASync where
  handle () (InitBody opts render st0) k =
    do
      guiR <- liftJS_IO $ initBodyRaw opts (\() => render) st0
      k () guiR
  handle () (InitBodyM render st0) k =
    do
      guiR <- liftJS_IO $ initBodyRaw [] render st0
      k () guiR
  handle (MkDGuiRef render queue st n) GetInput k =
    do
      r <- getFromQueue queue
      k r (MkDGuiRef render queue st n)
  handle (MkDGuiRef render queue st n) (DomPut x newSt) k =
    do
      liftJS_IO $ updateNode n (render x newSt)
      k () (MkDGuiRef render queue newSt n)
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
initBody : List (DomOption () (const b) (const c)) -> (b -> Html c) -> b -> Eff () [DOM ()] [DOM (DGuiRef () (const b) (const c) ())]
initBody opts h s = call $ InitBody opts h s

export
initBodyM : ({x:a} -> f x -> Html (g x)) -> f z -> Eff () [DOM ()] [DOM (DGuiRef a f g z)]
initBodyM h s = call $ InitBodyM (\y, w => h {x=y} w) s

export
getInput : Eff (g z) [DOM (DGuiRef a f g z)]
getInput = call GetInput

export
domPut : f z -> Eff () [DOM (DGuiRef a f g z)]
domPut {z} x = call $ DomPut z x

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
wait : Nat -> Eff () [DOM a]
wait millis = call $ Wait millis

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

namespace Simple

  public export
  GuiRef : Type -> Type -> Type
  GuiRef a b = DGuiRef () (const a) (const b) ()

-- Utils -------------

export
getLocationSearch : JS_IO String
getLocationSearch = jscall "decodeURI(window.location.search)" (() -> JS_IO String) ()
