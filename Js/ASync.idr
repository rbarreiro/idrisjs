module Js.ASync

import Control.ST
import public Js.Utils

%include JavaScript "js/async.js"
%include Node "js/async.js"

public export
data ASync : Type -> Type where
  MkASync : ((a -> JS_IO()) -> JS_IO ()) -> ASync a

export
setASync : (a -> JS_IO ()) -> ASync a -> JS_IO ()
setASync onEvent (MkASync set) =
  do
    set onEvent
    pure ()

export
setASync_ : ASync a -> JS_IO ()
setASync_ x = setASync (\_=>pure ()) x

export
total
setTimeout : Int -> a -> ASync a
setTimeout millis x =
  MkASync $ \onevt => assert_total $
    jscall  "setTimeout(%0, %1)" ( JsFn (() -> JS_IO ()) -> Int -> JS_IO ()) (MkJsFn $ \() => onevt x) millis

export
never : ASync a
never = MkASync (\onevt => pure ())

export
debugError : String -> ASync a
debugError s = MkASync (\_ => jscall "throw2(%0)" (String -> JS_IO ()) s)

export
liftJS_IO : JS_IO a -> ASync a
liftJS_IO x = MkASync (\onevt => x >>= onevt)

export
Functor ASync where
  map f (MkASync oldset) = MkASync (\onevt => oldset (\x => onevt (f x)) )

export
Applicative ASync where
  pure x = setTimeout 0 x
  (MkASync stepf) <*> (MkASync stepx) =
    MkASync (\onevt => stepf (\f => stepx (\x => onevt (f x)) ))

export
Monad ASync where
  (>>=) (MkASync stepx) f =
    MkASync $ \onevt => stepx (\x => let MkASync stepf = f x in stepf onevt )


export
data JSIOFifoQueue : (b : Type) -> Type where
  MkJSIOFifoQueue : Ptr -> JSIOFifoQueue a

export
newJSIOFifoQueue : (x : Type) -> JS_IO (JSIOFifoQueue x)
newJSIOFifoQueue _ =
  MkJSIOFifoQueue <$> jscall "{queue: [], callback: undefined}" (JS_IO Ptr)

export
putInQueue : a -> JSIOFifoQueue a -> JS_IO ()
putInQueue x (MkJSIOFifoQueue q) =
  jscall
    "$JSLIB$async.putInQueue(%0,%1)"
    (Ptr -> Ptr -> JS_IO ())
    (believe_me x)
    q

export
getQueuePtr : JSIOFifoQueue a -> Ptr
getQueuePtr (MkJSIOFifoQueue q) = q

export
getFromQueue : JSIOFifoQueue a -> ASync a
getFromQueue (MkJSIOFifoQueue q) =
  MkASync $ \proc =>
    believe_me <$>
      jscall
        "$JSLIB$async.getFromQueue(%0, %1)"
        (Ptr -> JsFn (Ptr -> JS_IO ()) -> JS_IO Ptr)
        q
        (MkJsFn (\x => proc $ believe_me x) )

export
implementation Console ASync where
  consoleLog s = lift $ liftJS_IO $ jscall "console.log(%0)" (String -> JS_IO ()) s


public export
interface Wait (m:Type -> Type) where
    wait : Int -> STrans m () xs (const xs)

export
implementation Wait ASync where
  wait millis = lift $ setTimeout millis ()

export
implementation JSRandom ASync where
  randInt i = lift $ liftJS_IO $ randomInt i
