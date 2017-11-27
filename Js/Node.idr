module Js.Node

{-
import Effects
import public Js.ASync
import public Js.ServiceTypes
import Data.SortedMap


require : String -> JS_IO Ptr
require s = jscall "require(%0)" (String -> JS_IO Ptr) s

export
err2Either : ASync Ptr -> ASync (Either String Ptr)
err2Either x =
  do
    y <- x
    errQ <- liftJS_IO $ jscall "%0[0] ? 1 : 0" (Ptr -> JS_IO Int) y
    if errQ == 1 then
      do
        msg <- liftJS_IO $ jscall "%0[0] + ''" (Ptr -> JS_IO String) y
        pure $ Left msg
      else
        do
          res <- liftJS_IO $ jscall "%0[1]" (Ptr -> JS_IO Ptr) y
          pure $ Right res

export
handleErr : ASync Ptr -> ASync Ptr
handleErr x  =
  do
    z <- err2Either x
    case z of
      Left e => debugError e
      Right res => pure res

export
readFileJS_IO : String -> JS_IO String
readFileJS_IO file =
  do
    fs <- require "fs"
    jscall "%0.readFileSync(%1)" (Ptr -> String -> JS_IO String) fs file

export
readFileASync : String -> ASync String
readFileASync f =
  do
    map (\x => the String (believe_me x)) $ handleErr $ MkASync $ \proc =>
      do
        jscall
          "require('fs').readFile(%0, 'utf8', function(e,c){%1([e,c])} )"
          (String -> JsFn (Ptr -> JS_IO ()) -> JS_IO () )
          f
          (MkJsFn $ proc)


export
data FileIO : Effect where
  ReadFile : String -> sig FileIO String

export
implementation Handler FileIO ASync where
  handle () (ReadFile s) k = do x <- readFileASync s; k x ()

public export
FILEIO : EFFECT
FILEIO = MkEff () FileIO

export
readFile : String -> Eff String [FILEIO]
readFile s = call $ ReadFile s



export
data RequestState = MkRequestState Ptr Ptr

export
data WSState = MkWSState Ptr (List String)

export
data WebSocket : Effect where
  WSSend : String -> sig WebSocket () WSState WSState
  WSReceive : sig WebSocket String WSState WSState

public export
WEBSOCKET : (ty : Type) -> EFFECT
WEBSOCKET t = MkEff t WebSocket

export
data Request : Effect where
  EndRequest : String -> sig Request () RequestState ()

public export
REQUEST : (ty : Type) -> EFFECT
REQUEST t = MkEff t Request

export
implementation Handler Request ASync where
  handle (MkRequestState req resp) (EndRequest s) k = do liftJS_IO $ jscall "%0.end(%1)" (Ptr -> String -> JS_IO ()) resp s ; k () ()

procReqRaw : (RequestState -> ASync ()) -> Ptr -> JS_IO ()
procReqRaw handler x =
  do
    req <- jscall "%0[0]" (Ptr -> JS_IO Ptr) x
    resp <- jscall "%0[1]" (Ptr -> JS_IO Ptr) x
    setASync_ $  handler $ MkRequestState req resp

export
runServer : (XS : List EFFECT) -> Env ASync XS -> Int ->
                EffM ASync () (REQUEST RequestState :: XS) (\_ => REQUEST () :: XS) -> JS_IO ()
runServer xs inits port handler =
  do
    http <- require "http"
    server <- jscall
                "%0.createServer(function(req, res){return %1([req,res])})"
                (Ptr -> (JsFn (Ptr -> JS_IO ())) -> JS_IO Ptr )
                http
                (MkJsFn $ procReqRaw (\x => runInit (x::inits) handler) )
    jscall
      ("%0.listen(%1, function(err){" ++
       " if(err){" ++
       "  console.log('Error starting server',err)" ++
       " }else{" ++
       "  console.log('server is listnening on port', %1)" ++
       "}" ++
       "})")
      (Ptr -> Int -> JS_IO ()) server port


export
endRequest : String -> Eff () [REQUEST RequestState] [REQUEST ()]
endRequest s = call $ EndRequest s
-}
