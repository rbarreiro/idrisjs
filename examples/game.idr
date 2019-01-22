
module Main

import Js.Dom
import public Js.ASync
import Control.ST
import Js.HtmlStyle
import Data.AVL.Set

data Input 
  = Tick
  | KeyDown String
  | KeyUp String

record Model where
  constructor MkModel
  scroll : List String
  n : Int
  keymap : Set String
  hero : (Double, Double)

emptyModel : Model
emptyModel =
  MkModel
    ["ola"]
    0
    Data.AVL.Set.empty
    (320,240)

add_scroll : String -> Model -> Model
add_scroll s model =
  let
    lines = scroll model
  in
  set_scroll (s :: lines) model

record Circle where
  constructor MkCircle
  center_x : Double
  center_y : Double
  radius : Double
  color : String

Gui : Dom m => Type
Gui {m} = DomRef {m} () (const Model) (const Input) ()

interface Drawable a where
  draw : a -> Html Input
  
implementation Drawable Circle where
  draw c =
    div
      [ style 
          [ position (Fixed (center_x c) (center_y c))
          , width (2.0 * (radius c))
          , height (2.0 * (radius c))
          , backgroundColor (color c)
          ]
      ] []
      
drawables : Model -> List (Html Input)
drawables model =
  let
    (hx,hy) = hero model
  in
  [ draw (MkCircle hx hy 5.0 "red") ]

vw : () -> Model -> Html Input
vw () inp = 
  div 
    [ style 
        [ position (Fixed 0 0)
        , width 640
        , height 480
        ] 
    , onkeydown (\k => KeyDown (key k))
    , onkeyup (\k => KeyUp (key k))
    , tabindex 0
    ]
    ([ div [] (map (\t => div [] [text t]) (scroll inp))
     ] ++ (drawables inp)
    )

handleKeys : Model -> Model
handleKeys model =
  let
    (hx,hy) = hero model 
    km = keymap model
  in
  if Data.AVL.Set.contains "a" km then
    set_hero (hx - 3, hy) model
  else if Data.AVL.Set.contains "s" km then
    set_hero (hx, hy + 3) model
  else if Data.AVL.Set.contains "d" km then
    set_hero (hx + 3, hy) model
  else if Data.AVL.Set.contains "w" km then
    set_hero (hx, hy - 3) model
  else
    model

handleInput : Dom m => (d: Var) -> (s: Var) -> Input -> ST m () [s:::State Model, d:::Gui {m}]
handleInput d s x =
  case x of
    KeyDown k =>
      do
        inp <- read s
        let km = keymap inp
        write s (add_scroll ("down " ++ k) (set_keymap (Data.AVL.Set.insert k km) inp))
        
    KeyUp k =>
      do
        inp <- read s
        let km = keymap inp
        let turnoff = Data.AVL.Set.fromList [ k ]
        write s (add_scroll ("up " ++ k) (set_keymap (Data.AVL.Set.difference km turnoff) inp))
        
    Tick => 
      do
        inp <- read s
        write s (handleKeys (set_n ((n inp) + 1) inp))
        call $ schedule d 30 Tick
        call $ domPut d inp

pageLoop : Dom m => (d: Var) -> (s:Var) -> ST m () [s:::State Model, d:::Gui {m}]
pageLoop d s =
  do
    x <- call $ getInput d

    handleInput d s x

    pageLoop d s
    
pageStart : Dom m => (d: Var) -> (s:Var) -> ST m () [s:::State Model, d:::Gui {m}]
pageStart d s =
  do
    call $ schedule d 30 Tick
    pageLoop d s

page : Dom m => ST m () []
page =
  do
    let model = emptyModel
    dom <- initBody [] vw () model
    txt <- new model
    pageStart dom txt
    delete txt
    clearDom dom

main : JS_IO ()
main = setASync_ $ run page
