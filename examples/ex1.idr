
module Main

import Js.Dom
import Control.ST

data Input = Set
           | Change String

Gui : Dom m => Type
Gui {m} = DomRef {m} () (const String) (const Input) ()

vw : () -> String -> Html Input
vw () s = div [] [form [] Set [input [onchange Change]], div [] [text s]]

procInput : Dom m => (d: Var) -> (s:Var) -> Input -> ST m () [s:::State String, d:::Gui {m}]
procInput d s Set =
  do
    inp <- read s
    call $ domPut d inp
procInput d s (Change new) =
    write s new

pageLoop : Dom m => (d: Var) -> (s:Var) -> ST m () [s:::State String, d:::Gui {m}]
pageLoop d s =
  do
    x <- call $ getInput d
    procInput d s x
    pageLoop d s


page : Dom m => ST m () []
page =
  do
    dom <- initBody [] vw () "ola"
    txt <- new ""
    pageLoop dom txt
    delete txt
    clearDom dom

main : JS_IO ()
main = setASync_ $ run page
