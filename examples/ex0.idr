module Main

import Js.Dom
import Control.ST
import Control.ST.ImplicitCall


Gui : Dom m => Type
Gui {m} = DomRef {m} () (const String) (const String) ()

vw : () -> String -> Html String
vw () s = div [] [input [onchange id], div [] [text s]]


pageLoop : Dom m => (d: Var) -> (s:Var) -> ST m () [s:::State String, d:::Gui {m}]
pageLoop d s =
  do
    x <- getInput d
    domPut d x
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
