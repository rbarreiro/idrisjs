module Main

import Js.Dom
import Js.HtmlEvents
import Effects
import Effect.State

data Input = Set
          |  Change String

Gui : Type
Gui = GuiRef String Input

vw : String -> Html Input
vw s = div [] [form [] Set [input [onChange Change]], div [] [text s]]

pageLoop : Effects.SimpleEff.Eff () [DOM Gui, STATE String, CONSOLE]
pageLoop =
  do
    x <- getInput
    case x of
      Set =>
        do
          s <- get
          domPut s
      Change s =>
        put s
    pageLoop

page : Eff () [DOM (), STATE String, CONSOLE] [DOM Gui, STATE String, CONSOLE]
page =
  do
    initBody [] vw ""
    pageLoop

main : JS_IO ()
main = setASync_ $ run page
