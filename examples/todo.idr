
import Effects
import Js.Browser
import Js.Forms

data TodoAction : Nat -> Type where
  TodoAdd : String -> TodoAction n
  TodoRemove : Fin n -> TodoAction n

Gui : Nat -> Type
Gui = GuiRef Nat (\n=>Vect n String) TodoAction

vwTasks : Template Nat (\n=>Vect n String) Fin
vwTasks =
  vectOnDivIndex
    []
    (\x => x)
    (\_,x => x)
    (div [] $ [ button [onclickD (\_,(i,_) => i)] "x"
              , textD [] (\_,(_,x) => x)
              ]
    )

vw : Template Nat (\n => Vect n String) TodoAction
vw =
  div
    []
    [ bform [onsubmit (\_,_,x => TodoAdd x)] textform
    , (\n, pos => TodoRemove pos) <$> vwTasks
    ]


lenAfterAction : (n : Nat) -> TodoAction n -> Nat
lenAfterAction n (TodoAdd _) = S n
lenAfterAction (S n) (TodoRemove _) = n


procAct : (a:TodoAction n) -> Eff () [HTML (Gui n)] [HTML (Gui (lenAfterAction n a))]
procAct (TodoAdd s) =
  updateGuiM (s::)
procAct {n=S m} (TodoRemove i) =
  updateGuiM (deleteAt i)


pageLoop : Eff () [HTML (Gui m)] [HTML (Gui n)]
pageLoop =
  do
    x <- getInput
    procAct x
    pageLoop

page : Eff () [HTML ()] [HTML (Gui 0)]
page =
  do
    initBodyM [] [] vw
    pageLoop

main : JS_IO ()
main = setASync_ $ run page
