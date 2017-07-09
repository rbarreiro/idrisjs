
import Effects
import Js.Dom
import Data.Vect

data TodoAction : Nat -> Type where
  TodoAdd : TodoAction n
  TodoChangeInputString : String -> TodoAction n
  TodoRemove : Fin n -> TodoAction n

record TodoState (n : Nat) where
  constructor MkTodoState
  todos : Vect n String
  inputString : String

removeTodo : Fin (S n) -> TodoState (S n) -> TodoState n
removeTodo i (MkTodoState tds inps) = MkTodoState (deleteAt i tds) inps

addTodo : TodoState n -> TodoState (S n)
addTodo (MkTodoState tds inps) = MkTodoState (inps :: tds) ""

Gui : Nat -> Type
Gui = DGuiRef Nat TodoState TodoAction

vwTasks : Vect n String -> Html (TodoAction n)
vwTasks todos =
  let idxTodos = zip range todos
  in div [] (map renderTodo $ toList idxTodos)
  where
    renderTodo : (Fin n, String) -> Html (TodoAction n)
    renderTodo (idx, s) =
      div [] [button [onclick (TodoRemove idx)] "x", text s]

vw : TodoState n -> Html (TodoAction n)
vw st =
  div
    []
    [ form [] TodoAdd [input [onchange TodoChangeInputString, value $ inputString st]]
    , vwTasks $ todos st
    ]

total
lenAfterAction : (n : Nat) -> TodoAction n -> Nat
lenAfterAction n TodoAdd = S n
lenAfterAction (S n) (TodoRemove _) = n
lenAfterAction n (TodoChangeInputString _) = n


procAct : (a:TodoAction n) -> Effects.TransEff.Eff () [DOM (Gui n)] [DOM (Gui (lenAfterAction n a))]
procAct TodoAdd =
  domUpdateM addTodo
procAct {n=S m} (TodoRemove i) =
  domUpdateM (removeTodo i)
procAct (TodoChangeInputString s) =
  domUpdate (record {inputString = s})


pageLoop : Eff () [DOM (Gui m)] [DOM (Gui n)]
pageLoop =
  do
    x <- getInput
    procAct x
    pageLoop

page : Eff () [DOM ()] [DOM (Gui 0)]
page =
  do
    initBodyM vw (MkTodoState [] "")
    pageLoop

main : JS_IO ()
main = setASync_ $ run page
