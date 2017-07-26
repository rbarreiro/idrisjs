
import Control.ST
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

Gui : Dom m => Nat -> Type
Gui {m} = DomRef {m} Nat TodoState TodoAction

vwTasks : Vect n String -> Html (TodoAction n)
vwTasks todos =
  let idxTodos = zip range todos
  in div [] (map renderTodo $ toList idxTodos)
  where
    renderTodo : (Fin n, String) -> Html (TodoAction n)
    renderTodo (idx, s) =
      div [] [button [onclick (TodoRemove idx)] "x", text s]

vw : (n:Nat) -> TodoState n -> Html (TodoAction n)
vw n st =
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


procAct : Dom m => (d:Var) -> (a:TodoAction n) -> ST m () [d ::: Gui {m} n :-> Gui {m} (lenAfterAction n a)]
procAct {n} d TodoAdd =
  domUpdateM d n (S n) addTodo
procAct {n=S k} d (TodoRemove i) =
  domUpdateM d (S k) k (removeTodo i)
procAct d (TodoChangeInputString s) =
  domUpdate d (record {inputString = s})


pageLoop : Dom m => (d:Var) -> ST m () [remove d (Gui {m} n)]
pageLoop d =
  do
    x <- getInput d
    procAct d x
    pageLoop d

page : Dom m => ST m () []
page =
  do
    d <- initBody [] vw 0 (MkTodoState [] "")
    pageLoop d

main : JS_IO ()
main = setASync_ $ run page
