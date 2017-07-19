export IDRISJS_OPTIM=$1
export IDRISJS_DEBUG=$2
idris --codegen javascript -p contrib -p js ex0.idr -o ex0.html
idris --codegen javascript -p contrib -p js ex1.idr -o ex1.html
idris --codegen javascript -p contrib -p js todo.idr -o todo.html
