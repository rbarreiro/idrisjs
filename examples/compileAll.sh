export IDRISJS_OPTIM=$1
export IDRISJS_DEBUG=$2
idris --codegen javascript -p effects -p js ex0.idr -o ex0.js
idris --codegen javascript -p effects -p js ex1.idr -o ex1.js
idris --codegen javascript -p effects -p js todo.idr -o todo.js
