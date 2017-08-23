# idrisjs
Js libraries for idris.

The only current working part are the modules to gui as single pape web apps. 

### To build
```shell
idris --install js.ipkg
```

### Running the example:
```shell
cd examples
idris --codegen javascript -p js todo.idr -o todo.html
```
then open todo.html

### Documentation
The only documentation available right now is the idris generated doc
```shell
idris --mkdoc js.ipkg
```
Open a github issue to discuss anything related to this project, including questions on how to use the project, as they are probably due to lack of documentation.
