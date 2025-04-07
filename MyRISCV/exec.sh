#!/bin/bash
clear
echo "Dune project clean and build start"
cd ./lib
rm -rf ./lexer.ml ./parser.ml ./parser.mli
ocamllex ./lexer.mll
ocamlyacc ./parser.mly
cd ../
dune clean
dune build
dune exec MyRISCV

if [ $? = 0 ]; then
	echo "shell complete success"
	exit 0
else
	echo "error"
	exit 1
fi

