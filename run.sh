tools="javac tools.java"
compiler="ocamlc imp.ml cj.ml -o cj_compiler"
compile_imp="./cj_compiler $1"
compile_java="javac Compiled.java"
run_java="java Compiled"

echo "Tools" &&
echo $tools &&
eval $tools &&
echo &&

echo "Compiler" &&
echo $compiler &&
eval $compiler &&
echo &&

echo "Compile Imp" &&
echo "$compile_imp" &&
eval $compile_imp &&
echo &&

echo "Compile Java" &&
echo $compile_java &&
eval $compile_java &&
echo &&

echo "Run Java" &&
echo $run_java &&
echo &&
eval $run_java
