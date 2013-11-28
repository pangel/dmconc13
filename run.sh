step() {
  echo $1 && echo $2 && eval $2 && echo
}

step "Tools" "javac tools.java" &&
step "Compiler" "ocamlc str.cma imp.ml cj.ml -o cj_compiler" &&
step "Compile Imp" "./cj_compiler $1 ${2+\"$2\"}" &&
step "Compile Java" "javac Compiled.java" &&
step "Run Java"  "java Compiled"
