Instructions to install and build system:
1. Install OPAM:
    - brew install opam
2. Initialize OPAM:
    - opam init eval $(opam env)
3. Install OCaml:
    - opam install ocaml
4. Install required packages:
    - opam update
    - opam install dune ounit2
5. Build the project:
    - dune build
6. Run the project:
    - dune exec bin/main.exe