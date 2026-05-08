# Installation

## Requirements
- OCaml >= 4.14
- opam
- dune >= 3.0

## Install dependencies
```bash
opam install dune
```

## Build
```bash
dune build
```

## Run
- Solo mode: `dune exec bin/main.exe`
- Server: `dune exec bin/server_main.exe -- 4000`
- Client: `dune exec bin/client_main.exe -- HOST PORT NAME`