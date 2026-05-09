(* Shared ANSI terminal formatting helpers used by both the single-player
   io_loop and the networked client. *)

let bold s = "\027[1m" ^ s ^ "\027[0m"
let cyan s = "\027[36m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"
let dim s = "\027[2m" ^ s ^ "\027[0m"

let line () = print_endline (dim "────────────────────────────────────────")
