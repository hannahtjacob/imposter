(** Shared ANSI terminal formatting helpers. *)

val bold : string -> string
(** [bold s] wraps [s] in ANSI bold escape codes. *)

val cyan : string -> string
(** [cyan s] wraps [s] in ANSI cyan color escape codes. *)

val green : string -> string
(** [green s] wraps [s] in ANSI green color escape codes. *)

val red : string -> string
(** [red s] wraps [s] in ANSI red color escape codes. *)

val yellow : string -> string
(** [yellow s] wraps [s] in ANSI yellow color escape codes. *)

val dim : string -> string
(** [dim s] wraps [s] in ANSI dim/faint escape codes. *)

val line : unit -> unit
(** [line ()] prints a dim horizontal separator line followed by a newline. *)
