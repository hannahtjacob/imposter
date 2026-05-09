type guess =
  | Give_up
  | Guess of string  (** A normalized player guess. *)

val random_category : string list -> string
(** [random_category categories] returns a random category from [categories].
    Raises [Invalid_argument] if [categories] is empty. *)

val random_answer : words_map:Game.words_map -> category:string -> string
(** [random_answer ~words_map ~category] returns a random answer for [category].
*)

val normalize_guess : string -> guess
(** [normalize_guess input] trims and lowercases player input. ["give up"]
    becomes [Give_up]; all other input becomes [Guess input]. *)

val is_correct : answer:string -> string -> bool
(** [is_correct ~answer guess] is true when [guess] matches [answer], ignoring
    surrounding whitespace and ASCII case. *)

val unused_hints :
  possible_hints:string list -> previous_guesses:string list -> string list
(** [unused_hints ~possible_hints ~previous_guesses] removes hints already
    guessed by the player. *)

val remove_hint_and_guess :
  possible_hints:string list -> hint:string -> guess:string -> string list
(** [remove_hint_and_guess ~possible_hints ~hint ~guess] removes the displayed
    [hint] and the player's [guess] from [possible_hints], ignoring ASCII case
    for [guess]. *)
