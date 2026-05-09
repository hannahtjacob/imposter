(** Wire protocol for Imposter networked game.

    Messages are line-delimited JSON. Each message has a [type] field
    identifying it; remaining fields are message-specific. *)

(** Messages sent from a client to the server. *)
type client_msg =
  | Join of string  (** Player joins lobby with this display name. *)
  | Start  (** Host requests to start the game. *)
  | Clue of string  (** Player submits a one-word clue. *)
  | Vote of string  (** Player votes for the named player. *)
  | ImposterGuess of string
      (** After being correctly accused, imposter guesses the secret word. *)
  | PlayAgain of bool  (** Vote to play another round. *)

(** Messages broadcast or sent privately by the server. *)
type score_entry = {
  player : string;
  crew_wins : int;
  imposter_wins : int;
  times_caught : int;
  rounds_played : int;
}
(** Per-player statistics accumulated over the session. *)

type server_msg =
  | Welcome of string  (** Confirms join; payload is the assigned name. *)
  | LobbyUpdate of string list  (** Current lobby roster. *)
  | Error of string  (** Soft error (bad input, not fatal). *)
  | RoundStart of {
      category : string;
      role : [ `Imposter | `Crew ];
      word : string option;  (** [Some w] for crew, [None] for imposter. *)
      players : string list;
      clue_order : string list;
    }
  | YourTurnClue  (** Server prompts this client for a clue. *)
  | CluePosted of {
      player : string;
      clue : string;
    }
  | YourTurnVote of { candidates : string list }
      (** Server prompts this client to vote; [candidates] is the full player
          list. *)
  | VotePosted of {
      voter : string;
      voted_for : string;
    }  (** Broadcast when any player casts a vote. *)
  | Accused of {
      player : string;
      was_imposter : bool;
    }
      (** Broadcast after voting; names the player with the most votes and
          whether they were actually the imposter. *)
  | YourTurnGuess of { hint : string }
      (** Imposter was correctly accused; now gets to guess the word. *)
  | RoundEnd of {
      winner : [ `Crew | `Imposter | `Draw ];
      imposter : string;
      word : string;
      reason : string;
    }  (** Broadcast at the end of every round with the outcome. *)
  | ScoreUpdate of score_entry list
      (** Full session scoreboard, sent after every RoundEnd. *)
  | YourTurnPlayAgain
      (** Server asks this client whether they want to play another round. *)
  | ServerShutdown of string
      (** Server is closing; payload is a farewell message. *)

val encode_client : client_msg -> string
(** Encode a client message as a single line (no trailing newline). *)

val decode_client : string -> (client_msg, string) result
(** Parse one line into a client message. Returns [Error msg] on malformed
    input. *)

val encode_server : server_msg -> string
(** Encode a server message as a single line (no trailing newline). *)

val decode_server : string -> (server_msg, string) result
(** Parse one line into a server message. *)
