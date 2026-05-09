let () = Random.self_init ()

open Imposter

(* Layout *)
let blank () = print_newline ()

(* State *)
let categories = Game.load_categories ()
let words_map = Game.load_words ()
let current_category = ref ""
let current_answer = ref ""

(* Helpers *)
let get_category () =
  let cat = Single_player.random_category categories in
  current_category := cat;
  cat

let get_answer () =
  let ans =
    Single_player.random_answer ~words_map ~category:!current_category
  in
  current_answer := ans;
  ans

(* Display *)
let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

let print_title () =
  blank ();
  Display.line ();
  Printf.printf "  %s\n"
    (Display.bold (Display.red "             IMPOSTER      "));
  Display.line ();
  blank ()

let print_header category =
  Printf.printf "  %s  %s\n" (Display.bold "Category:")
    (Display.cyan (Display.bold category));
  Printf.printf "  %s\n"
    (Display.dim "You are the imposter. Guess the secret word using the hints.");
  blank ();
  Display.line ()

let print_hint_row n word =
  blank ();
  Printf.printf "  %s  %s\n"
    (Display.yellow (Printf.sprintf "[Hint %d]" n))
    (Display.bold word)

let print_prompt () =
  blank ();
  print_string (Display.dim "  Your guess (or 'give up'): ");
  flush stdout

let print_correct attempts =
  blank ();
  Display.line ();
  Printf.printf "  %s  Solved in %s hint(s)!\n"
    (Display.green (Display.bold "✓ Correct!"))
    (Display.bold (string_of_int attempts));
  Display.line ();
  blank ()

let print_wrong guess =
  blank ();
  Printf.printf "  %s  \"%s\" is not the word.\n" (Display.red "X")
    (Display.dim guess)

let print_gave_up answer =
  blank ();
  Display.line ();
  Printf.printf "  You gave up. The word was: %s\n"
    (Display.cyan (Display.bold answer));
  Display.line ();
  blank ()

let print_no_hints answer =
  blank ();
  Display.line ();
  Printf.printf "  Out of hints! The word was: %s\n"
    (Display.cyan (Display.bold answer));
  Display.line ();
  blank ()

let print_play_again () =
  blank ();
  print_string (Display.dim "  Play again? (y/n): ");
  flush stdout

(* Main game loop *)
let rec game_loop possible_hints previous_guesses answer attempts =
  let available_hints =
    Single_player.unused_hints ~possible_hints ~previous_guesses
  in
  match available_hints with
  | [] -> print_no_hints answer
  | _ -> (
      let idx = Random.int (List.length available_hints) in
      let hint = List.nth available_hints idx in
      print_hint_row (attempts + 1) hint;
      print_prompt ();
      match Single_player.normalize_guess (read_line ()) with
      | Give_up -> print_gave_up answer
      | Guess input ->
          if Single_player.is_correct ~answer input then
            print_correct (attempts + 1)
          else begin
            print_wrong input;
            let new_possible_hints =
              Single_player.remove_hint_and_guess ~possible_hints ~hint
                ~guess:input
            in
            let new_previous_guesses = input :: previous_guesses in
            game_loop new_possible_hints new_previous_guesses answer
              (attempts + 1)
          end)

let rec run () =
  clear_screen ();
  print_title ();
  let category = get_category () in
  let answer = get_answer () in
  let possible_hints = Game.get_hints words_map category answer in
  print_header category;
  game_loop possible_hints [] answer 0;
  print_play_again ();
  let again = String.lowercase_ascii (String.trim (read_line ())) in
  if again = "y" then begin
    run ()
  end
