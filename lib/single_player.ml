type guess =
  | Give_up
  | Guess of string

let choose_random = function
  | [] -> invalid_arg "cannot choose from an empty list"
  | xs -> List.nth xs (Random.int (List.length xs))

let random_category categories = choose_random categories

let random_answer ~words_map ~category =
  choose_random (Hashtbl.find words_map category)

let normalize_guess input =
  let input = String.lowercase_ascii (String.trim input) in
  if input = "give up" then Give_up else Guess input

let is_correct ~answer guess =
  String.lowercase_ascii (String.trim guess) = String.lowercase_ascii answer

let unused_hints ~possible_hints ~previous_guesses =
  List.filter (fun h -> not (List.mem h previous_guesses)) possible_hints

let remove_hint_and_guess ~possible_hints ~hint ~guess =
  List.filter
    (fun h -> String.lowercase_ascii h <> guess && h <> hint)
    possible_hints
