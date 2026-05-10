open OUnit2

let pp_string_list xs = "[" ^ String.concat "; " xs ^ "]"
let pp_string s = s
let pp_int = string_of_int
let pp_char c = String.make 1 c

let pp_guess = function
  | Imposter.Single_player.Give_up -> "Give_up"
  | Imposter.Single_player.Guess s -> "Guess \"" ^ s ^ "\""

let pp_client_msg msg = Imposter.Protocol.encode_client msg
let pp_server_msg msg = Imposter.Protocol.encode_server msg

let pp_result pp = function
  | Ok v -> "Ok (" ^ pp v ^ ")"
  | Error e -> "Error \"" ^ e ^ "\""

let assert_ok_equal ?printer expected actual =
  match actual with
  | Ok value -> assert_equal ?printer expected value
  | Error msg -> assert_failure ("expected Ok, got Error: " ^ msg)

let assert_error actual =
  match actual with
  | Ok _ -> assert_failure "expected Error, got Ok"
  | Error _ -> ()

let make_words_map pairs =
  let map = Hashtbl.create (List.length pairs) in
  List.iter (fun (category, words) -> Hashtbl.add map category words) pairs;
  map

let test_load_categories _ =
  let categories = Imposter.Game.load_categories () in
  assert_equal ~printer:(fun x -> x) "Animals" (List.hd categories);
  assert_equal ~printer:string_of_int 50 (List.length categories);
  assert_bool "TV genres category present" (List.mem "TV genres" categories)

let test_load_words _ =
  let words_map = Imposter.Game.load_words () in
  let animals = Hashtbl.find words_map "Animals" in
  let fruits = Hashtbl.find words_map "Fruits" in
  assert_bool "lion present" (List.exists (( = ) "lion") animals);
  assert_bool "shark present" (List.exists (( = ) "shark") animals);
  assert_equal ~printer:string_of_int 9 (List.length animals);
  assert_bool "apple present" (List.exists (( = ) "apple") fruits)

let test_get_hints _ =
  let words_map = Imposter.Game.load_words () in
  Random.init 0;
  let hints = Imposter.Game.get_hints words_map "Animals" "lion" in
  assert_equal ~printer:string_of_int 8 (List.length hints);
  assert_bool "answer excluded" (not (List.exists (( = ) "lion") hints));
  assert_equal ~printer:pp_string_list
    (List.sort String.compare
       [
         "cobra";
         "dolphin";
         "elephant";
         "giraffe";
         "kangaroo";
         "penguin";
         "shark";
         "tiger";
       ])
    (List.sort String.compare hints)

let test_get_hints_edge_cases _ =
  let words_map =
    make_words_map
      [ ("Only", [ "solo" ]); ("Duplicates", [ "apple"; "apple"; "banana" ]) ]
  in
  Random.init 1;
  assert_equal ~printer:pp_string_list []
    (Imposter.Game.get_hints words_map "Only" "solo");
  assert_equal ~printer:pp_string_list [ "banana" ]
    (Imposter.Game.get_hints words_map "Duplicates" "apple");
  assert_raises Not_found (fun () ->
      ignore (Imposter.Game.get_hints words_map "Missing" "solo"))

let test_random_category _ =
  Random.init 7;
  let categories = [ "Animals"; "Fruits"; "Sports" ] in
  let category = Imposter.Single_player.random_category categories in
  assert_bool "category came from input" (List.mem category categories);
  assert_equal ~printer:pp_string "Only"
    (Imposter.Single_player.random_category [ "Only" ]);
  assert_raises (Invalid_argument "cannot choose from an empty list") (fun () ->
      ignore (Imposter.Single_player.random_category []))

let test_random_answer _ =
  Random.init 8;
  let words_map = make_words_map [ ("Animals", [ "lion"; "tiger" ]) ] in
  let answer =
    Imposter.Single_player.random_answer ~words_map ~category:"Animals"
  in
  assert_bool "answer came from category" (List.mem answer [ "lion"; "tiger" ]);
  assert_equal ~printer:pp_string "solo"
    (Imposter.Single_player.random_answer
       ~words_map:(make_words_map [ ("Only", [ "solo" ]) ])
       ~category:"Only");
  assert_raises Not_found (fun () ->
      ignore
        (Imposter.Single_player.random_answer ~words_map ~category:"Missing"))

let test_normalize_guess _ =
  assert_equal ~printer:pp_guess Imposter.Single_player.Give_up
    (Imposter.Single_player.normalize_guess "  GIVE UP  ");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "orange")
    (Imposter.Single_player.normalize_guess "  Orange  ");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "give  up")
    (Imposter.Single_player.normalize_guess "give  up");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "pear pie")
    (Imposter.Single_player.normalize_guess " Pear Pie ");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "")
    (Imposter.Single_player.normalize_guess "   ")

let test_is_correct _ =
  assert_bool "case-insensitive match"
    (Imposter.Single_player.is_correct ~answer:"Orange" " orange ");
  assert_bool "different word"
    (not (Imposter.Single_player.is_correct ~answer:"Orange" "apple"))

let test_hint_filtering _ =
  let hints = [ "apple"; "banana"; "orange" ] in
  assert_equal ~printer:pp_string_list hints
    (Imposter.Single_player.unused_hints ~possible_hints:hints
       ~previous_guesses:[]);
  assert_equal ~printer:pp_string_list []
    (Imposter.Single_player.unused_hints ~possible_hints:hints
       ~previous_guesses:hints);
  assert_equal ~printer:pp_string_list [ "apple"; "orange" ]
    (Imposter.Single_player.unused_hints ~possible_hints:hints
       ~previous_guesses:[ "banana" ]);
  assert_equal ~printer:pp_string_list [ "orange" ]
    (Imposter.Single_player.remove_hint_and_guess ~possible_hints:hints
       ~hint:"apple" ~guess:"banana");
  assert_equal ~printer:pp_string_list [ "apple"; "orange" ]
    (Imposter.Single_player.remove_hint_and_guess ~possible_hints:hints
       ~hint:"missing" ~guess:"banana")

let test_decode_client_manual_inputs _ =
  assert_ok_equal (Imposter.Protocol.Join "Ada")
    (Imposter.Protocol.decode_client
       " \t { \t \"name\" : \"Ada\" , \"type\" : \"join\" }");
  assert_ok_equal Imposter.Protocol.Start
    (Imposter.Protocol.decode_client
       "{\"ignored\":\"field\",\"type\":\"start\"}");
  assert_ok_equal (Imposter.Protocol.PlayAgain false)
    (Imposter.Protocol.decode_client "{\"yes\":false,\"type\":\"play_again\"}");
  assert_ok_equal (Imposter.Protocol.Clue "one")
    (Imposter.Protocol.decode_client
       "{\"type\":\"clue\",\"clue\":\"one\"} trailing text")

let test_decode_server_manual_inputs _ =
  assert_ok_equal
    (Imposter.Protocol.LobbyUpdate [ "Ada"; "Grace" ])
    (Imposter.Protocol.decode_server
       " { \"players\" : [ \"Ada\" , \"Grace\" ] , \"type\" : \"lobby\" }");
  assert_ok_equal
    (Imposter.Protocol.RoundStart
       {
         category = "Fruits";
         role = `Crew;
         word = Some "apple";
         players = [];
         clue_order = [];
       })
    (Imposter.Protocol.decode_server
       "{\"type\":\"round_start\",\"category\":\"Fruits\",\"role\":\"crew\",\"word\":\"apple\",\"players\":[],\"clue_order\":[]}");
  assert_ok_equal
    (Imposter.Protocol.ScoreUpdate
       [
         {
           player = "Ada";
           crew_wins = -1;
           imposter_wins = 2;
           times_caught = 0;
           rounds_played = 3;
         };
       ])
    (Imposter.Protocol.decode_server
       "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Ada\",\"cw0\":-1,\"iw0\":2,\"tc0\":0,\"rp0\":3}");
  assert_ok_equal (Imposter.Protocol.ServerShutdown "bye")
    (Imposter.Protocol.decode_server
       "{\"message\":\"bye\",\"type\":\"shutdown\"} ignored")

let client_messages =
  let open Imposter.Protocol in
  [
    Join "Ada";
    Start;
    Clue "river";
    Vote "Grace";
    ImposterGuess "delta";
    PlayAgain true;
    PlayAgain false;
  ]

let server_messages =
  let open Imposter.Protocol in
  [
    Welcome "Ada";
    LobbyUpdate [ "Ada"; "Grace" ];
    LobbyUpdate [];
    Error "try again";
    RoundStart
      {
        category = "Animals";
        role = `Crew;
        word = Some "lion";
        players = [ "Ada"; "Grace"; "Linus" ];
        clue_order = [ "Grace"; "Ada"; "Linus" ];
      };
    RoundStart
      {
        category = "Animals";
        role = `Imposter;
        word = None;
        players = [ "Ada"; "Grace"; "Linus" ];
        clue_order = [];
      };
    YourTurnClue;
    CluePosted { player = "Ada"; clue = "mane" };
    YourTurnVote { candidates = [ "Ada"; "Grace" ] };
    VotePosted { voter = "Grace"; voted_for = "Ada" };
    Accused { player = "Ada"; was_imposter = true };
    Accused { player = "Grace"; was_imposter = false };
    YourTurnGuess { hint = "mane" };
    RoundEnd
      { winner = `Crew; imposter = "Ada"; word = "lion"; reason = "caught" };
    RoundEnd
      {
        winner = `Imposter;
        imposter = "Grace";
        word = "lion";
        reason = "guessed";
      };
    RoundEnd
      { winner = `Draw; imposter = "Linus"; word = "lion"; reason = "tie" };
    ScoreUpdate
      [
        {
          player = "Ada";
          crew_wins = 1;
          imposter_wins = 0;
          times_caught = 1;
          rounds_played = 2;
        };
        {
          player = "Grace";
          crew_wins = 0;
          imposter_wins = 1;
          times_caught = 0;
          rounds_played = 2;
        };
      ];
    ScoreUpdate [];
    YourTurnPlayAgain;
    ServerShutdown "bye";
  ]

let test_client_protocol_round_trips _ =
  List.iter
    (fun msg ->
      let encoded = Imposter.Protocol.encode_client msg in
      assert_bool "client encoding has no newline"
        (not (String.contains encoded '\n'));
      assert_ok_equal ~printer:pp_client_msg msg
        (Imposter.Protocol.decode_client encoded))
    client_messages

let test_server_protocol_round_trips _ =
  List.iter
    (fun msg ->
      let encoded = Imposter.Protocol.encode_server msg in
      assert_bool "server encoding has no newline"
        (not (String.contains encoded '\n'));
      assert_ok_equal ~printer:pp_server_msg msg
        (Imposter.Protocol.decode_server encoded))
    server_messages

let test_protocol_sanitizes_encoded_strings _ =
  assert_equal ~printer:(pp_result pp_client_msg)
    (Ok (Imposter.Protocol.Join "A_B_C"))
    (Imposter.Protocol.decode_client
       (Imposter.Protocol.encode_client (Imposter.Protocol.Join "A\"B\\C")));
  assert_equal ~printer:(pp_result pp_client_msg)
    (Ok (Imposter.Protocol.Clue "two words"))
    (Imposter.Protocol.decode_client
       (Imposter.Protocol.encode_client (Imposter.Protocol.Clue "two\twords")));
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok (Imposter.Protocol.Error "bad input"))
    (Imposter.Protocol.decode_server
       (Imposter.Protocol.encode_server (Imposter.Protocol.Error "bad\ninput")))

let test_decode_client_errors _ =
  assert_error (Imposter.Protocol.decode_client "");
  assert_error (Imposter.Protocol.decode_client "{\"type\":\"wat\"}");
  assert_error (Imposter.Protocol.decode_client "{\"type\":\"join\"}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"play_again\",\"yes\":1}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"join\",\"name\":[\"Ada\"]}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"join\",\"name\":\"Ada\"");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"join\",\"name\":\"Ada\",}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"join\",\"name\":@}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"join\",\"name\":\"Ada}");
  assert_error
    (Imposter.Protocol.decode_client
       "{\"type\":\"join\",\"name\":\"Ada\",\"x\":-}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"play_again\",\"yes\":maybe}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"vote\",\"voted_for\":null}")

let test_decode_server_errors _ =
  assert_error (Imposter.Protocol.decode_server "");
  assert_error (Imposter.Protocol.decode_server "{\"type\":\"wat\"}");
  assert_error (Imposter.Protocol.decode_server "{\"type\":\"welcome\"}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"spy\",\"word\":null,\"players\":[],\"clue_order\":[]}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"crew\",\"word\":false,\"players\":[],\"clue_order\":[]}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"round_end\",\"winner\":\"nobody\",\"imposter\":\"Ada\",\"word\":\"lion\",\"reason\":\"bad\"}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"score_update\",\"count\":\"two\"}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"lobby\",\"players\":[\"Ada\",]}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"error\",\"message\":\"bad\",\"extra\":{}}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Ada\",\"cw0\":0,\"iw0\":0,\"tc0\":0}");
  assert_error
    (Imposter.Protocol.decode_server "{\"type\":\"accused\",\"player\":\"Ada\"}")

let expected_categories =
  [
    "Animals";
    "Fruits";
    "Vegetables";
    "Furniture";
    "Kitchen tools";
    "Musical instruments";
    "Planets";
    "Sports";
    "Colors";
    "Clothing";
    "Jobs";
    "Countries";
    "Weather";
    "Desserts";
    "Transport";
    "Emotions";
    "Superheroes";
    "Ocean creatures";
    "Flowers";
    "Spices";
    "Board games";
    "Mythical creatures";
    "Drinks";
    "Languages";
    "Artworks";
    "Shapes";
    "Card games";
    "Dances";
    "Trees";
    "Cheeses";
    "Reptiles";
    "Gems";
    "Currencies";
    "Cooking methods";
    "Phobias";
    "Fairy tale characters";
    "Body parts";
    "Mountains";
    "Photography terms";
    "Cocktails";
    "Pasta types";
    "Planets' moons";
    "Fabric types";
    "Philosophers";
    "Hairstyles";
    "Cleaning products";
    "Punctuation";
    "Architecture styles";
    "Birds";
    "TV genres";
  ]

let test_load_categories_exact_file_order _ =
  assert_equal ~printer:pp_string_list expected_categories
    (Imposter.Game.load_categories ())

let assert_words_equal expected words_map category =
  assert_equal ~printer:pp_string_list expected
    (Hashtbl.find words_map category)

let test_load_words_exact_representative_categories _ =
  let words_map = Imposter.Game.load_words () in
  assert_words_equal
    [
      "lion";
      "elephant";
      "penguin";
      "dolphin";
      "cobra";
      "tiger";
      "giraffe";
      "kangaroo";
      "shark";
    ]
    words_map "Animals";
  assert_words_equal
    [
      "mango";
      "lychee";
      "papaya";
      "fig";
      "guava";
      "apple";
      "banana";
      "orange";
      "grape";
    ]
    words_map "Fruits";
  assert_words_equal
    [
      "Venus";
      "Neptune";
      "Mercury";
      "Uranus";
      "Saturn";
      "Earth";
      "Mars";
      "Jupiter";
      "Pluto";
    ]
    words_map "Planets";
  assert_words_equal
    [
      "semicolon";
      "em dash";
      "ellipsis";
      "tilde";
      "pilcrow";
      "period";
      "comma";
      "question mark";
      "exclamation point";
    ]
    words_map "Punctuation"

let test_load_words_matches_category_file _ =
  let categories = Imposter.Game.load_categories () in
  let words_map = Imposter.Game.load_words () in
  assert_equal ~printer:string_of_int 50 (Hashtbl.length words_map);
  List.iter
    (fun category ->
      assert_bool
        ("missing words for " ^ category)
        (Hashtbl.mem words_map category);
      assert_equal
        ~msg:("word count for " ^ category)
        ~printer:string_of_int 9
        (List.length (Hashtbl.find words_map category)))
    categories

let test_get_hints_preserves_non_answer_duplicates _ =
  let words_map =
    make_words_map [ ("Food", [ "pie"; "cake"; "cake"; "tart"; "pie" ]) ]
  in
  Random.init 3;
  let hints = Imposter.Game.get_hints words_map "Food" "pie" in
  assert_equal ~printer:string_of_int 3 (List.length hints);
  assert_equal ~printer:pp_string_list [ "cake"; "cake"; "tart" ]
    (List.sort String.compare hints)

let test_get_hints_missing_answer_keeps_every_word _ =
  let words_map = make_words_map [ ("Letters", [ "a"; "b"; "c" ]) ] in
  Random.init 4;
  let hints = Imposter.Game.get_hints words_map "Letters" "z" in
  assert_equal ~printer:pp_string_list [ "a"; "b"; "c" ]
    (List.sort String.compare hints)

let test_choose_random_singleton_paths _ =
  let words_map =
    make_words_map [ ("Only category", [ "only answer" ]); ("Empty", []) ]
  in
  assert_equal ~printer:pp_string "Only category"
    (Imposter.Single_player.random_category [ "Only category" ]);
  assert_equal ~printer:pp_string "only answer"
    (Imposter.Single_player.random_answer ~words_map ~category:"Only category");
  assert_raises (Invalid_argument "cannot choose from an empty list") (fun () ->
      ignore (Imposter.Single_player.random_answer ~words_map ~category:"Empty"))

let test_normalize_guess_ascii_only_case_folding _ =
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "resume")
    (Imposter.Single_player.normalize_guess " ReSuMe ");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "give up!")
    (Imposter.Single_player.normalize_guess " GIVE UP! ");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "give up please")
    (Imposter.Single_player.normalize_guess "give up please");
  assert_equal ~printer:pp_guess (Imposter.Single_player.Guess "two\twords")
    (Imposter.Single_player.normalize_guess " Two\tWords ")

let test_is_correct_trims_guess_but_not_answer _ =
  assert_bool "answer is compared case-insensitively"
    (Imposter.Single_player.is_correct ~answer:"LiOn" " lion ");
  assert_bool "answer whitespace is significant"
    (not (Imposter.Single_player.is_correct ~answer:" lion " "lion"));
  assert_bool "empty answer can match trimmed empty guess"
    (Imposter.Single_player.is_correct ~answer:"" "   ")

let test_unused_hints_uses_exact_string_membership _ =
  let hints = [ "Apple"; "apple"; "banana"; "apple" ] in
  assert_equal ~printer:pp_string_list [ "Apple"; "banana" ]
    (Imposter.Single_player.unused_hints ~possible_hints:hints
       ~previous_guesses:[ "apple" ]);
  assert_equal ~printer:pp_string_list hints
    (Imposter.Single_player.unused_hints ~possible_hints:hints
       ~previous_guesses:[ "APPLE" ]);
  assert_equal ~printer:pp_string_list [ "banana" ]
    (Imposter.Single_player.unused_hints ~possible_hints:hints
       ~previous_guesses:[ "Apple"; "apple" ])

let test_remove_hint_and_guess_case_behavior _ =
  let hints = [ "Apple"; "apple"; "banana"; "BANANA"; "pear" ] in
  assert_equal ~printer:pp_string_list [ "apple"; "pear" ]
    (Imposter.Single_player.remove_hint_and_guess ~possible_hints:hints
       ~hint:"Apple" ~guess:"banana");
  assert_equal ~printer:pp_string_list
    [ "apple"; "banana"; "BANANA"; "pear" ]
    (Imposter.Single_player.remove_hint_and_guess ~possible_hints:hints
       ~hint:"Apple" ~guess:"APPLE");
  assert_equal ~printer:pp_string_list
    [ "banana"; "BANANA"; "pear" ]
    (Imposter.Single_player.remove_hint_and_guess ~possible_hints:hints
       ~hint:"missing" ~guess:"apple")

let test_encode_client_exact_shapes _ =
  let cases =
    [
      (Imposter.Protocol.Join "Ada", "{\"type\":\"join\",\"name\":\"Ada\"}");
      (Imposter.Protocol.Start, "{\"type\":\"start\"}");
      (Imposter.Protocol.Clue "river", "{\"type\":\"clue\",\"clue\":\"river\"}");
      ( Imposter.Protocol.Vote "Grace",
        "{\"type\":\"vote\",\"voted_for\":\"Grace\"}" );
      ( Imposter.Protocol.ImposterGuess "apple",
        "{\"type\":\"imposter_guess\",\"guess\":\"apple\"}" );
      ( Imposter.Protocol.PlayAgain true,
        "{\"type\":\"play_again\",\"yes\":true}" );
      ( Imposter.Protocol.PlayAgain false,
        "{\"type\":\"play_again\",\"yes\":false}" );
    ]
  in
  List.iter
    (fun (msg, encoded) ->
      assert_equal ~printer:pp_string encoded
        (Imposter.Protocol.encode_client msg);
      assert_equal ~printer:(pp_result pp_client_msg) (Ok msg)
        (Imposter.Protocol.decode_client encoded))
    cases

let test_encode_server_exact_simple_shapes _ =
  let cases =
    [
      ( Imposter.Protocol.Welcome "Ada",
        "{\"type\":\"welcome\",\"name\":\"Ada\"}" );
      ( Imposter.Protocol.LobbyUpdate [ "Ada"; "Grace" ],
        "{\"type\":\"lobby\",\"players\":[\"Ada\",\"Grace\"]}" );
      (Imposter.Protocol.LobbyUpdate [], "{\"type\":\"lobby\",\"players\":[]}");
      (Imposter.Protocol.Error "bad", "{\"type\":\"error\",\"message\":\"bad\"}");
      (Imposter.Protocol.YourTurnClue, "{\"type\":\"your_turn_clue\"}");
      ( Imposter.Protocol.CluePosted { player = "Ada"; clue = "mane" },
        "{\"type\":\"clue_posted\",\"player\":\"Ada\",\"clue\":\"mane\"}" );
      ( Imposter.Protocol.YourTurnVote { candidates = [ "Ada"; "Grace" ] },
        "{\"type\":\"your_turn_vote\",\"candidates\":[\"Ada\",\"Grace\"]}" );
      ( Imposter.Protocol.VotePosted { voter = "Ada"; voted_for = "Grace" },
        "{\"type\":\"vote_posted\",\"voter\":\"Ada\",\"voted_for\":\"Grace\"}"
      );
      ( Imposter.Protocol.Accused { player = "Grace"; was_imposter = false },
        "{\"type\":\"accused\",\"player\":\"Grace\",\"was_imposter\":false}" );
      ( Imposter.Protocol.YourTurnGuess { hint = "mane" },
        "{\"type\":\"your_turn_guess\",\"hint\":\"mane\"}" );
      ( Imposter.Protocol.YourTurnPlayAgain,
        "{\"type\":\"your_turn_play_again\"}" );
      ( Imposter.Protocol.ServerShutdown "bye",
        "{\"type\":\"shutdown\",\"message\":\"bye\"}" );
    ]
  in
  List.iter
    (fun (msg, encoded) ->
      assert_equal ~printer:pp_string encoded
        (Imposter.Protocol.encode_server msg);
      assert_equal ~printer:(pp_result pp_server_msg) (Ok msg)
        (Imposter.Protocol.decode_server encoded))
    cases

let test_encode_server_exact_round_start_shapes _ =
  let crew =
    Imposter.Protocol.RoundStart
      {
        category = "Animals";
        role = `Crew;
        word = Some "lion";
        players = [ "Ada"; "Grace" ];
        clue_order = [ "Grace"; "Ada" ];
      }
  in
  let imposter =
    Imposter.Protocol.RoundStart
      {
        category = "Animals";
        role = `Imposter;
        word = None;
        players = [ "Ada"; "Grace" ];
        clue_order = [];
      }
  in
  let crew_encoded =
    "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"crew\",\"word\":\"lion\",\"players\":[\"Ada\",\"Grace\"],\"clue_order\":[\"Grace\",\"Ada\"]}"
  in
  let imposter_encoded =
    "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"imposter\",\"word\":null,\"players\":[\"Ada\",\"Grace\"],\"clue_order\":[]}"
  in
  assert_equal ~printer:pp_string crew_encoded
    (Imposter.Protocol.encode_server crew);
  assert_equal ~printer:(pp_result pp_server_msg) (Ok crew)
    (Imposter.Protocol.decode_server crew_encoded);
  assert_equal ~printer:pp_string imposter_encoded
    (Imposter.Protocol.encode_server imposter);
  assert_equal ~printer:(pp_result pp_server_msg) (Ok imposter)
    (Imposter.Protocol.decode_server imposter_encoded)

let test_encode_server_exact_round_end_shapes _ =
  let cases =
    [
      ( Imposter.Protocol.RoundEnd
          { winner = `Crew; imposter = "Ada"; word = "lion"; reason = "caught" },
        "{\"type\":\"round_end\",\"winner\":\"crew\",\"imposter\":\"Ada\",\"word\":\"lion\",\"reason\":\"caught\"}"
      );
      ( Imposter.Protocol.RoundEnd
          {
            winner = `Imposter;
            imposter = "Grace";
            word = "apple";
            reason = "guessed";
          },
        "{\"type\":\"round_end\",\"winner\":\"imposter\",\"imposter\":\"Grace\",\"word\":\"apple\",\"reason\":\"guessed\"}"
      );
      ( Imposter.Protocol.RoundEnd
          { winner = `Draw; imposter = "Linus"; word = "fig"; reason = "tie" },
        "{\"type\":\"round_end\",\"winner\":\"draw\",\"imposter\":\"Linus\",\"word\":\"fig\",\"reason\":\"tie\"}"
      );
    ]
  in
  List.iter
    (fun (msg, encoded) ->
      assert_equal ~printer:pp_string encoded
        (Imposter.Protocol.encode_server msg);
      assert_equal ~printer:(pp_result pp_server_msg) (Ok msg)
        (Imposter.Protocol.decode_server encoded))
    cases

let test_encode_server_exact_score_update_shape _ =
  let msg =
    Imposter.Protocol.ScoreUpdate
      [
        {
          player = "Ada";
          crew_wins = 10;
          imposter_wins = 0;
          times_caught = 1;
          rounds_played = 11;
        };
        {
          player = "Grace";
          crew_wins = 1;
          imposter_wins = 9;
          times_caught = 2;
          rounds_played = 12;
        };
      ]
  in
  let encoded =
    "{\"type\":\"score_update\",\"count\":2,\"p0\":\"Ada\",\"cw0\":10,\"iw0\":0,\"tc0\":1,\"rp0\":11,\"p1\":\"Grace\",\"cw1\":1,\"iw1\":9,\"tc1\":2,\"rp1\":12}"
  in
  assert_equal ~printer:pp_string encoded (Imposter.Protocol.encode_server msg);
  assert_equal ~printer:(pp_result pp_server_msg) (Ok msg)
    (Imposter.Protocol.decode_server encoded);
  assert_equal ~printer:pp_string "{\"type\":\"score_update\",\"count\":0}"
    (Imposter.Protocol.encode_server (Imposter.Protocol.ScoreUpdate []));
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok (Imposter.Protocol.ScoreUpdate []))
    (Imposter.Protocol.decode_server "{\"type\":\"score_update\",\"count\":0}")

let test_protocol_sanitizes_all_client_string_fields _ =
  let cases =
    [
      ( Imposter.Protocol.Join "A\"B\\C\nD",
        Ok (Imposter.Protocol.Join "A_B_C D") );
      (Imposter.Protocol.Clue "one\ttwo", Ok (Imposter.Protocol.Clue "one two"));
      (Imposter.Protocol.Vote "A\rB", Ok (Imposter.Protocol.Vote "A B"));
      ( Imposter.Protocol.ImposterGuess "x\"y\\z",
        Ok (Imposter.Protocol.ImposterGuess "x_y_z") );
    ]
  in
  List.iter
    (fun (msg, expected) ->
      let encoded = Imposter.Protocol.encode_client msg in
      assert_bool "quote and slash sanitized"
        (not (String.contains encoded '\\'));
      assert_equal ~printer:(pp_result pp_client_msg) expected
        (Imposter.Protocol.decode_client encoded))
    cases

let test_protocol_sanitizes_all_server_string_fields _ =
  let cases =
    [
      (Imposter.Protocol.Welcome "A\"B", Ok (Imposter.Protocol.Welcome "A_B"));
      ( Imposter.Protocol.LobbyUpdate [ "A\\B"; "C\nD" ],
        Ok (Imposter.Protocol.LobbyUpdate [ "A_B"; "C D" ]) );
      ( Imposter.Protocol.CluePosted { player = "A\"B"; clue = "C\\D" },
        Ok (Imposter.Protocol.CluePosted { player = "A_B"; clue = "C_D" }) );
      ( Imposter.Protocol.VotePosted { voter = "A\tB"; voted_for = "C\rD" },
        Ok (Imposter.Protocol.VotePosted { voter = "A B"; voted_for = "C D" })
      );
      ( Imposter.Protocol.Accused { player = "A\nB"; was_imposter = true },
        Ok (Imposter.Protocol.Accused { player = "A B"; was_imposter = true })
      );
      ( Imposter.Protocol.YourTurnGuess { hint = "a\"b\\c" },
        Ok (Imposter.Protocol.YourTurnGuess { hint = "a_b_c" }) );
      ( Imposter.Protocol.ServerShutdown "bye\nnow",
        Ok (Imposter.Protocol.ServerShutdown "bye now") );
    ]
  in
  List.iter
    (fun (msg, expected) ->
      let encoded = Imposter.Protocol.encode_server msg in
      assert_bool "encoded string should stay one line"
        (not (String.contains encoded '\n'));
      assert_equal ~printer:(pp_result pp_server_msg) expected
        (Imposter.Protocol.decode_server encoded))
    cases

let test_protocol_sanitizes_nested_round_and_score_fields _ =
  let round =
    Imposter.Protocol.RoundStart
      {
        category = "A\"B";
        role = `Crew;
        word = Some "C\\D";
        players = [ "E\nF" ];
        clue_order = [ "G\tH" ];
      }
  in
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok
       (Imposter.Protocol.RoundStart
          {
            category = "A_B";
            role = `Crew;
            word = Some "C_D";
            players = [ "E F" ];
            clue_order = [ "G H" ];
          }))
    (Imposter.Protocol.decode_server (Imposter.Protocol.encode_server round));
  let score =
    Imposter.Protocol.ScoreUpdate
      [
        {
          player = "A\"B\\C";
          crew_wins = 1;
          imposter_wins = 2;
          times_caught = 3;
          rounds_played = 4;
        };
      ]
  in
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok
       (Imposter.Protocol.ScoreUpdate
          [
            {
              player = "A_B_C";
              crew_wins = 1;
              imposter_wins = 2;
              times_caught = 3;
              rounds_played = 4;
            };
          ]))
    (Imposter.Protocol.decode_server (Imposter.Protocol.encode_server score))

let test_protocol_duplicate_fields_last_textual_field_wins _ =
  assert_equal ~printer:(pp_result pp_client_msg)
    (Ok (Imposter.Protocol.Join "second"))
    (Imposter.Protocol.decode_client
       "{\"type\":\"join\",\"name\":\"first\",\"name\":\"second\"}");
  assert_equal ~printer:(pp_result pp_client_msg)
    (Ok (Imposter.Protocol.Clue "actual"))
    (Imposter.Protocol.decode_client
       "{\"type\":\"join\",\"name\":\"Ada\",\"type\":\"clue\",\"clue\":\"actual\"}");
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok (Imposter.Protocol.ServerShutdown "new"))
    (Imposter.Protocol.decode_server
       "{\"type\":\"error\",\"message\":\"old\",\"type\":\"shutdown\",\"message\":\"new\"}")

let test_protocol_trailing_text_is_ignored _ =
  assert_equal ~printer:(pp_result pp_client_msg) (Ok Imposter.Protocol.Start)
    (Imposter.Protocol.decode_client "{\"type\":\"start\"} trailing");
  assert_equal ~printer:(pp_result pp_client_msg)
    (Ok (Imposter.Protocol.Vote "Ada"))
    (Imposter.Protocol.decode_client
       "{\"type\":\"vote\",\"voted_for\":\"Ada\"}{\"type\":\"start\"}");
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok Imposter.Protocol.YourTurnClue)
    (Imposter.Protocol.decode_server "{\"type\":\"your_turn_clue\"}\nnot parsed")

let test_protocol_allows_space_and_tab_whitespace_only _ =
  assert_equal ~printer:(pp_result pp_client_msg) (Ok Imposter.Protocol.Start)
    (Imposter.Protocol.decode_client " \t{\"type\"\t:\t\"start\"\t}\t ");
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok (Imposter.Protocol.LobbyUpdate [ "Ada"; "Grace" ]))
    (Imposter.Protocol.decode_server
       "\t { \t \"type\" : \"lobby\" , \"players\" : [ \"Ada\" , \t\"Grace\" ] \
        }");
  assert_error (Imposter.Protocol.decode_client "\n{\"type\":\"start\"}");
  assert_error
    (Imposter.Protocol.decode_server "{\"type\":\"lobby\",\n\"players\":[]}")

let test_protocol_list_parser_preserves_order _ =
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok (Imposter.Protocol.LobbyUpdate [ "one"; "two"; "three"; "four" ]))
    (Imposter.Protocol.decode_server
       "{\"type\":\"lobby\",\"players\":[\"one\",\"two\",\"three\",\"four\"]}");
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok
       (Imposter.Protocol.RoundStart
          {
            category = "x";
            role = `Crew;
            word = Some "y";
            players = [ "p1"; "p2"; "p3" ];
            clue_order = [ "p3"; "p1"; "p2" ];
          }))
    (Imposter.Protocol.decode_server
       "{\"type\":\"round_start\",\"category\":\"x\",\"role\":\"crew\",\"word\":\"y\",\"players\":[\"p1\",\"p2\",\"p3\"],\"clue_order\":[\"p3\",\"p1\",\"p2\"]}")

let test_protocol_int_parser_score_update_edges _ =
  assert_equal ~printer:(pp_result pp_server_msg)
    (Ok
       (Imposter.Protocol.ScoreUpdate
          [
            {
              player = "Zero";
              crew_wins = 0;
              imposter_wins = 0;
              times_caught = 0;
              rounds_played = 0;
            };
          ]))
    (Imposter.Protocol.decode_server
       "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Zero\",\"cw0\":00,\"iw0\":0,\"tc0\":0,\"rp0\":0}");
  assert_raises (Invalid_argument "List.init") (fun () ->
      ignore
        (Imposter.Protocol.decode_server
           "{\"type\":\"score_update\",\"count\":-1}"));
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"score_update\",\"count\":1,\"p0\":\"A\",\"cw0\":-}")

let test_decode_client_each_type_rejects_wrong_field_type _ =
  let inputs =
    [
      "{\"type\":\"join\",\"name\":false}";
      "{\"type\":\"clue\",\"clue\":false}";
      "{\"type\":\"vote\",\"voted_for\":false}";
      "{\"type\":\"imposter_guess\",\"guess\":false}";
      "{\"type\":\"play_again\",\"yes\":\"true\"}";
      "{\"type\":false}";
      "{\"type\":null}";
      "{\"type\":[]}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_client input))
    inputs

let test_decode_server_each_type_rejects_wrong_field_type _ =
  let inputs =
    [
      "{\"type\":\"welcome\",\"name\":false}";
      "{\"type\":\"lobby\",\"players\":\"Ada\"}";
      "{\"type\":\"error\",\"message\":false}";
      "{\"type\":\"clue_posted\",\"player\":false,\"clue\":\"x\"}";
      "{\"type\":\"clue_posted\",\"player\":\"Ada\",\"clue\":false}";
      "{\"type\":\"your_turn_vote\",\"candidates\":\"Ada\"}";
      "{\"type\":\"vote_posted\",\"voter\":false,\"voted_for\":\"Ada\"}";
      "{\"type\":\"vote_posted\",\"voter\":\"Ada\",\"voted_for\":false}";
      "{\"type\":\"accused\",\"player\":false,\"was_imposter\":true}";
      "{\"type\":\"accused\",\"player\":\"Ada\",\"was_imposter\":\"true\"}";
      "{\"type\":\"your_turn_guess\",\"hint\":false}";
      "{\"type\":\"shutdown\",\"message\":false}";
      "{\"type\":false}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_server input))
    inputs

let test_decode_server_round_start_missing_fields _ =
  let inputs =
    [
      "{\"type\":\"round_start\",\"role\":\"crew\",\"word\":\"lion\",\"players\":[],\"clue_order\":[]}";
      "{\"type\":\"round_start\",\"category\":\"Animals\",\"word\":\"lion\",\"players\":[],\"clue_order\":[]}";
      "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"crew\",\"players\":[],\"clue_order\":[]}";
      "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"crew\",\"word\":\"lion\",\"clue_order\":[]}";
      "{\"type\":\"round_start\",\"category\":\"Animals\",\"role\":\"crew\",\"word\":\"lion\",\"players\":[]}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_server input))
    inputs

let test_decode_server_round_end_missing_fields _ =
  let inputs =
    [
      "{\"type\":\"round_end\",\"imposter\":\"Ada\",\"word\":\"lion\",\"reason\":\"caught\"}";
      "{\"type\":\"round_end\",\"winner\":\"crew\",\"word\":\"lion\",\"reason\":\"caught\"}";
      "{\"type\":\"round_end\",\"winner\":\"crew\",\"imposter\":\"Ada\",\"reason\":\"caught\"}";
      "{\"type\":\"round_end\",\"winner\":\"crew\",\"imposter\":\"Ada\",\"word\":\"lion\"}";
      "{\"type\":\"round_end\",\"winner\":false,\"imposter\":\"Ada\",\"word\":\"lion\",\"reason\":\"caught\"}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_server input))
    inputs

let test_decode_server_score_update_missing_indexed_fields _ =
  let inputs =
    [
      "{\"type\":\"score_update\"}";
      "{\"type\":\"score_update\",\"count\":1}";
      "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Ada\"}";
      "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Ada\",\"cw0\":0}";
      "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Ada\",\"cw0\":0,\"iw0\":0}";
      "{\"type\":\"score_update\",\"count\":1,\"p0\":\"Ada\",\"cw0\":0,\"iw0\":0,\"tc0\":0}";
      "{\"type\":\"score_update\",\"count\":2,\"p0\":\"Ada\",\"cw0\":0,\"iw0\":0,\"tc0\":0,\"rp0\":0}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_server input))
    inputs

let test_protocol_malformed_object_boundaries _ =
  let client_inputs =
    [
      "";
      "[]";
      "\"type\"";
      "{\"type\":\"start\"";
      "{\"type\":\"start\",}";
      "{\"type\":\"start\"";
      "{\"type\":\"start\",";
      "{,\"type\":\"start\"}";
      "{\"type\":\"start\" \"extra\":\"x\"}";
    ]
  in
  let server_inputs =
    [
      "";
      "[]";
      "\"type\"";
      "{\"type\":\"your_turn_clue\"";
      "{\"type\":\"your_turn_clue\",}";
      "{\"type\":\"your_turn_clue\"";
      "{\"type\":\"your_turn_clue\",";
      "{,\"type\":\"your_turn_clue\"}";
      "{\"type\":\"your_turn_clue\" \"extra\":\"x\"}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_client input))
    client_inputs;
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_server input))
    server_inputs

let test_protocol_malformed_string_boundaries _ =
  let inputs =
    [
      "{\"type\":\"join\",\"name\":\"Ada}";
      "{\"type\":\"join\",\"name\":Ada}";
      "{\"type\":\"join\",\"name\":\"Ada\",\"x\":\"unterminated}";
      "{\"type\":\"join\",\"name\":\"Ada\",\"x\":'bad'}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_client input))
    inputs

let test_protocol_malformed_literals _ =
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"play_again\",\"yes\":tru}");
  assert_error
    (Imposter.Protocol.decode_client "{\"type\":\"play_again\",\"yes\":FALSE}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"round_start\",\"category\":\"x\",\"role\":\"crew\",\"word\":nil,\"players\":[],\"clue_order\":[]}");
  assert_error
    (Imposter.Protocol.decode_server
       "{\"type\":\"accused\",\"player\":\"Ada\",\"was_imposter\":truth}");
  assert_error
    (Imposter.Protocol.decode_server "{\"type\":\"score_update\",\"count\":one}")

let test_protocol_malformed_lists _ =
  let inputs =
    [
      "{\"type\":\"lobby\",\"players\":[}";
      "{\"type\":\"lobby\",\"players\":[\"Ada\"}";
      "{\"type\":\"lobby\",\"players\":[\"Ada\",]}";
      "{\"type\":\"lobby\",\"players\":[\"Ada\" \"Grace\"]}";
      "{\"type\":\"lobby\",\"players\":[false]}";
      "{\"type\":\"lobby\",\"players\":[null]}";
      "{\"type\":\"lobby\",\"players\":[[]]}";
      "{\"type\":\"lobby\",\"players\":[\"Ada\",false]}";
    ]
  in
  List.iter
    (fun input -> assert_error (Imposter.Protocol.decode_server input))
    inputs

let test_round_trip_many_scoreboard_sizes _ =
  let make_entry i =
    {
      Imposter.Protocol.player = "p" ^ string_of_int i;
      crew_wins = i;
      imposter_wins = i + 1;
      times_caught = i + 2;
      rounds_played = i + 3;
    }
  in
  List.iter
    (fun n ->
      let entries = List.init n make_entry in
      let msg = Imposter.Protocol.ScoreUpdate entries in
      assert_equal ~printer:(pp_result pp_server_msg) (Ok msg)
        (Imposter.Protocol.decode_server (Imposter.Protocol.encode_server msg)))
    [ 0; 1; 2; 3; 5; 10 ]

let test_round_trip_many_round_start_variants _ =
  let cases =
    [
      (`Crew, Some "lion", [ "Ada" ], [ "Ada" ]);
      (`Crew, Some "apple", [ "Ada"; "Grace" ], [ "Grace"; "Ada" ]);
      (`Imposter, None, [ "Ada"; "Grace"; "Linus" ], []);
      (`Imposter, None, [], []);
    ]
  in
  List.iter
    (fun (role, word, players, clue_order) ->
      let msg =
        Imposter.Protocol.RoundStart
          { category = "x"; role; word; players; clue_order }
      in
      assert_equal ~printer:(pp_result pp_server_msg) (Ok msg)
        (Imposter.Protocol.decode_server (Imposter.Protocol.encode_server msg)))
    cases

let test_client_round_trip_sanitized_payload_matrix _ =
  let payloads = [ ""; "plain"; "has space"; "quote\""; "slash\\"; "tab\t" ] in
  List.iter
    (fun payload ->
      List.iter
        (fun make_msg ->
          let encoded = Imposter.Protocol.encode_client (make_msg payload) in
          assert_bool "no encoded newlines" (not (String.contains encoded '\n'));
          assert_bool "encoded as object" (String.length encoded >= 2);
          assert_equal ~printer:pp_char '{' encoded.[0])
        [
          (fun s -> Imposter.Protocol.Join s);
          (fun s -> Imposter.Protocol.Clue s);
          (fun s -> Imposter.Protocol.Vote s);
          (fun s -> Imposter.Protocol.ImposterGuess s);
        ])
    payloads

let test_server_round_trip_sanitized_payload_matrix _ =
  let payloads = [ ""; "plain"; "has space"; "quote\""; "slash\\"; "tab\t" ] in
  List.iter
    (fun payload ->
      List.iter
        (fun msg ->
          let encoded = Imposter.Protocol.encode_server msg in
          assert_bool "no encoded newlines" (not (String.contains encoded '\n'));
          assert_bool "encoded object" (String.length encoded >= 2);
          assert_equal ~printer:pp_char '{' encoded.[0])
        [
          Imposter.Protocol.Welcome payload;
          Imposter.Protocol.Error payload;
          Imposter.Protocol.CluePosted { player = payload; clue = payload };
          Imposter.Protocol.YourTurnGuess { hint = payload };
          Imposter.Protocol.ServerShutdown payload;
        ])
    payloads

let suite =
  "Imposter Tests"
  >::: [
         "load_categories" >:: test_load_categories;
         "load_words" >:: test_load_words;
         "get_hints" >:: test_get_hints;
         "get_hints_edge_cases" >:: test_get_hints_edge_cases;
         "random_category" >:: test_random_category;
         "random_answer" >:: test_random_answer;
         "normalize_guess" >:: test_normalize_guess;
         "is_correct" >:: test_is_correct;
         "hint_filtering" >:: test_hint_filtering;
         "decode_client_manual_inputs" >:: test_decode_client_manual_inputs;
         "decode_server_manual_inputs" >:: test_decode_server_manual_inputs;
         "client_protocol_round_trips" >:: test_client_protocol_round_trips;
         "server_protocol_round_trips" >:: test_server_protocol_round_trips;
         "protocol_sanitizes_encoded_strings"
         >:: test_protocol_sanitizes_encoded_strings;
         "decode_client_errors" >:: test_decode_client_errors;
         "decode_server_errors" >:: test_decode_server_errors;
         "load_categories_exact_file_order"
         >:: test_load_categories_exact_file_order;
         "load_words_exact_representative_categories"
         >:: test_load_words_exact_representative_categories;
         "load_words_matches_category_file"
         >:: test_load_words_matches_category_file;
         "get_hints_preserves_non_answer_duplicates"
         >:: test_get_hints_preserves_non_answer_duplicates;
         "get_hints_missing_answer_keeps_every_word"
         >:: test_get_hints_missing_answer_keeps_every_word;
         "choose_random_singleton_paths" >:: test_choose_random_singleton_paths;
         "normalize_guess_ascii_only_case_folding"
         >:: test_normalize_guess_ascii_only_case_folding;
         "is_correct_trims_guess_but_not_answer"
         >:: test_is_correct_trims_guess_but_not_answer;
         "unused_hints_uses_exact_string_membership"
         >:: test_unused_hints_uses_exact_string_membership;
         "remove_hint_and_guess_case_behavior"
         >:: test_remove_hint_and_guess_case_behavior;
         "encode_client_exact_shapes" >:: test_encode_client_exact_shapes;
         "encode_server_exact_simple_shapes"
         >:: test_encode_server_exact_simple_shapes;
         "encode_server_exact_round_start_shapes"
         >:: test_encode_server_exact_round_start_shapes;
         "encode_server_exact_round_end_shapes"
         >:: test_encode_server_exact_round_end_shapes;
         "encode_server_exact_score_update_shape"
         >:: test_encode_server_exact_score_update_shape;
         "protocol_sanitizes_all_client_string_fields"
         >:: test_protocol_sanitizes_all_client_string_fields;
         "protocol_sanitizes_all_server_string_fields"
         >:: test_protocol_sanitizes_all_server_string_fields;
         "protocol_sanitizes_nested_round_and_score_fields"
         >:: test_protocol_sanitizes_nested_round_and_score_fields;
         "protocol_duplicate_fields_last_textual_field_wins"
         >:: test_protocol_duplicate_fields_last_textual_field_wins;
         "protocol_trailing_text_is_ignored"
         >:: test_protocol_trailing_text_is_ignored;
         "protocol_allows_space_and_tab_whitespace_only"
         >:: test_protocol_allows_space_and_tab_whitespace_only;
         "protocol_list_parser_preserves_order"
         >:: test_protocol_list_parser_preserves_order;
         "protocol_int_parser_score_update_edges"
         >:: test_protocol_int_parser_score_update_edges;
         "decode_client_each_type_rejects_wrong_field_type"
         >:: test_decode_client_each_type_rejects_wrong_field_type;
         "decode_server_each_type_rejects_wrong_field_type"
         >:: test_decode_server_each_type_rejects_wrong_field_type;
         "decode_server_round_start_missing_fields"
         >:: test_decode_server_round_start_missing_fields;
         "decode_server_round_end_missing_fields"
         >:: test_decode_server_round_end_missing_fields;
         "decode_server_score_update_missing_indexed_fields"
         >:: test_decode_server_score_update_missing_indexed_fields;
         "protocol_malformed_object_boundaries"
         >:: test_protocol_malformed_object_boundaries;
         "protocol_malformed_string_boundaries"
         >:: test_protocol_malformed_string_boundaries;
         "protocol_malformed_literals" >:: test_protocol_malformed_literals;
         "protocol_malformed_lists" >:: test_protocol_malformed_lists;
         "round_trip_many_scoreboard_sizes"
         >:: test_round_trip_many_scoreboard_sizes;
         "round_trip_many_round_start_variants"
         >:: test_round_trip_many_round_start_variants;
         "client_round_trip_sanitized_payload_matrix"
         >:: test_client_round_trip_sanitized_payload_matrix;
         "server_round_trip_sanitized_payload_matrix"
         >:: test_server_round_trip_sanitized_payload_matrix;
       ]

let () = run_test_tt_main suite
