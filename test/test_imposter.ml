open OUnit2

let pp_string_list xs = "[" ^ String.concat "; " xs ^ "]"

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
  assert_equal "Animals" (List.hd categories);
  assert_equal 50 (List.length categories);
  assert_bool "TV genres category present" (List.mem "TV genres" categories)

let test_load_words _ =
  let words_map = Imposter.Game.load_words () in
  let animals = Hashtbl.find words_map "Animals" in
  let fruits = Hashtbl.find words_map "Fruits" in
  assert_bool "lion present" (List.exists (( = ) "lion") animals);
  assert_bool "shark present" (List.exists (( = ) "shark") animals);
  assert_equal 9 (List.length animals);
  assert_bool "apple present" (List.exists (( = ) "apple") fruits)

let test_get_hints _ =
  let words_map = Imposter.Game.load_words () in
  Random.init 0;
  let hints = Imposter.Game.get_hints words_map "Animals" "lion" in
  assert_equal 8 (List.length hints);
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
  assert_equal "Only" (Imposter.Single_player.random_category [ "Only" ]);
  assert_raises (Invalid_argument "cannot choose from an empty list") (fun () ->
      ignore (Imposter.Single_player.random_category []))

let test_random_answer _ =
  Random.init 8;
  let words_map = make_words_map [ ("Animals", [ "lion"; "tiger" ]) ] in
  let answer =
    Imposter.Single_player.random_answer ~words_map ~category:"Animals"
  in
  assert_bool "answer came from category" (List.mem answer [ "lion"; "tiger" ]);
  assert_equal "solo"
    (Imposter.Single_player.random_answer
       ~words_map:(make_words_map [ ("Only", [ "solo" ]) ])
       ~category:"Only");
  assert_raises Not_found (fun () ->
      ignore
        (Imposter.Single_player.random_answer ~words_map ~category:"Missing"))

let test_normalize_guess _ =
  assert_equal Imposter.Single_player.Give_up
    (Imposter.Single_player.normalize_guess "  GIVE UP  ");
  assert_equal (Imposter.Single_player.Guess "orange")
    (Imposter.Single_player.normalize_guess "  Orange  ");
  assert_equal (Imposter.Single_player.Guess "give  up")
    (Imposter.Single_player.normalize_guess "give  up");
  assert_equal (Imposter.Single_player.Guess "pear pie")
    (Imposter.Single_player.normalize_guess " Pear Pie ");
  assert_equal (Imposter.Single_player.Guess "")
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
      assert_ok_equal msg (Imposter.Protocol.decode_client encoded))
    client_messages

let test_server_protocol_round_trips _ =
  List.iter
    (fun msg ->
      let encoded = Imposter.Protocol.encode_server msg in
      assert_bool "server encoding has no newline"
        (not (String.contains encoded '\n'));
      assert_ok_equal msg (Imposter.Protocol.decode_server encoded))
    server_messages

let test_protocol_sanitizes_encoded_strings _ =
  assert_equal (Ok (Imposter.Protocol.Join "A_B_C"))
    (Imposter.Protocol.decode_client
       (Imposter.Protocol.encode_client (Imposter.Protocol.Join "A\"B\\C")));
  assert_equal (Ok (Imposter.Protocol.Clue "two words"))
    (Imposter.Protocol.decode_client
       (Imposter.Protocol.encode_client (Imposter.Protocol.Clue "two\twords")));
  assert_equal (Ok (Imposter.Protocol.Error "bad input"))
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
       ]

let () = run_test_tt_main suite
