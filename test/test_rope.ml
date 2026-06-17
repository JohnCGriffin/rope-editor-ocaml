module Rope = Ropes.Rope
module U = Ropes.Ustring

let line s = Rope.Line (U.of_string s)
let loc l c = Rope.location_of l c
let str r = Rope.string_of r
let line_str r l = U.string_of (Rope.line_at r (loc l 0))

let () = Alcotest.run "Rope" [
  "line_count", [
    Alcotest.test_case "single Line" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (Rope.line_count (line "hello")));

    Alcotest.test_case "node_of two lines" `Quick (fun () ->
      let r = Rope.node_of (line "foo\n") (line "bar") in
      Alcotest.(check int) "2" 2 (Rope.line_count r));

    Alcotest.test_case "build_rope of 3" `Quick (fun () ->
      let r = Rope.build_rope [line "a\n"; line "b\n"; line "c"] in
      Alcotest.(check int) "3" 3 (Rope.line_count r));
  ];

  "char_count", [
    Alcotest.test_case "single ascii line" `Quick (fun () ->
      Alcotest.(check int) "5" 5 (Rope.char_count (line "hello")));

    Alcotest.test_case "emoji counts as one codepoint" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (Rope.char_count (line "💀")));

    Alcotest.test_case "newline counts as one char" `Quick (fun () ->
      Alcotest.(check int) "4" 4 (Rope.char_count (line "foo\n")));

    Alcotest.test_case "node sums children" `Quick (fun () ->
      let r = Rope.node_of (line "foo\n") (line "bar") in
      Alcotest.(check int) "7" 7 (Rope.char_count r));
  ];

  "line_at", [
    Alcotest.test_case "single Line returns its text" `Quick (fun () ->
      Alcotest.(check string) "hello" "hello" (line_str (line "hello") 0));

    Alcotest.test_case "first of two lines includes newline" `Quick (fun () ->
      let r = Rope.build_rope [line "first\n"; line "second"] in
      Alcotest.(check string) "first\n" "first\n" (line_str r 0));

    Alcotest.test_case "second of two lines" `Quick (fun () ->
      let r = Rope.build_rope [line "first\n"; line "second"] in
      Alcotest.(check string) "second" "second" (line_str r 1));

    Alcotest.test_case "third of three lines" `Quick (fun () ->
      let r = Rope.build_rope [line "a\n"; line "b\n"; line "c"] in
      Alcotest.(check string) "c" "c" (line_str r 2));

    Alcotest.test_case "negative index raises" `Quick (fun () ->
      Alcotest.check_raises "negative" (Failure "ndx -1 outside [0..1]")
        (fun () -> ignore (Rope.line_at (line "x") (loc (-1) 0))));

    Alcotest.test_case "out-of-bounds index raises" `Quick (fun () ->
      Alcotest.check_raises "oob" (Failure "ndx 1 outside [0..1]")
        (fun () -> ignore (Rope.line_at (line "x") (loc 1 0))));
  ];

  "string_of", [
    Alcotest.test_case "single line" `Quick (fun () ->
      Alcotest.(check string) "hello" "hello" (str (line "hello")));

    Alcotest.test_case "preserves newlines" `Quick (fun () ->
      let r = Rope.build_rope [line "foo\n"; line "bar\n"; line "baz"] in
      Alcotest.(check string) "foo\nbar\nbaz" "foo\nbar\nbaz" (str r));

    Alcotest.test_case "preserves unicode" `Quick (fun () ->
      let r = Rope.build_rope [line "💀\n"; line "🌞"] in
      Alcotest.(check string) "emoji" "💀\n🌞" (str r));
  ];

  "insert", [
    Alcotest.test_case "insert at beginning of line" `Quick (fun () ->
      let r = Rope.insert (line "world") (loc 0 0) (U.of_string "hello ") in
      Alcotest.(check string) "hello world" "hello world" (str r));

    Alcotest.test_case "insert in middle of line" `Quick (fun () ->
      let r = Rope.insert (line "helo") (loc 0 2) (U.of_string "l") in
      Alcotest.(check string) "hello" "hello" (str r));

    Alcotest.test_case "insert at end of line" `Quick (fun () ->
      let r = Rope.insert (line "hi") (loc 0 2) (U.of_string "!") in
      Alcotest.(check string) "hi!" "hi!" (str r));

    Alcotest.test_case "insert newline splits line" `Quick (fun () ->
      let r = Rope.insert (line "helloworld") (loc 0 5) (U.of_string "\n") in
      Alcotest.(check int) "line_count" 2 (Rope.line_count r);
      Alcotest.(check string) "first line" "hello\n" (line_str r 0);
      Alcotest.(check string) "second line" "world" (line_str r 1));

    Alcotest.test_case "insert text with embedded newline" `Quick (fun () ->
      let r = Rope.insert (line "ac") (loc 0 1) (U.of_string "b\n") in
      Alcotest.(check int) "line_count" 2 (Rope.line_count r);
      Alcotest.(check string) "first" "ab\n" (line_str r 0);
      Alcotest.(check string) "second" "c" (line_str r 1));

    Alcotest.test_case "insert into second line of multi-line rope" `Quick (fun () ->
      let r = Rope.build_rope [line "foo\n"; line "bar"] in
      let r' = Rope.insert r (loc 1 1) (U.of_string "!!") in
      Alcotest.(check string) "result" "foo\nb!!ar" (str r'));

    Alcotest.test_case "insert does not change line_count when no newline added" `Quick (fun () ->
      let r = Rope.build_rope [line "foo\n"; line "bar"] in
      let r' = Rope.insert r (loc 0 1) (U.of_string "XX") in
      Alcotest.(check int) "line_count" 2 (Rope.line_count r'));
  ];

  "depth", [
    Alcotest.test_case "single Line has depth 1" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (Rope.depth (line "x")));

    Alcotest.test_case "node_of two Lines has depth 2" `Quick (fun () ->
      Alcotest.(check int) "2" 2
        (Rope.depth (Rope.node_of (line "a\n") (line "b"))));

    Alcotest.test_case "build_rope of 8 lines has depth 4" `Quick (fun () ->
      let lines = List.init 8 (fun i -> line (string_of_int i ^ "\n")) in
      Alcotest.(check int) "4" 4 (Rope.depth (Rope.build_rope lines)));
  ];

  "leaves_of", [
    Alcotest.test_case "single Line yields one leaf" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (List.length (Rope.leaves_of (line "abc"))));

    Alcotest.test_case "node_of yields two leaves" `Quick (fun () ->
      Alcotest.(check int) "2" 2
        (List.length (Rope.leaves_of (Rope.node_of (line "a\n") (line "b")))));

    Alcotest.test_case "build_rope of 4 yields 4 leaves" `Quick (fun () ->
      let r = Rope.build_rope [line "a\n"; line "b\n"; line "c\n"; line "d"] in
      Alcotest.(check int) "4" 4 (List.length (Rope.leaves_of r)));

    Alcotest.test_case "leaves content in order" `Quick (fun () ->
      let r = Rope.node_of (line "foo\n") (line "bar") in
      let contents =
        List.map (function
          | Rope.Line s -> U.string_of s
          | Rope.Node _ -> failwith "unexpected node in leaves")
          (Rope.leaves_of r)
      in
      Alcotest.(check (list string)) "order" ["foo\n"; "bar"] contents);
  ];
]
