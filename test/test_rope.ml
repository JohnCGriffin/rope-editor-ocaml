module Rope = Ropes.Rope
module U = Ropes.Ustring

let leaf s = Rope.Leaf (U.of_string s)

let prop_tests = [
  QCheck2.Test.make
    ~name:"string_of . build_rope = String.concat"
    ~print:QCheck2.Print.(list string)
    QCheck2.Gen.(list string_printable)
    (fun strs ->
      let r = Rope.build_rope (List.map leaf strs) in
      Rope.string_of r = String.concat "" strs);

  QCheck2.Test.make
    ~name:"concatenate string_of = (^)"
    ~print:QCheck2.Print.(pair string string)
    QCheck2.Gen.(pair string_printable string_printable)
    (fun (a, b) ->
      let r = Rope.concatenate (leaf a) (leaf b) in
      Rope.string_of r = a ^ b);

  QCheck2.Test.make
    ~name:"length r = String.length (string_of r) for ascii"
    ~print:Fun.id
    QCheck2.Gen.string_printable
    (fun s ->
      Rope.length (leaf s) = String.length (Rope.string_of (leaf s)));

  QCheck2.Test.make
    ~name:"concatenate is associative over string_of"
    ~print:QCheck2.Print.(triple string string string)
    QCheck2.Gen.(triple string_printable string_printable string_printable)
    (fun (a, b, c) ->
      let r1 = Rope.concatenate (Rope.concatenate (leaf a) (leaf b)) (leaf c) in
      let r2 = Rope.concatenate (leaf a) (Rope.concatenate (leaf b) (leaf c)) in
      Rope.string_of r1 = Rope.string_of r2);

  QCheck2.Test.make
    ~name:"char_at i matches String.get for all valid i (ascii)"
    ~print:Fun.id
    QCheck2.Gen.string_printable
    (fun s ->
      let r = leaf s in
      let n = Rope.length r in
      List.for_all
        (fun i -> Uchar.to_int (Rope.char_at r i) = Char.code s.[i])
        (List.init n Fun.id));
]

let () = Alcotest.run "Rope" [
  "length", [
    Alcotest.test_case "leaf ascii" `Quick (fun () ->
      Alcotest.(check int) "5" 5 (Rope.length (leaf "hello")));

    Alcotest.test_case "leaf emoji counts codepoints" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (Rope.length (leaf "💀")));

    Alcotest.test_case "node sums children" `Quick (fun () ->
      Alcotest.(check int) "6" 6
        (Rope.length (Rope.concatenate (leaf "foo") (leaf "bar"))));

    Alcotest.test_case "build_rope length = total chars" `Quick (fun () ->
      let r = Rope.build_rope (List.map leaf ["Once "; "upon "; "a "; "time"]) in
      Alcotest.(check int) "16" 16 (Rope.length r));
  ];

  "string_of", [
    Alcotest.test_case "single leaf" `Quick (fun () ->
      Alcotest.(check string) "hello" "hello" (Rope.string_of (leaf "hello")));

    Alcotest.test_case "concat preserves order" `Quick (fun () ->
      Alcotest.(check string) "foobar" "foobar"
        (Rope.string_of (Rope.concatenate (leaf "foo") (leaf "bar"))));

    Alcotest.test_case "build_rope preserves order" `Quick (fun () ->
      let r = Rope.build_rope (List.map leaf ["a"; "b"; "c"; "d"]) in
      Alcotest.(check string) "abcd" "abcd" (Rope.string_of r));

    Alcotest.test_case "preserves unicode" `Quick (fun () ->
      Alcotest.(check string) "emoji" "💀🌞"
        (Rope.string_of (Rope.concatenate (leaf "💀") (leaf "🌞"))));
  ];

  "char_at", [
    Alcotest.test_case "first char of leaf" `Quick (fun () ->
      Alcotest.(check int) "h" (Char.code 'h')
        (Uchar.to_int (Rope.char_at (leaf "hello") 0)));

    Alcotest.test_case "second char of leaf" `Quick (fun () ->
      Alcotest.(check int) "e" (Char.code 'e')
        (Uchar.to_int (Rope.char_at (leaf "hello") 1)));

    Alcotest.test_case "first char of right child" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foo") (leaf "bar") in
      Alcotest.(check int) "b" (Char.code 'b')
        (Uchar.to_int (Rope.char_at r 3)));

    Alcotest.test_case "non-first char of right child" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foo") (leaf "bar") in
      Alcotest.(check int) "a" (Char.code 'a')
        (Uchar.to_int (Rope.char_at r 4)));

    Alcotest.test_case "emoji in right child at index 0" `Quick (fun () ->
      let r = Rope.concatenate (leaf "a") (leaf "💀") in
      Alcotest.(check int) "skull" 0x1F480
        (Uchar.to_int (Rope.char_at r 1)));

    Alcotest.test_case "negative index raises" `Quick (fun () ->
      Alcotest.check_raises "negative" (Failure "ndx -1 outside [0..4]")
        (fun () -> ignore (Rope.char_at (leaf "hello") (-1))));

    Alcotest.test_case "out-of-bounds index raises" `Quick (fun () ->
      Alcotest.check_raises "oob" (Failure "ndx 5 outside [0..4]")
        (fun () -> ignore (Rope.char_at (leaf "hello") 5)));
  ];

  "insert", [
    Alcotest.test_case "insert at beginning" `Quick (fun () ->
      Alcotest.(check string) "hello world" "hello world"
        (Rope.string_of (Rope.insert (leaf "world") 0 "hello ")));

    Alcotest.test_case "insert in middle of leaf" `Quick (fun () ->
      Alcotest.(check string) "hello" "hello"
        (Rope.string_of (Rope.insert (leaf "helo") 2 "l")));

    Alcotest.test_case "insert at node boundary" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foo") (leaf "bar") in
      Alcotest.(check string) "foo-bar" "foo-bar"
        (Rope.string_of (Rope.insert r 3 "-")));

    Alcotest.test_case "insert into left subtree of node" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foobar") (leaf "baz") in
      Alcotest.(check string) "foo-barbaz" "foo-barbaz"
        (Rope.string_of (Rope.insert r 3 "-")));

    Alcotest.test_case "insert at end appends" `Quick (fun () ->
      Alcotest.(check string) "hi!" "hi!"
        (Rope.string_of (Rope.insert (leaf "hi") 2 "!")));
  ];

  "sub", [
    Alcotest.test_case "sub of leaf middle" `Quick (fun () ->
      Alcotest.(check string) "ell" "ell"
        (U.string_of (Rope.sub (leaf "hello") 1 3)));

    Alcotest.test_case "sub fully within left child" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foobar") (leaf "baz") in
      Alcotest.(check string) "oob" "oob"
        (U.string_of (Rope.sub r 1 3)));

    Alcotest.test_case "sub fully within right child" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foo") (leaf "barbaz") in
      Alcotest.(check string) "arb" "arb"
        (U.string_of (Rope.sub r 4 3)));

    Alcotest.test_case "sub spanning node boundary" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foo") (leaf "bar") in
      Alcotest.(check string) "ooba" "ooba"
        (U.string_of (Rope.sub r 1 4)));

    Alcotest.test_case "sub exceeding length raises" `Quick (fun () ->
      Alcotest.check_raises "oob"
        (Invalid_argument "(sub rope 3 3) exceeds rope length 5")
        (fun () -> ignore (Rope.sub (leaf "hello") 3 3)));
  ];

  "depth", [
    Alcotest.test_case "leaf has depth 1" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (Rope.depth (leaf "x")));

    Alcotest.test_case "single concat has depth 2" `Quick (fun () ->
      Alcotest.(check int) "2" 2
        (Rope.depth (Rope.concatenate (leaf "a") (leaf "b"))));

    Alcotest.test_case "build_rope of 8 is depth 4" `Quick (fun () ->
      let r = Rope.build_rope (List.init 8 (fun i -> leaf (string_of_int i))) in
      Alcotest.(check int) "4" 4 (Rope.depth r));
  ];

  "leaves_of", [
    Alcotest.test_case "leaf yields one leaf" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (List.length (Rope.leaves_of (leaf "abc"))));

    Alcotest.test_case "concat yields two leaves" `Quick (fun () ->
      Alcotest.(check int) "2" 2
        (List.length (Rope.leaves_of (Rope.concatenate (leaf "a") (leaf "b")))));

    Alcotest.test_case "build_rope of 4 yields 4 leaves" `Quick (fun () ->
      let r = Rope.build_rope (List.map leaf ["a"; "b"; "c"; "d"]) in
      Alcotest.(check int) "4" 4 (List.length (Rope.leaves_of r)));

    Alcotest.test_case "leaves content in order" `Quick (fun () ->
      let r = Rope.concatenate (leaf "foo") (leaf "bar") in
      let contents =
        List.map (function
          | Rope.Leaf s -> U.string_of s
          | Rope.Node _ -> failwith "unexpected node in leaves")
          (Rope.leaves_of r)
      in
      Alcotest.(check (list string)) "order" ["foo"; "bar"] contents);
  ];

  "properties", List.map QCheck_alcotest.to_alcotest prop_tests;
]
