module U = Ropes.Ustring

let us s = U.of_string s
let str u = U.string_of u

let () = Alcotest.run "Ustring" [
  "of_string / string_of", [
    Alcotest.test_case "empty" `Quick (fun () ->
      Alcotest.(check string) "empty" "" (str (us "")));

    Alcotest.test_case "ascii roundtrip" `Quick (fun () ->
      Alcotest.(check string) "hello" "hello" (str (us "hello")));

    Alcotest.test_case "emoji roundtrip" `Quick (fun () ->
      Alcotest.(check string) "emoji" "💀🌞" (str (us "💀🌞")));

    Alcotest.test_case "mixed ascii and emoji" `Quick (fun () ->
      Alcotest.(check string) "mixed" "hi 💀 there" (str (us "hi 💀 there")));
  ];

  "length", [
    Alcotest.test_case "empty string is 0" `Quick (fun () ->
      Alcotest.(check int) "0" 0 (U.length (us "")));

    Alcotest.test_case "ascii counts codepoints" `Quick (fun () ->
      Alcotest.(check int) "5" 5 (U.length (us "hello")));

    Alcotest.test_case "skull emoji is 1 codepoint not 4 bytes" `Quick (fun () ->
      Alcotest.(check int) "1" 1 (U.length (us "💀")));

    Alcotest.test_case "emoji byte count exceeds codepoint count" `Quick (fun () ->
      let s = "💀" in
      Alcotest.(check bool) "bytes > codepoints" true
        (String.length s > U.length (us s)));

    Alcotest.test_case "mixed string codepoint count" `Quick (fun () ->
      Alcotest.(check int) "3" 3 (U.length (us "hi💀")));

    Alcotest.test_case "cafe is 4 codepoints" `Quick (fun () ->
      Alcotest.(check int) "4" 4 (U.length (us "café")));
  ];

  "get", [
    Alcotest.test_case "first char of ascii string" `Quick (fun () ->
      Alcotest.(check int) "h" (Char.code 'h')
        (Uchar.to_int (U.get (us "hello") 0)));

    Alcotest.test_case "second char of ascii string" `Quick (fun () ->
      Alcotest.(check int) "e" (Char.code 'e')
        (Uchar.to_int (U.get (us "hello") 1)));

    Alcotest.test_case "last char of ascii string" `Quick (fun () ->
      Alcotest.(check int) "o" (Char.code 'o')
        (Uchar.to_int (U.get (us "hello") 4)));

    Alcotest.test_case "emoji at index 0" `Quick (fun () ->
      Alcotest.(check int) "skull" 0x1F480
        (Uchar.to_int (U.get (us "💀") 0)));

    Alcotest.test_case "emoji at index 1 in mixed string" `Quick (fun () ->
      Alcotest.(check int) "skull" 0x1F480
        (Uchar.to_int (U.get (us "a💀b") 1)));

    Alcotest.test_case "ascii char after emoji" `Quick (fun () ->
      Alcotest.(check int) "b" (Char.code 'b')
        (Uchar.to_int (U.get (us "a💀b") 2)));
  ];

  "sub", [
    Alcotest.test_case "zero-length slice" `Quick (fun () ->
      Alcotest.(check string) "empty" "" (str (U.sub (us "hello") 0 0)));

    Alcotest.test_case "full string" `Quick (fun () ->
      Alcotest.(check string) "hello" "hello" (str (U.sub (us "hello") 0 5)));

    Alcotest.test_case "middle slice" `Quick (fun () ->
      Alcotest.(check string) "ell" "ell" (str (U.sub (us "hello") 1 3)));

    Alcotest.test_case "slice ending at last codepoint" `Quick (fun () ->
      Alcotest.(check string) "lo" "lo" (str (U.sub (us "hello") 3 2)));

    Alcotest.test_case "slice spanning emoji" `Quick (fun () ->
      Alcotest.(check string) "skull+b" "💀b" (str (U.sub (us "a💀b") 1 2)));

    Alcotest.test_case "single emoji extracted by sub" `Quick (fun () ->
      Alcotest.(check string) "skull" "💀" (str (U.sub (us "a💀b") 1 1)));
  ];

  "concatenate", [
    Alcotest.test_case "empty list yields empty string" `Quick (fun () ->
      Alcotest.(check string) "empty" "" (str (U.concatenate [])));

    Alcotest.test_case "singleton list" `Quick (fun () ->
      Alcotest.(check string) "hi" "hi" (str (U.concatenate [us "hi"])));

    Alcotest.test_case "two ascii strings" `Quick (fun () ->
      Alcotest.(check string) "foobar" "foobar"
        (str (U.concatenate [us "foo"; us "bar"])));

    Alcotest.test_case "three strings" `Quick (fun () ->
      Alcotest.(check string) "foobarbaz" "foobarbaz"
        (str (U.concatenate [us "foo"; us "bar"; us "baz"])));

    Alcotest.test_case "preserves unicode across parts" `Quick (fun () ->
      Alcotest.(check string) "emoji join" "☀️💀"
        (str (U.concatenate [us "☀️"; us "💀"])));

    Alcotest.test_case "length after concatenation" `Quick (fun () ->
      Alcotest.(check int) "9" 9
        (U.length (U.concatenate [us "foo"; us "bar"; us "baz"])));
  ];
]
