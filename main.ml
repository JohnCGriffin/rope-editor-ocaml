
open Curses
open Ropes
open Printf


let main w raw_bytes : unit =
  let text = Ustring.of_string raw_bytes in
  let multiple_texts = Ustring.split_with_nl text in
  let leaves = List.map (fun text -> Rope.Line text) multiple_texts in 
  let r = Ropes.Rope.build_rope leaves in
  ignore(wclear w);
  ignore(wmove w 0 0);
  ignore(waddstr w (Rope.string_of r));
  ignore(wmove w 0 40);
  ignore(waddstr w (sprintf "length of rope = %d" (Rope.line_count r)));
  ignore(wrefresh w);
  ignore (getch())

let () =
  let raw_bytes = if (Array.length (Sys.argv)) == 2
                  then let ic = open_in Sys.argv.(1) in
                       In_channel.input_all ic
                  else
                    "hello"
  in
  let win = Curses.initscr() in
  ignore(keypad win true);
  ignore(noecho());
  ignore(cbreak());
  main win raw_bytes;
  ignore(endwin())




