
open Curses
open Ropes
open Model

let update_windows (wins:window list) : unit =
  let rec loop wins =
    match wins with
    | [] -> ()
    | win::tail ->
       ignore(wnoutrefresh win);
       loop tail
  in
  loop wins

let main raw_bytes : unit =
  let edit_win = newwin 30 60 10 10 in
  let text = Ustring.of_string raw_bytes in
  let multiple_texts = Ustring.split_with_nl text in
  let leaves = List.map (fun text -> Rope.Line text) multiple_texts in 
  let r = Ropes.Rope.build_rope leaves in
  let e:Model.t = { rope=r; loc=Rope.location_of 0 0; top_offset = 0 } in
  let windows = [ stdscr (); edit_win ] in
  let rec loop e : unit =
    let top_offset = View.view edit_win e in
    let e = { e with top_offset } in
    ignore(update_windows windows);
    ignore(refresh());
    let ch = getch() in
    match keyname(ch) with
    | "KEY_DOWN" -> loop (move_down e)
    | "KEY_RIGHT" -> loop (move_right e)
    | "KEY_UP" -> loop (move_up e)
    | "KEY_LEFT" -> loop (move_left e)
    | _ -> ()
  in
  loop e


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
  ignore(curs_set 1);
  main raw_bytes;
  ignore(endwin())




