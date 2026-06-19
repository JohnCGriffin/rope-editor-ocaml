
open Curses
open Ropes
open Model


let main raw_bytes : unit =
  let text = Ustring.of_string raw_bytes in
  let multiple_texts = Ustring.split_with_nl text in
  let leaves = List.map (fun text -> Rope.Line text) multiple_texts in 
  let r = Ropes.Rope.build_rope leaves in
  let e:Model.t = { rope=r; loc=Rope.location_of 0 0; top_offset = 0 } in
  let rec loop e : unit =
    let e = View.view e in
    let ch = getch() in
    match keyname(ch) with
    | "KEY_DOWN" -> loop (move_down e)
    | "KEY_RIGHT" -> loop (move_right e)
    | "KEY_UP" -> loop (move_up e)
    | "KEY_LEFT" -> loop (move_left e)
    | "KEY_HOME" -> loop (move_top e)
    | "KEY_END" -> loop (move_bottom e)
    | "d" ->
       let windows = Windows.get() in
       let diags_width =
         if windows.settings.diags_width > 0 then
           0
         else
           30
       in
       Windows.set_diags_width diags_width;
       loop e
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
  main raw_bytes;
  let windows = Windows.get() in
  let elines,ecols = Curses.getmaxyx windows.edit_w in
  Windows.destroy ();
  ignore(endwin());
  Printf.printf "%d, %d\n" elines ecols;






