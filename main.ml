
open Curses
open Ropes
open Printf

type editor = {
    rope:Rope.t;
    loc:Rope.location;
    top_offset:int;
  }

let location_of lo co : Rope.location =
  { line_offset=lo; char_offset=co }
  

let without_nl text =
  let len = Ustring.length text in
  if len > 0 && Ustring.get text (len-1) = (Uchar.of_int 10) then
    Ustring.sub text 0 (len-1)
  else
    text

let view w (e:editor) : unit =
  let line_count = Rope.line_count e.rope in
  let lines,cols = get_size () in
  ignore(wclear w);
  for line = 0 to lines do
    let ndx = Rope.location_of line 0 in
    ignore(wmove w line 0);
    ignore(wclrtoeol w);
    if line < line_count then
      let text = Rope.line_at e.rope ndx in
      let text = without_nl text in
      let text = Ustring.string_of text in
      ignore(waddstr w text);
  done;
  ignore(wmove w 0 (cols-20));
  ignore(waddstr w (sprintf "[%d,%d]" e.loc.line_offset e.loc.char_offset))


let move_right (e:editor) : editor =
  let text = Rope.line_at e.rope e.loc in
  if e.loc.char_offset < (Ustring.length text)-1 then
    let loc = { e.loc with char_offset = e.loc.char_offset + 1 } in
    { e with loc }
  else
    e

let move_down (e:editor) : editor =
  let loc = location_of (e.loc.line_offset+1) 0 in
  { e with loc }

  


let main w raw_bytes : unit =
  let text = Ustring.of_string raw_bytes in
  let multiple_texts = Ustring.split_with_nl text in
  let leaves = List.map (fun text -> Rope.Line text) multiple_texts in 
  let r = Ropes.Rope.build_rope leaves in
  let e = { rope=r; loc=Rope.location_of 0 0; top_offset = 0 } in
  let rec loop e : unit =
    view w e;
    ignore(refresh());
    let ch = getch() in
    match keyname(ch) with
    | "KEY_DOWN" -> loop (move_down e)
    | "KEY_RIGHT" -> loop ( move_right e)
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
  main win raw_bytes;
  ignore(endwin())




