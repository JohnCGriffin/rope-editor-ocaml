
open Curses
open Ropes
open Printf

let split_with_nl (text:string) : (Rope.t list) = 
  let lines = ref ([]:Rope.t list) in
  let buf = Buffer.create 256 in
  for i=0 to String.length text - 1 do
    let ch = String.get text i in
    Buffer.add_char buf ch;
    if ch = (Char.chr 10) then (
      let u = Ustring.of_string (Buffer.contents buf) in
      let r = Rope.Leaf u in
      lines := r :: !lines;
      Buffer.clear buf
    );
  done;
  if Buffer.length buf > 0 then (
    let u = Ustring.of_string (Buffer.contents buf) in
    let r = Rope.Leaf u in
    lines := r :: !lines
  );
  List.rev !lines

let main w raw_bytes : unit =
  let lines = split_with_nl raw_bytes in
  let r = Ropes.Rope.build_rope lines in
  ignore(wclear w);
  ignore(wmove w 0 0);
  ignore(waddstr w (Rope.string_of r));
  ignore(wmove w 0 40);
  ignore(waddstr w (sprintf "length of rope = %d" (Rope.length r)));
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




