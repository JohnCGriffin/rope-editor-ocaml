open Printf
open Curses
open Ropes


let without_nl text =
  let len = Ustring.length text in
  if len > 0 && Ustring.get text (len-1) = (Uchar.of_int 10) then
    Ustring.sub text 0 (len-1)
  else
    text

let view w (e:Model.t) : unit =
  let active_ndx = e.loc.line_offset in
  let line_count = Rope.line_count e.rope in
  let screen_lines,screen_cols = getmaxyx w in
  let top_ndx = max 0 active_ndx - e.top_offset in
  ignore(wclear w);
  let rec loop screen_line ndx : unit =
    if screen_line < screen_lines && ndx < line_count then (
      let loc = Model.location_of ndx 0 in
      let text = Rope.line_at e.rope loc in
      let text = Ustring.string_of text in
      ignore(wmove w screen_line 0);
      if ndx = active_ndx then
        ignore(waddstr w "> ");
      ignore(waddstr w text);
      loop (screen_line + 1) (ndx+1)
    )
  in
  loop 0 top_ndx;
  ignore(wmove w 0 (screen_cols-20));
  ignore(waddstr w (sprintf "[%d,%d]" e.loc.line_offset e.loc.char_offset))

