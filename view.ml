open Printf
open Curses
open Ropes


let without_nl text =
  let len = Ustring.length text in
  if len > 0 && Ustring.get text (len-1) = (Uchar.of_int 10) then
    Ustring.sub text 0 (len-1)
  else
    text

let height_in_lines width text =
  1 + Ustring.length text / width

let view w (e:Model.t) : unit =
  let active_ndx = e.loc.line_offset in
  let line_count = Rope.line_count e.rope in
  let screen_lines,screen_cols = getmaxyx w in
  let top_ndx = max 0 active_ndx - e.top_offset in
  let cursor_x = ref 0 in
  let cursor_y = ref 0 in
  ignore(wclear w);
  let rec loop screen_line ndx : unit =
    if screen_line < screen_lines && ndx < line_count then (
      let loc = Model.location_of ndx 0 in
      let utext = Rope.line_at e.rope loc in
      let stext = Ustring.string_of utext in
      ignore(wmove w screen_line 0);
      ignore(waddstr w stext);
      if ndx = active_ndx then (
        ignore(wmove w screen_line 0);
        let subtext = Ustring.sub utext 0 e.loc.char_offset |> Ustring.string_of in
        ignore(waddstr w subtext);
        let y,x = getyx w in
        cursor_x := x;
        cursor_y := y;
      );
      loop (screen_line + 1) (ndx+1)
    )
  in
  loop 0 top_ndx;
  ignore(wmove w 0 (screen_cols-20));
  ignore(waddstr w (sprintf "[%d,%d]" e.loc.line_offset e.loc.char_offset));
  ignore(wnoutrefresh w);
  ignore(move !cursor_y !cursor_x);

