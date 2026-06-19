
open Curses
open Ropes


let without_nl text =
  let len = Ustring.length text in
  if len > 0 && Ustring.get text (len-1) = (Uchar.of_int 10) then
    Ustring.sub text 0 (len-1)
  else
    text

let height_in_lines width text : int =
  1 + (Ustring.length text / width)

let get_desired_top_lines w (e:Model.t) : int =
  let _, screen_cols = getmaxyx w in
  let height_of = height_in_lines screen_cols in
  let active_ndx = e.loc.line_offset in
  let max_visual_rows = e.top_offset in
  let rec loop i visual_so_far =
    if i >= active_ndx then i
    else
      let loc = Rope.location_of (active_ndx - i - 1) 0 in
      let h = Rope.line_at e.rope loc |> height_of in
      if visual_so_far + h > max_visual_rows then i
      else loop (i+1) (visual_so_far + h)
  in
  loop 0 0


let view (e:Model.t) : Model.t =
  let windows = Windows.get() in
  let edit_w = windows.edit_w in
  let active_ndx = e.loc.line_offset in
  let line_count = Rope.line_count e.rope in
  let screen_lines,_ = getmaxyx edit_w in

  (* constrain e.top_offset between 0 and active line - 1 *)
  let top_offset = max 0 e.top_offset
                   |> min e.loc.line_offset
                   |> min (screen_lines-3)
  in
  let e = { e with top_offset } in
  
  let desired_top_lines = get_desired_top_lines edit_w e in
  let top_ndx = max 0 (active_ndx - desired_top_lines) in
  let cursor_x = ref 0 in
  let cursor_y = ref 0 in
  
  ignore(werase edit_w);
  let rec loop screen_line ndx : unit =
    if screen_line < screen_lines && ndx < line_count then (
      let loc = Model.location_of ndx 0 in
      let utext = Rope.line_at e.rope loc |> without_nl in
      let stext = Ustring.string_of utext in

      (* This is intended to draw the current line up to the
         char_offset, then remember the position for final
         placement of cursor *)
      if ndx = active_ndx then (
        ignore(wmove edit_w screen_line 0);
        let subtext = Ustring.sub utext 0 e.loc.char_offset |> Ustring.string_of in
        ignore(waddstr edit_w subtext);
        let y,x = getyx edit_w in
        let _,begin_x = getbegyx edit_w in
        cursor_x := x + begin_x;
        cursor_y := y;
      );
      
      ignore(wmove edit_w screen_line 0);
      ignore(waddstr edit_w stext);
      let y,_ = getyx edit_w in
      loop (y+1) (ndx+1)
    )
  in
  loop 0 top_ndx;
  let status_w = windows.status_w in

  ignore(wstandout status_w);
  ignore(werase status_w);
  ignore(wmove status_w 0 0);
  ignore(waddstr status_w (Printf.sprintf "location: %d %d" e.loc.line_offset e.loc.char_offset));
  ignore(wstandend status_w);

  Windows.refresh ~cursor_y:!cursor_y ~cursor_x:!cursor_x ();
  e

