
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
  let screen_lines,screen_cols = getmaxyx w in
  let height_of = height_in_lines screen_cols in
  let active_ndx = e.loc.line_offset in
  let requested = e.top_offset |> min 0 |> max (screen_lines/2) |> min active_ndx in
  let length_at i =
    let loc = Rope.location_of (active_ndx - i - 1) 0 in
    Rope.line_at e.rope loc |> height_of
  in
  let line_lengths = List.rev (List.init requested length_at) in
  let rec loop lengths =
    match lengths with
    | [] -> 0
    | _::tail ->
       let sum = List.fold_left (+) 0 line_lengths in
       if sum >= requested then
         List.length lengths
       else
         loop tail
  in
  loop line_lengths


let view (e:Model.t) : int =
  let windows = Windows.get() in
  let edit_w = windows.edit_w in
  ignore(werase edit_w);
  let active_ndx = e.loc.line_offset in
  let line_count = Rope.line_count e.rope in
  let screen_lines,_ = getmaxyx edit_w in
  let desired_top_lines = get_desired_top_lines edit_w e |> max 0 |> min (screen_lines/2) in
  let top_ndx = max 0 (active_ndx - desired_top_lines) in
  let cursor_x = ref 0 in
  let cursor_y = ref 0 in
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
  desired_top_lines

