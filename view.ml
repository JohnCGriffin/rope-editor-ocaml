open Printf
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
  ignore(wmove w (screen_lines-1) 0);
  ignore(waddstr w (sprintf "requested = %d, sum=%d" requested (List.fold_left (+) 0 line_lengths)));
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


let view w (e:Model.t) : int =
  ignore(werase w);
  let active_ndx = e.loc.line_offset in
  let line_count = Rope.line_count e.rope in
  let screen_lines,screen_cols = getmaxyx w in
  let desired_top_lines = get_desired_top_lines w e |> max 0 |> min (screen_lines/2) in
  let top_ndx = max 0 (active_ndx - desired_top_lines) in
  let cursor_x = ref 0 in
  let cursor_y = ref 0 in
  let rec loop screen_line ndx : unit =
    if screen_line < screen_lines-1 && ndx < line_count then (
      let loc = Model.location_of ndx 0 in
      let utext = Rope.line_at e.rope loc |> without_nl in
      let stext = Ustring.string_of utext in
      if ndx = active_ndx then (
        ignore(wmove w screen_line 0);
        let subtext = Ustring.sub utext 0 e.loc.char_offset |> Ustring.string_of in
        ignore(waddstr w subtext);
        let y,x = getyx w in
        let offset_y, offset_x = getbegyx w in
        cursor_x := x + offset_x;
        cursor_y := y + offset_y;
      );
      ignore(wmove w screen_line 0);
      ignore(waddstr w stext);
      let y,_ = getyx w in
      loop (y+1) (ndx+1)
    )
  in
  loop 0 top_ndx;
  ignore(wmove w 0 (screen_cols-20));
  ignore(waddstr w (sprintf "[%d,%d,%d,%d]"
                      e.loc.line_offset e.loc.char_offset desired_top_lines e.top_offset));
  ignore(move !cursor_y !cursor_x);
  desired_top_lines

