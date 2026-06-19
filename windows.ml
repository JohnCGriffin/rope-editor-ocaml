
open Curses

type window_settings = {
    diags_width   : int;
    numbers_width : int;
    status_height : int;
    entry_height  : int;
    lines         : int;
    cols          : int;
  }

type t = {
    edit_w    : window;
    status_w  : window;
    entry_w   : window;
    numbers_w : window;
    diags_w   : window;
    settings  : window_settings;
  }

let create settings : t =
  let edit_width = settings.cols - settings.diags_width - settings.numbers_width in
  let edit_height = settings.lines - settings.status_height - settings.entry_height in
  let edit_w = newwin edit_height edit_width 0 settings.numbers_width in
  let diags_w = if settings.diags_width > 0 then
                  newwin edit_height settings.diags_width 0 (settings.cols - settings.diags_width)
                else
                  newwin 0 0 (-1) (-1)
  in
  let numbers_w = if settings.numbers_width > 0 then
                    newwin edit_height settings.numbers_width 0 0
                  else
                    newwin 0 0 (-1) (-1)
  in
  let status_w = if settings.status_height > 0 then
                   newwin settings.status_height settings.cols edit_height 0
                 else
                   newwin 0 0 (-1) (-1)
  in
  let entry_w = if settings.entry_height > 0 then
                  newwin settings.entry_height settings.cols (edit_height + settings.status_height) 0
                else
                  newwin 0 0 (-1) (-1)
  in
  { edit_w; numbers_w; status_w; entry_w; diags_w; settings }
                

let default_settings =
  { diags_width = 0; numbers_width = 7; status_height = 1; entry_height = 1; lines = 0; cols = 0 }

let default_t =
  { edit_w = null_window;
    status_w = null_window;
    entry_w = null_window;
    numbers_w = null_window;
    diags_w = null_window;
    settings = default_settings }
  
let _current =
  ignore(Curses.setlocale Curses.lC_ALL "");
  let win = Curses.initscr() in
  ignore(keypad win true);
  ignore(noecho());
  ignore(cbreak());
  ignore(curs_set 1);
  let lines,cols = get_size () in
  let settings = { default_settings with lines; cols } in
  ref (create settings)
  

let destroy () : unit =
  let tmp = !_current in
  let windows = [ tmp.edit_w; tmp.status_w; tmp.entry_w; tmp.numbers_w; tmp.diags_w ] in
     let rec loop windows : unit =
       match windows with
       | [] -> ()
       | w::tail -> ignore(Curses.delwin w);
                    loop tail;
     in
     loop windows;
     _current := default_t
     

let get () : t = !_current

let set_diags_width width : unit =
  let settings = (!_current).settings in
  if width <> settings.diags_width then
    destroy();
    _current := create { settings with diags_width = width }

let set_numbers_width width : unit =
  let settings = (!_current).settings in
  if width <> settings.numbers_width then
    destroy();
  _current := create { settings with numbers_width = width }

let set_numbers_width_by_example num : unit =
  let text = Printf.sprintf "%d  " num in
  set_numbers_width (String.length text)

let refresh ~cursor_y ~cursor_x () : unit =
  let current = get() in
  let wins = [stdscr();
              current.diags_w; current.status_w; current.edit_w;
              current.numbers_w; current.entry_w ]
  in
  List.iter (fun w -> ignore(wnoutrefresh w)) wins;
  ignore(move cursor_y cursor_x);
  ignore(wnoutrefresh (stdscr ()));
  ignore(doupdate ())

  


  
