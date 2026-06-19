
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

val get : unit -> t
val set_diags_width : int -> unit
val set_numbers_width : int -> unit
val set_numbers_width_by_example : int -> unit
val destroy : unit -> unit
val refresh : cursor_y:int -> cursor_x:int -> unit -> unit
val acs : Acs.acs
