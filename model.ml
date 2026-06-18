
open Ropes

type t = {
    rope       : Rope.t;
    loc        : Rope.location;
    top_offset : int;
  }

let location_of lo co : Rope.location =
  { line_offset=lo; char_offset=co }
  

let move_left (e:t) : t =
  if e.loc.char_offset > 0 then
    let char_offset = e.loc.char_offset - 1 in
    let loc = { e.loc with char_offset } in
    { e with loc }
  else
    e

let move_right (e:t) : t =
  let text = Rope.line_at e.rope e.loc in
  if e.loc.char_offset < (Ustring.length text)-1 then
    let loc = { e.loc with char_offset = e.loc.char_offset + 1 } in
    { e with loc }
  else
    e

let move_down (e:t) : t =
  if e.loc.line_offset < Rope.line_count e.rope - 1 then
    let loc = location_of (e.loc.line_offset+1) 0 in
    let top_offset = e.top_offset + 1 in
    { e with loc; top_offset }
  else
    e

let move_up (e:t) : t =
  if e.loc.line_offset > 0 then
    let loc = location_of (e.loc.line_offset-1) 0 in
    let top_offset = e.top_offset -1 in
    { e with loc; top_offset }
  else
    e
                
  
