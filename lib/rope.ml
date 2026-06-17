

type t = 
  | Line of Ustring.t
  | Node of { left:t;  right:t; line_count:int; char_count:int }

type location = { line_offset:int; char_offset:int }
            
open Printf

let location_of l c = { line_offset=l; char_offset=c }

let line_count (r:t) : int =
  match r with
  | Line _ -> 1
  | Node n -> n.line_count

let char_count (r:t) : int =
  match r with
  | Line ln -> Ustring.length ln
  | Node n -> n.char_count

let rec depth (r:t) : int =
  match r with
  | Line _ -> 1
  | Node n -> 1 + max (depth n.left) (depth n.right)

let node_of (left:t) (right:t) =
  let line_count = (line_count left) + (line_count right) in
  let char_count = (char_count left) + (char_count right) in
  Node { left; right; line_count; char_count }

let rec line_at r loc : Ustring.t =
  let ndx = loc.line_offset in
  if ndx < 0 || ndx >= (line_count r) then
    failwith (sprintf "ndx %d outside [0..%d]" ndx (line_count r));
  match r with
  | (Line text) -> text
  | (Node n) when ndx < (line_count n.left) -> line_at n.left loc
  | (Node n) -> line_at n.right { line_offset = ndx - line_count n.left; char_offset = 0}

let rec replace_line r line_offset sub_rope : t =
  match r with
  | Line _ -> sub_rope
  | (Node n) when line_offset < (line_count n.left) ->
     let right = n.right in
     let left = replace_line n.left line_offset sub_rope in
     let line_count = (line_count left) + (line_count right) in
     let char_count = (char_count left) + (char_count right) in
     Node { left; right; char_count; line_count }
  | (Node n) ->
     let right = replace_line n.right (line_offset - line_count n.left) sub_rope in
     let left = n.left in
     let line_count = (line_count left) + (line_count right) in
     let char_count = (char_count left) + (char_count right) in
     Node { left; right; char_count; line_count }

let rec build_rope (ropes:t list) : t =
  match ropes with
  | [] -> Line (Ustring.of_string(""))
  | [single] -> single
  | _ -> 
     let rec aux (ropes:t list) acc =
       match ropes with
       | [] -> List.rev acc
       | h::[] -> List.rev (h::acc)
       | l::r::t -> aux t ((node_of l r)::acc)
     in
     build_rope (aux ropes [])
  
let insert (r:t) loc (text:Ustring.t) =
  let old_text = line_at r loc in
  let left = Ustring.sub old_text 0 loc.char_offset in
  let right_len = (Ustring.length old_text) - loc.char_offset in
  let right = Ustring.sub old_text loc.char_offset right_len in
  let new_text = Ustring.concatenate [left; text; right] in
  let multiple_strings = Ustring.split_with_nl new_text in
  let ropes = List.map (fun text -> Line text) multiple_strings in
  let sub_rope = build_rope ropes in
  replace_line r loc.line_offset sub_rope

let leaves_of (r:t) : (t list) =
  let acc  = ref ([]:t list) in
  let rec visit (r:t) : unit =
    match r with
    | Line s -> acc := (Line s) :: !acc
    | Node n -> visit n.left; visit n.right
  in
  visit r;
  List.rev !acc

let string_of (r:t) : string =
  let leaves = leaves_of r in
  let strs =
    List.map (fun r ->
        match r with
        | Line s -> Ustring.string_of s
        | Node _ -> failwith "unexpected non-Leaf in leaves")
      leaves
  in
  String.concat "" strs


let leaves = List.map (fun text -> Line (Ustring.of_string text))
               ["Once "; "upon "; "a "; "time "; "there ";
                "were "; "three "; "little "; "girls scared of 💀. ";
                "However, the sun 🌞 came out and they were 👍!"]

let tree = build_rope leaves

