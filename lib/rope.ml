

type t = 
  | Leaf of Ustring.t
  | Node of { left:t;  right:t; len:int; }

            
open Printf

let node_of left right len : t =
  Node { left; right; len }

let length (r:t) : int =
  match r with
  | Leaf us -> (Ustring.length us)
  | Node n -> n.len

let rec depth (r:t) : int =
  match r with
  | Leaf _ -> 1
  | Node n -> 1 + max (depth n.left) (depth n.right)

let concatenate (left:t) (right:t)  =
  let len = (length left) + (length right) in
  Node { left; right; len }

let rec char_at (r:t) (ndx:int) : Uchar.t =
  if ndx < 0 || ndx >= length r then
    failwith (sprintf "ndx %d outside [0..%d]" ndx ((length r)-1));
  match r with
  | (Leaf s) -> Ustring.get s ndx
  | (Node n) when ndx < (length n.left) -> char_at n.left ndx
  | Node n -> char_at n.right (ndx - length n.left)

let rec insert (r:t) (ndx:int) (text:string) : t =
  if ndx < 0 || ndx > length r then
    failwith (sprintf "ndx %d outside [0..%d]" ndx (length r));
  match r with
  | (Leaf s) ->
     let slen = Ustring.length s in
     let left_part = Ustring.sub s 0 ndx in
     let right_part = Ustring.sub s ndx (slen-ndx) in
     Leaf (Ustring.concatenate [ left_part; Ustring.of_string(text); right_part ])
  | (Node n) when ndx < (length n.left) ->
     let left = insert n.left ndx text in
     let right = n.right in
     let len = length left + length right in
     Node { left; right; len }
  | (Node n) ->
     let left = n.left in
     let right = insert n.right (ndx - length n.left) text in
     let len = length left + length right in
     Node { left; right; len }

let rec build_rope (ropes:t list) : t =
  match ropes with
  | [] -> Leaf (Ustring.of_string(""))
  | [single] -> single
  | _ -> 
     let rec aux (ropes:t list) acc =
       match ropes with
       | [] -> List.rev acc
       | h::[] -> List.rev (h::acc)
       | l::r::t -> aux t ((concatenate l r)::acc)
     in
     build_rope (aux ropes [])
  
let leaves_of (r:t) : (t list) =
  let acc  = ref ([]:t list) in
  let rec visit (r:t) : unit =
    match r with
    | Leaf s -> acc := (Leaf s) :: !acc
    | Node n -> visit n.left; visit n.right
  in
  visit r;
  List.rev !acc

let string_of (r:t) : string =
  let leaves = leaves_of r in
  let strs =
    List.map (fun r ->
        match r with
        | Leaf s -> Ustring.string_of s
        | Node _ -> failwith "unexpected non-Leaf in leaves")
      leaves
  in
  String.concat "" strs

let rec sub (r:t) pos len : Ustring.t =
  if pos + len > length r then
    invalid_arg (sprintf "(sub rope %d %d) exceeds rope length %d"
                   pos len (length r));

  match r with
  | (Leaf s) ->
     Ustring.sub s pos len
  | (Node n) when (pos+len) <= (length n.left) ->
     sub n.left pos len
  | (Node n) when (length n.left) <= pos ->
     sub n.right (pos - (length n.left)) len
  | (Node n) ->
     let left_len = length n.left in
     let left_part  = sub n.left pos (left_len - pos) in
     let right_part = sub n.right 0 (pos + len - left_len) in
     Ustring.concatenate [left_part; right_part]
     

let leaves = List.map (fun text -> Leaf (Ustring.of_string text))
               ["Once "; "upon "; "a "; "time "; "there ";
                "were "; "three "; "little "; "girls scared of 💀. ";
                "However, the sun 🌞 came out and they were 👍!"]

let tree = build_rope leaves



