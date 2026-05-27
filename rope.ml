
type rope = 
  | Leaf of string
  | Node of { left:rope;  right:rope; len:int; }

open Printf

let node_of left right len : rope =
  Node { left; right; len }

let length (r:rope) : int =
  match r with
  | Leaf s -> String.length s
  | Node n -> n.len

let rec depth (r:rope) : int =
  match r with
  | Leaf s -> 1
  | Node n -> 1 + max (depth n.left) (depth n.right)

let concatenate (left:rope) (right:rope)  =
  let len = (length left) + (length right) in
  Node { left; right; len }

let rec char_at (r:rope) (ndx:int) : char =
  if ndx < 0 || ndx >= length r then
    failwith (sprintf "ndx %d outside [0..%d]" ndx ((length r)-1));
  match r with
  | Leaf s -> String.get s ndx
  | Node n when ndx < (length n.left) -> char_at n.left ndx
  | Node n -> char_at n.right (ndx - length n.left)

let rec insert (r:rope) (ndx:int) (text:string) : rope =
  (** new top level rope *)
  if ndx < 0 || ndx >= length r then
    failwith (sprintf "ndx %d outside [0..%d]" ndx (length r));
  match r with
  | Leaf s ->
     let slen = String.length s in
     let left_part = String.sub s 0 ndx in
     let right_part = String.sub s ndx (slen-ndx) in
     Leaf (left_part ^ text ^ right_part)
  | Node n when ndx < (length n.left) ->
     let left = insert n.left ndx text in
     let right = n.right in
     let len = length left + length right in
     Node { left; right; len }
  | Node n ->
     let left = n.left in
     let right = insert n.right (ndx - length n.left) text in
     let len = length left + length right in
     Node { left; right; len }

let rec build_rope (ropes:rope list) : rope =
  match ropes with
  | [] -> Leaf ""
  | [single] -> single
  | _ -> 
     let rec aux (ropes:rope list) acc =
       match ropes with
       | [] -> List.rev acc
       | h::[] -> List.rev (h::acc)
       | l::r::t -> aux t ((concatenate l r)::acc)
     in
     build_rope (aux ropes [])
  
let leaves_of (r:rope) : (rope list) =
  let acc  = ref ([]:rope list) in
  let rec visit (r:rope) : unit =
    match r with
    | Leaf s -> acc := (Leaf s) :: !acc
    | Node n -> visit n.left; visit n.right
  in
  visit r;
  List.rev !acc

let string_of (r:rope) : string =
  let leaves = leaves_of r in
  let strs =
    List.map (fun r ->
        match r with
        | Leaf s -> s
        | Node _ -> failwith "unexpected non-Leaf in leaves")
      leaves
  in
  String.concat "" strs

let rec sub (r:rope) pos len : string =
  if pos + len > length r then
    invalid_arg (sprintf "(sub rope %d %d) exceeds rope length %d"
                   pos len (length r));

  match r with
  | (Leaf s) ->
     printf "choosing string %s\n" s;
     String.sub s pos len
  | (Node n) when (pos+len) <= (length n.left) ->
     printf "choosing left\n";
     sub n.left pos len
  | (Node n) when (length n.left) <= pos ->
     printf "choosing right\n";
     sub n.right (pos - (length n.left)) len
  | (Node n) ->
     printf "choosing both\n";
     let text = string_of (Node n) in
     String.sub text pos len
     
     
let leaves = List.map (fun s -> Leaf s)
               ["Once"; "upon"; "a"; "time"; "there";
                "were"; "three"; "little"; "girls" ]

let tree = build_rope leaves




