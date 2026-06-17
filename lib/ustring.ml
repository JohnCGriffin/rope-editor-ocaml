
type t = { bytes:string; unicode_len:int }

let unicode_length (text:string) : int =
  let d = Uutf.decoder ~encoding:`UTF_8 (`String text) in
  let rec loop n =
    match Uutf.decode d with
    | `Uchar _     -> loop (n + 1)
    | `End         -> n
    | `Malformed _ -> failwith "bad utf8"
    | `Await       -> assert false
  in
  loop 0

let of_string str : t =
  { bytes=str; unicode_len=unicode_length str }

let string_of (ustr:t) = ustr.bytes

let length ustr : int = ustr.unicode_len

let get ustr pos : Uchar.t =
  let s = ustr.bytes in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec loop n =
    match Uutf.decode d with
    | `Uchar uc when n=0 -> uc
    | `Uchar _           -> loop (n-1)
    | `End               -> failwith "utf8_get given illegal pos"
    | `Malformed _       -> failwith "bad utf8"
    | `Await             -> assert false
  in
  loop pos

let sub ustr pos len : t =
  let s = ustr.bytes in
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let buf = Buffer.create (String.length s) in
  let rec loop n =
    if n >= pos + len then ()
    else
      match Uutf.decode d with
      | `Uchar uc ->
         if n >= pos then Uutf.Buffer.add_utf_8 buf uc;
         loop (n + 1)
      | `End         -> ()
      | `Malformed _ -> failwith "bad utf8"
      | `Await       -> assert false
  in
  loop 0;
  let bytes = Buffer.contents buf in
  let unicode_len = unicode_length bytes in
  { bytes; unicode_len }

let concatenate (ts:t list) : t = 
  let strings = List.map (fun x -> x.bytes) ts in
  let bytes = String.concat "" strings in
  let unicode_len = unicode_length bytes in
  { bytes; unicode_len }
    
