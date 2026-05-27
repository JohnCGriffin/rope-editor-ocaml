type t

val of_string : string -> t
val string_of : t -> string
val length : t -> int
val get : t -> int -> Uchar.t
val sub : t -> int -> int -> t
val concatenate : t list -> t
