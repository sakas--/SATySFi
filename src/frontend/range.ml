
type t = Dummy of string
       (* Normal(file_name, start_line, start_column, end_line, end_column) *)
       | Normal of string * int * int * int * int

let pp ppf rng =
  Format.fprintf ppf ""
(*
  match rng with
  | Dummy(msg)                   -> ppf "RangeDummy(%s)" msg
  | Normal(ln1, pos1, ln2, pos2) -> "Range"
*)

let show rng = ""


let dummy msg = Dummy(msg)


let is_dummy = function
  | Dummy(_) -> true
  | _        -> false


let message  = function
  | Dummy(msg)         -> msg
  | Normal(_, _, _, _, _) -> "*NORMAL*"


let to_string rng =
  let s = string_of_int in
    match rng with
    | Dummy(msg)                   -> "dummy range '" ^ msg ^ "'"
    | Normal(file_name, ln1, pos1, ln2, pos2) ->
        if ln1 = ln2 then
          file_name ^ " line " ^ (s ln1) ^ ", characters " ^ (s pos1) ^ "-" ^ (s pos2)
        else
          file_name ^ " line " ^ (s ln1) ^ ", character " ^ (s pos1) ^ " to line " ^ (s ln2) ^ ", character " ^ (s pos2)


let unite rng1 rng2 =
  match (rng1, rng2) with
  | (Normal(f1, ln1, pos1, _, _), Normal(f2, _, _, ln2, pos2)) -> assert(f1 = f2);  Normal(f1, ln1, pos1, ln2, pos2)
  | (Normal(_, _, _, _, _), _)                                 -> rng1
  | (_, Normal(_, _, _, _, _))                                 -> rng2
  | _                                                          -> Dummy("unite")


let make file_name ln pos1 pos2 = Normal(file_name, ln, pos1, ln, pos2)
