module Queue : sig
  type 'x t
  val empty : 'x t
  val add : 'x t -> 'x -> 'x t
  val take : 'x t -> ('x * 'x t) option
end = struct
  type 'x t = 'x list*'x list
  let empty = [], []
  let add (r,u) x = (x :: r, u)
  let rec take (r,u) = match u, r with
    | hd :: rest, _ -> Some (hd, (r, rest))
    | [], (_ :: _) -> take ([], List.rev r)
    | [], [] -> None
end
type 'x queue = 'x Queue.t
