module Queue : sig
  type 'x t = private 'x list
  val empty : 'x t
  val add : 'x t -> 'x -> 'x t
  val take : 'x t -> ('x * 'x t) option
end = struct
  type 'x t = 'x list
  let empty = []
  let add q x = x :: q
  let take q = match List.rev q with
    | hd :: rest -> Some (hd, List.rev rest)
    | [] -> None
end
type 'x queue = 'x Queue.t
