module type S = functor (Ord : Map.OrderedType) -> sig
  type t
  val empty      : t
  val is_empty : t -> bool
  val insert     : Ord.t -> t -> t
  exception Queue_is_empty
  val top        : t -> Ord.t
  val remove_top : t -> t
  val extract    : t -> Ord.t * t
end;;

module Make : S = functor (Ord : Map.OrderedType) -> struct
  type t = Empty | Node of Ord.t * t * t
  let empty = Empty
  let is_empty = function Empty -> true | _ -> false
  let rec insert item queue =
    match queue with
      | Empty -> Node (item,Empty,Empty)
      | Node (root, left, right) ->
          (* we swap left and right below to balance the tree a bit *)
          if Ord.compare item root < 0
          then Node (item, right, insert root left)
          else Node (root, right, insert item left)
  exception Queue_is_empty
  let top queue =
    match queue with
      | Empty             -> raise Queue_is_empty
      | Node (root, _, _) -> root
  let rec remove_top queue =
    match queue with
      | Empty -> raise Queue_is_empty
      | Node (_, left, Empty) -> left
      | Node (_, Empty, right) -> right
      | Node (_, (Node (litem,_,_) as left), (Node (ritem,_,_) as right)) ->
          if Ord.compare litem ritem < 0
            (* since we always insert into the left subtree, make
               the left subtree smaller *)
          then Node(litem, remove_top left, right)
          else Node(ritem, remove_top right, left)
  let extract queue = (top queue, remove_top queue)
end
