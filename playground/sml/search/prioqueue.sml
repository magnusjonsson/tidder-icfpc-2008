functor PrioQueue (Item : ORD_KEY) = struct
  datatype t = Nil | Branch of Item.ord_key * t * t
  val empty = Nil
  fun null queue = case queue of Nil => true
                               | _ => false
  fun insert (queue,item) =
      case queue of
          Nil => Branch (item, Nil, Nil)
        | Branch (root, left, right) =>
          if Item.compare (item,root) = LESS then
              Branch (item, right, insert (left,root))
          else
              Branch (root, right, insert (left,item))
  exception QueueEmpty
  fun top queue =
      case queue of Nil => raise QueueEmpty
                  | Branch (root,_,_) => root
  fun drop queue =
      case queue of
          Nil => raise QueueEmpty
        | Branch (root, Nil, right) => right
        | Branch (root, left, Nil) => left
        | Branch (root, left as Branch (lelt,_,_),
                        right as Branch (relt,_,_)) =>
          if Item.compare (lelt,relt) = LESS then
              Branch(lelt, drop left, right)
          else
              Branch(relt, drop right, left)
  fun extract queue = (top queue, drop queue)
end
