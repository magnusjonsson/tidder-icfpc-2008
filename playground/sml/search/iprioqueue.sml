functor ImperativePrioQueue (Item : ORD_KEY) = struct
  type item = Item.ord_key 
  type t = int ref * item option array ref
  fun make () : t = (ref 0, ref (Array.array (8, NONE)))
                    
  fun full (used, array) =
      !used = Array.length (!array)
  fun grow (used, array) =
      let 
          val oldArray = !array
          val oldLen = Array.length oldArray
          val newLen = 2*oldLen
          val newArray = Array.array (newLen, NONE)
      in
          Array.copy {src = oldArray, dst = newArray, di = 0 };
          array := newArray
      end
  fun insert (queue : t, item : item) : unit =
      (if full queue then grow queue else ();
       let
           val (used, array) = queue
           val a = !array
           fun bubbleUp 0 = Array.update(a, 0, SOME item)
             | bubbleUp i =
               let
                   val j = (i-1) div 2
                   val jitem = valOf (Array.sub(a, j))
               in
                   if Item.compare (item, jitem) = LESS then
                       (Array.update(a, i, SOME jitem);
                        bubbleUp j)
                   else
                       (Array.update(a, i, SOME item))
               end
       in
           bubbleUp(!used);
           used := !used + 1
       end)
  fun null (used, array) =
      !used = 0
  exception QueueEmpty
  fun top (queue : t) : item = 
      if null queue then raise QueueEmpty else
      let
          val (used, array) = queue
          val result = valOf (Array.sub (!array, 0))
      in
          result
      end
  fun drop (queue : t) : unit =
      if null queue then raise QueueEmpty else
      let
          val (used, array) = queue
          val a = !array
          val lastIndex = !used - 1
          val lastItem = valOf (Array.sub (a, lastIndex))
          fun bubbleDown (i, iv) =
              let
                  val j = 2*i+1
              in
                  if j >= lastIndex then 
                      Array.update(a, i, SOME iv)
                  else
                      let
                          val (j,jv) =
                              let
                                  val k = j+1
                              in
                                  if k >= lastIndex then
                                      let
                                          val jv = valOf (Array.sub(a, j))
                                      in
                                          (j, jv)
                                      end
                                  else
                                      let
                                          val jv = valOf (Array.sub(a, j))
                                          val kv = valOf (Array.sub(a, k))
                                      in
                                          if Item.compare (jv, kv) <> GREATER then 
                                              (j, jv)
                                          else
                                              (k, kv)
                                      end
                              end
                      in
                          if Item.compare (iv, jv) = GREATER then
                              (Array.update(a, i, SOME jv);
                               bubbleDown(j, iv))
                          else
                              Array.update(a, i, SOME iv)
                      end
              end
      in
          bubbleDown(0, lastItem);
          Array.update(a, lastIndex, NONE);  (* avoid stale pointer *)
          used := lastIndex
      end
  fun extract (queue : t) : item =
      let
          val result = top queue
      in
          drop queue;
          result
      end
end
