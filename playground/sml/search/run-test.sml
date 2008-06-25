fun loop i =
    if i = 0 then ()
    else (AStarTest.test (253,252);
          loop (i-1))

val _ = loop 1
