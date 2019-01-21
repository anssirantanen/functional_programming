
generate xs s f =
 filter (f)  [x| x <- sequence (replicate s xs)]


fun x  =
    True

