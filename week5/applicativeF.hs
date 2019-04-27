generateLists () =
    (\x y z -> [x,y,z])<$> [1..10] <*> [1..10] <*> [1..10]
filterLenghts triangles = 
  filter (\xs -> sum(xs) < 24) $ triangles
onlyRight triangles =
  filter (isRightAngle) triangles
tuplify3([x,y,z]) = (x,y,z)
isright (a,b,c) =
    a^2+b^2 == c^2
isRightAngle list =
     isright (tuplify3 list)
combination() =
  onlyRight $ filterLenghts $ generateLists()