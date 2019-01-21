
points::((Float,Float)->(Float,Float)-> Float)->
    Float ->(Float,Float) ->[(Float,Float)] -> [(Float,Float)]

points distanceF dist point pointA = 
      filter p pointA 
      where p x = (distanceF x point) < dist  
manhattan_filter = 
   points man_dist 
man_dist:: (Float,Float) -> (Float,Float) -> Float
man_dist  (x1,y1) (x2 ,y2) = 
   abs (x1 -x2) + abs(y1-y2)
