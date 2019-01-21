import Data.List

support xs k = take k (sortBy (sort_tuples) (to_tuples xs))

sort_tuples (_,n)(_,k)
    | n > k = LT
    | n < k = GT
    | n == k = EQ

to_tuples xs = (tuple_parts . group_parts) xs

tuple_parts xs = map (\v -> (head v, length v)) xs

group_parts xs = (group . sort)(flatten_parts xs)

flatten_parts xs =  concat (map_parts xs)


map_parts xs = map fs xs
     where fs = generate_sections []

generate_sections [] ys = generate_sections ((head ys : head (tail ys ): []):[]) (tail ys )
generate_sections xs [y] = xs
generate_sections xs ys = generate_sections (xs ++( (head ys : head (tail ys): [])):[])(tail ys)
