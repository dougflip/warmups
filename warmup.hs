-- quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lower) ++ [x] ++ (quicksort higher)
    where
        lower = filter (<x) xs
        higher = filter (>=x) xs

-- myLast
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- myButLast
myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

-- elementAt
elementAt :: [a] -> Int -> a
elementAt [] _ = error "List is empty"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)
