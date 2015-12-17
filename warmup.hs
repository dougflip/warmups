-- quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lower) ++ [x] ++ (quicksort higher)
    where
        lower = filter (<x) xs
        higher = filter (>=x) xs

-- recurseLast
recurseLast :: [a] -> a
recurseLast [x] = x
recurseLast (x:xs) = recurseLast xs

-- myLast
myLast :: [a] -> a
myLast = head . reverse

-- recurseButLast
recurseButLast :: [a] -> a
recurseButLast [x,y] = x
recurseButLast (x:xs) = recurseButLast xs

-- myButLast
myButLast :: [a] -> a
myButLast = head . drop 1 . reverse

-- recurseElementAt
recurseElementAt :: [a] -> Int -> a
recurseElementAt [] _ = error "List is empty"
recurseElementAt (x:xs) 1 = x
recurseElementAt (x:xs) i = recurseElementAt xs (i-1)

-- elementAt
elementAt :: Int -> [a] -> a
elementAt i xs = head $ drop (i-1) xs

-- foldSum
foldSum :: [Integer] -> Integer
foldSum = foldl1 (+)
