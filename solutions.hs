import Data.Char

-- quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lower) ++ [x] ++ (quicksort higher)
    where
        lower = filter (<x) xs
        higher = filter (>=x) xs

-- mergesort
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = mergeTwoLists (mergesort qs) (mergesort rs)
    where
        p = (length xs) `quot` 2
        qs = take p xs
        rs = drop p xs

mergeTwoLists :: Ord a => [a] -> [a] -> [a]
mergeTwoLists xs [] = xs
mergeTwoLists [] ys = ys
mergeTwoLists (x:xs) (y:ys)
    | x <= y = x : mergeTwoLists xs (y:ys)
    | otherwise = y : mergeTwoLists (x:xs) ys

-- selectionSort
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = smallest : selectionSort unsorted
    where
        smallest = minimum xs
        unsorted = deleteFirst smallest xs

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst a (x:xs)
    | a == x = xs
    | otherwise = x : deleteFirst a xs

-- insertionSort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x < y = x:y:ys
    | otherwise = y : (insert x ys)

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
myButLast = head . reverse . init

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

-- toDigits
toDigits :: Int -> [Int]
toDigits = map digitToInt . show

-- doubleSecond
doubleSecond :: [Int] -> [Int]
doubleSecond = zipWith (\x y -> if even x then y*2 else y) [1..]
