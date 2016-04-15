module SimpleFunctions where
    filterFirst :: (a -> Bool) -> [a] -> [a]
    filterFirst p (x:xs) = 
        if (p x) then (:) x (filterFirst p xs) 
        else xs
    
    filterLast :: (a -> Bool) -> [a] -> [a]
    filterLast p xs = reverse (filterFirst p (reverse xs))
    
    split :: [a] -> ([a],[a])
    splitter _ [] = []
    splitter True (x:xs) = x:(splitter False xs)
    splitter False (x:xs) = (splitter True xs)
    split (x:xs) = ((splitter True (x:xs)),(splitter False (x:xs))) 

    interleave :: ([a],[a]) -> [a]
    interleave' [] [] = []
    interleave' [] x = x
    interleave' x [] = x
    interleave' (x:xs) (y:ys) = x:y:(interleave' xs ys)
    interleave (x,y) = interleave' x y


    quicksort []     = []
    quicksort (x:xs) = (quicksort floor) ++ [x] ++ (quicksort ceiling)
        where
            floor  = filter (< x) xs
            ceiling = filter (>= x) xs

    merge :: (Ord a) => ([a],[a]) -> [a]
    merge (x,y) = quicksort (x ++ y)


    mergeSort :: Ord a => [a] -> [a]
    mergeSort [] = []
    mergeSort [x] = [x]
    mergeSort xs  = merge ((mergeSort ceiling),(mergeSort floor))
        where
            (ceiling, floor) = splitAt (length xs `div` 2) xs
