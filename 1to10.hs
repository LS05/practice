myLast :: [a] -> a
myLast [] = error "Lista Vuota"
myLast (x:xs) = reverse (x:xs) !! 0

myLastBut :: [a] -> a
myLastBut [] = error "Lista Vuota"
myLastBut (x:xs) = reverse (x:xs) !! 1

elementAt :: [a] -> Int -> a
elementAt (x:xs) n | n == 0 = "Posizione non corretta"
		   | n > 0 = (x:xs) !! (n - 1)	

myLength ::[a] -> Int
myLength xs = foldr (\_ x -> x + 1) 0 xs

myReverse :: [a] -> [a]
myReverse (x:xs) = reverse xs ++ [x]

isPalindrome ::(Eq a) => [a] -> Bool
isPalindrome xs = if xs == reverse xs then True else False

data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem a)   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

pack :: (Eq a) => [a] -> [[a]]
pack = foldr f []
       where f x [] = [[x]]
   	     f x (y:xs) = if x == (head y) then ((x:y):xs) else ([x]:y:xs)

encode::[[a]] -> [(Int, a)]
encode = map (\x -> (length x, head x))

dropEvery::[a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) n = dropEvery' (x:xs) n 1 where
    dropEvery' (x:xs) n i = (if (n `divides` i) then
        [] else
        [x])
        ++ (dropEvery' xs n (i+1))
    dropEvery' [] _ _ = []
    divides x y = y `mod` x == 0


split'::[a] -> Int -> ([a], [a])
split' (x:xs) n = (take n (x:xs), drop (n - 1) xs)

slice'::[a] -> Int -> Int -> [a]	  
slice' xs i k = take (k - y) (drop y xs)
		where y = i - 1

rotate'::[a] -> Int -> [a]
rotate' [] n = []
rotate' xs n | n > length xs = error "Indice non corretto"
	     | n <= length xs = rotateser (drop n xs) where
	       rotateser ys = ys ++ (take n xs)


removeAt::[a] -> Int -> (a, [a])
removeAt xs n = (xs !! n, take n xs ++ drop (n+1) xs)

insertAt::[a] -> a -> Int -> [a]
insertAt xs a n = (take n xs) ++ [a] ++ (drop n xs)

range:: Int -> Int -> [Int]
range x y = [x..y]