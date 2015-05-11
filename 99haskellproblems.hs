--Nikhil Khanna--
-- solutions to the 99 haskell problems http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems --
-- problem 1 --
myLast :: [a] -> a
myLast = last

--problem 2 --
myButLast :: [a] -> a
myButLast = last . init

--problem 3 --
elementAt :: [a] -> Int -> a
elementAt xs num = xs !! (num-1)

--problem 4--
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' xs = sum [1 | _ <- xs]

--problem 5 (obviously not using reverse which is kind of cheating--
myReverse :: [a] -> [a]
myReverse xs = myReverse' xs []
	where
		myReverse' [] reversed = reversed
		myReverse' (x:xs) reversed = myReverse' xs (x:reversed)

--problem 6 --
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

--problem 7 --
--TODO After reviewing data types for non-homogenous list

--problem 8 --
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

--problem 9 --
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile(== x) xs) : pack (dropWhile (== x) xs)

--problem 10 --
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = [(myLength' ys, head ys)| ys <- (pack xs)]

--problem 14 --
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

--problem 15 --
repeatNTimes :: a -> Int -> [a]
repeatNTimes _ 0 = []
repeatNTimes a n = a:repeatNTimes a (n-1)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) num = repeatNTimes	x num ++ repli xs num
