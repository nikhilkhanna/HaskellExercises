import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Geometry

boomBang :: [Int]-> [[Char]]
boomBang xs = [if x <10 then "Boom" else "Bang" | x<-xs, odd x]

length' :: [a] -> Int
length' xs = sum[1 | _ <-xs]

removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

pow2 :: Int -> Int
pow2 x = 2^x

lucky :: Int->String
lucky 7 = "Lucky 7"
lucky x = "not seven "

factorial :: Int -> Int
factorial 0 = 1;
factorial n = n*factorial(n-1)

addVectors :: (Num a) => (a, a, a) -> (a,a ,a) -> (a,a,a)
addVectors (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:y:_) = "The first letter of " ++ all ++ " is " ++ [x] ++ " the second letter is "++[y]
capital x = "this is a problem..."

ageTell :: Int -> Int-> String
ageTell currentdate birthdate
	| age < young = "you're young"
	| age <middleAge = "middle age"
	| age <old = "old"
	| otherwise = "dead"
	where age = currentdate - birthdate
	      (young, middleAge, old) = (10, 30, 50)


compare' :: (Ord a) => a-> a-> Ordering
a `compare'` b
	| a > b = GT
	| a < b = LT
	| otherwise = EQ

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ "." ++ [l]++"."

calcAges :: [(Int,Int)] -> [Int]
calcAges dates = [age c b | (c,b)<-dates]
	where age current birth = current-birth

cube :: (RealFloat a) => a->a


cube side =
	let sideArea = side^2
	in sideArea*6

calcAges' :: [(Int,Int)] -> [Int]
calcAges' dates = [age | (c,b)<-dates, let age = c-b, age >3]


replicate' :: Int->a->[a]
replicate' n item
	|n<=0 = []
	|otherwise = item:(replicate' (n-1) item)

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n _
	| n <= 0 = []
take' n (first:items) = first: take' (n-1) items

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] ->[b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys


quicksort ::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [n | n <- xs, n<x]
        biggerSorted = quicksort [n | n <- xs, n>=x]
	in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a-> a-> a
multThree x y z = x*y*z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs 

flip' :: (a-> b -> c) -> (b-> a -> c)
flip' f y x = f x y 

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	|f x == True = x : filter' f xs
	|otherwise = filter' f xs


quicksort' ::(Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
	in smallerSorted ++ [x] ++ biggerSorted

largestDivisble :: (Integral a) => a
largestDivisble = head(filter p [100000, 99999..])
	where p x = x `mod` 3829 == 0

oddSquaresSum :: (Integral a) => a
oddSquaresSum = sum (takeWhile(<10000)([x^2| x <- [1..], odd(x^2)]))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| odd n = n:chain (n*3+1)
	| otherwise = n:chain (n`div`2)

numLongChains :: Int
numLongChains = length(filter isLong (map (chain) [1,2..100]))
	where isLong xs = length xs >15

myLast :: [a] -> a
myLast x = last x

myButLast :: [a] -> a
myButLast xs = reverse xs !! 1

sum' :: (Num a) => [a] -> a
sum' = foldl1 (+)

elem' :: (Eq a) => a-> [a] -> Bool
elem' x = foldl (\acc y->if x==y then True else acc) False

map' :: (a->b) -> [a] -> [b]
map' f xs = foldr(\y acc-> f y : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' xs = foldl1 (\acc y -> if y > acc then y else acc) xs

reverse' :: [a] -> [a]
reverse' = foldl (\acc y -> y:acc) []

filter'' :: (a->Bool) -> [a] -> [a]
filter'' f= foldr (\y acc -> if f y then y:acc else acc) [] 

head' :: [a] -> a
head' (x:xs) = x

last' :: [a] -> a
last' = foldr1(\_ acc->acc)

sqrtSum :: Int
sqrtSum = length(takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))+1

numUniques ::(Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
	let nlen = length needle
	in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

encode :: Int -> String -> String
encode shift msg = map (chr.(+shift).ord) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc-> if key == k then Just v else acc) Nothing

phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs 

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape->Float
surface (Circle _ r) = r^2*pi
surface (Rectangle (Point x1 y1)(Point x2 y2)) = (abs $ x1-x2)*(abs $ y1-y2)

move :: Shape -> Float -> Float -> Shape
move (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r 
move (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person { firstName :: String
					 , lastName :: String
					 , age :: Int
					 } deriving (Eq)

data Car = Car {company :: String
			   , model :: String
			   , year :: Int
			   } deriving (Show)  

data Vector a = Vector a a a deriving(Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProduct :: (Num t) => Vector t-> Vector t -> t
dotProduct (Vector i j k) (Vector l m n) = (i*l)+(j*m)+(k*n)
