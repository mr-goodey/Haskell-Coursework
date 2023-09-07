---- Part 1 ---- 

type Dog = (String, Int) 
--This creates a new type that I can use instead of writing (String, Int) everytime 

create_dog_list :: [String] -> [Int] -> [Dog] 
create_dog_list xs ys 
 |xs == [] || ys == [] = [] 
 |otherwise = [(head xs,head ys)] ++ create_dog_list (tail xs) (tail ys) 
--This creates a list of doggs based on two input lists of types [String] and [Int] 

mergesort'merge :: [(Int,String)] -> [(Int,String)] -> [(Int,String)] 
mergesort'merge [] xs = xs 
mergesort'merge xs [] = xs 
mergesort'merge (x:xs) (y:ys) 
    | (x < y) = x:mergesort'merge xs (y:ys) 
    | otherwise = y:mergesort'merge (x:xs) ys 
--This merges two lists into one 

mergesort'splitinhalf :: [(Int,String)] -> ([(Int,String)],[(Int,String)]) 
mergesort'splitinhalf xs = (take n xs, drop n xs) 
    where n = (length xs) `div` 2  
--This splits the list in half 

mergesort :: [(Int,String)] -> [(Int,String)] 
mergesort xs  
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs) 
    | otherwise = xs 
    where (ls, rs) = mergesort'splitinhalf xs 
--This executes the merge sort 

swap :: (a,b) -> (b,a) 
swap (a, b) = (b, a) 
--This swaps the tuple around 

tupleXTurn :: [(a,b)] -> [(b,a)] 
tupleXTurn xs = map swap xs 
--This swaps a list of tuples around 

sort_dog_list :: [Dog] -> [Dog] 
sort_dog_list xs = tupleXTurn(mergesort (tupleXTurn xs)) 
--This sorts the dog list in ascending order of height 

remove_smallest_dogs :: Int -> [Dog] -> [Dog] 
remove_smallest_dogs k xs = drop k (sort_dog_list xs) 
--This removes the k smallest dogs in the dog list 

remove_tall_dogs :: [Dog] -> [Dog] 
remove_tall_dogs xs = [a | a <- xs, snd a <= 80] 
--This removes all dogs above 80cm 

---- Part 2 ---- 

dup :: Int -> Char -> String 
dup n a 
 |n == 0 = [] 
 |otherwise = [a] ++ dup (n-1) a 
--This duplicates an item a certain number of times 

step1 :: Int -> Int -> [String] 
step1 a b 
 |a == 0 || b == 0 = [] 
 |otherwise = [(dup (a * b) ' ') ++ (dup (a * b) '*')] ++ step1 a (b-1) 
--This produces on line of each step in descending order 

step2 :: Int -> Int -> [String] 
step2 a b = reverse (step1 a b) ++ step1 a b 
--This concatenates the lists in the correct order 

step3 :: Int -> Int -> Int -> [String] 
step3 x a b = concatMap (replicate x) (step2 a b) 
--This replicates each item in the list a specific amount of times 

steps :: Int -> Int -> Int -> String 
steps x a b = unlines (step3 x a b) 
--This adds the \n to format the output correctly 

flagrow :: Int -> Int -> String 
flagrow a b 
 |a < 1 || a > b = [] 
 |a == 1 || a == b = dup b '*' 
 |b `mod` 2 == 0 && a <= b `div` 2 = "*" ++ dup (a - 2) ' ' ++ "+" ++ dup (b - (2 * a)) ' ' ++ "+" ++ dup (a - 2) ' ' ++ "*" 
 |b `mod` 2 == 0 && a > b `div` 2 = flagrow (b - a + 1) b 
 |b `mod` 2 /= 0 && a <= b `div` 2 = "*" ++ dup (a - 2) ' ' ++ "+" ++ dup (b - (2 * a)) ' ' ++ "+" ++ dup (a - 2) ' ' ++ "*" 
 |b `mod` 2 /= 0 && a == ((b `div` 2) + 1) = "*" ++ dup (a - 2) ' ' ++ "+" ++ dup (a - 2) ' ' ++ "*" 
 |b `mod` 2 /= 0 && a > ((b `div` 2) + 1) = flagrow (b - a + 1) b 
--This returns what a specific row of the flag looks like 

flagrows1 :: Int -> Int -> [String] 
flagrows1 a b 
 |a == 0 = [] 
 |otherwise = [flagrow a b] ++ flagrows1 (a - 1) b 
--This returns each flow of the flag 

flagrows2 :: Int -> Int -> String 
flagrows2 a b = unlines (flagrows1 a b) 
--This adds the /n to format the output correctly 

dup2 :: Int -> String -> String 
dup2 n a  
 |n == 0 = [] 
 |otherwise = a ++ dup2 (n - 1) a 
--This duplicates a string a specific amount of times 

flagpattern :: Int -> Int -> String 
flagpattern x y = dup2 y (flagrows2 x x) 
--This produces the flag a specific amount of times 

---- Part 3 ---- 

removeCommonLetters1 :: String -> String -> String 
removeCommonLetters1 xs ys = filter (\x -> x `notElem` ys) xs 
--This removes common letters from first string 

reverse1 :: String -> String -> String 
reverse1 xs ys = ys 
--This returns second string 

reverse2 :: String -> String -> String 
reverse2 xs ys = xs 
--This returns first string 

removeCommonLetters2 :: String -> String -> String 
removeCommonLetters2 xs ys = removeCommonLetters1 (reverse1 xs ys) (reverse2 xs ys) 
--This removes common letters from second string 

sublist1 :: String -> [String] 
sublist1 xs 
    |null xs = [] 
    |otherwise = take 4 xs:sublist1 (drop 4 xs) 
--This creates sub lists of size 4 

sublist2 :: String -> String 
sublist2 xs = last (sublist1 xs) 
--This returns the final sub list 

condition1 :: String -> String -> String 
condition1 xs ys 
 |length xs == 0 && length ys == 0 = "" 
 |length (sublist2 (removeCommonLetters1 xs ys)) == 1 = "likes" 
 |length (sublist2 (removeCommonLetters1 xs ys)) == 2 = "admires" 
 |length (sublist2 (removeCommonLetters1 xs ys)) == 3 = "hates" 
 |length (sublist2 (removeCommonLetters1 xs ys)) == 4 = "is indifferent to" 
--This returns a condition depending on the size of the final list (xs) 

condition2 :: String -> String -> String 
condition2 xs ys 
 |length xs == 0 && length ys == 0 = "" 
 |length (sublist2 (removeCommonLetters2 xs ys)) == 1 = "likes" 
 |length (sublist2 (removeCommonLetters2 xs ys)) == 2 = "admires" 
 |length (sublist2 (removeCommonLetters2 xs ys)) == 3 = "hates" 
 |length (sublist2 (removeCommonLetters2 xs ys)) == 4 = "is indifferent to" 
--This returns a condition depending on the size of the final list (ys) 

compatibility :: String -> String -> String 
compatibility xs ys 
 |length xs == 0 && length ys == 0 = "" 
 |xs == ys = xs ++ " is indifferent to " ++ ys ++ " and " ++ ys ++ " is indifferent to " ++ xs  
 |otherwise = xs ++ " " ++ condition1 xs ys ++ " " ++ ys ++ " and " ++ ys ++ " " ++ condition2 xs ys ++ " " ++ xs 
--This returns the desired message depending on the conditions 

---- Part 4 ---- 

remove :: Eq a => [a] -> a -> [a] 
remove xs x 
 |head xs == x = remove (tail xs) x 
 |otherwise = xs 
--This function removes the specified element from the list 

removed :: Eq a => [a] -> a -> Int 
removed xs x 
 |head xs == x = 1 + removed (tail xs) x 
 |otherwise = 0 
--This function calculates how many elements were removed 

split1 :: Eq a => [a] -> a -> [a] 
split1 xs x 
 |xs == [] = [] 
 |otherwise = takeWhile (/= x) (remove xs x) 
--This function splits the first valid interval of elements 

split2 :: Eq a => [a] -> a -> [[a]] 
split2 xs x  
 |xs == [] = [] 
 |otherwise = [split1 (split4 xs x) x] ++ split2 (drop (length (split1 xs x) + (removed xs x)) (split4 xs x)) x 
--This function creates a list of all valid intervals of elements 

split3 :: Eq a => [[a]] -> [Int] 
split3 xs 
 |xs == [] = [] 
 |otherwise = [length l | l <- xs] 
--This calculates the length of all valid intervals and lists them 

split4 :: Eq a => [a] -> a -> [a] 
split4 xs x 
 |last xs == x = split4 (init xs) x 
 |otherwise = xs 
--This function removes the chosen element from the end of the list 

nsplit :: Eq a => [a] -> a -> [Int] 
nsplit xs x = split3 (split2 xs x) 
--This function returns the lengths of all valid intervals 

main = putStrLn(flagpattern 5 1)
