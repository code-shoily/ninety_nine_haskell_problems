-- P01 (*) Find the last element of a list.
lastElem :: [a] -> a
lastElem [] = error "Empty list"
lastElem [x] = x
lastElem (_:xs) = lastElem xs


-- P02 (*) Find the last but one element of a list.
butLast :: [a] -> a
butLast [] = error "Empty list"
butLast [_] = error "Only one element"
butLast [x, _] = x
butLast [_, x, _] = x
butLast (_:xs) = butLast xs


-- P03 (*) Find the K'th element of a list.
kth :: [a] -> Int -> a
kth [] _ = error "Index out of bounds"
kth _ 0 = error "Zero index"
kth (x:xs) 1 = x
kth (_:xs) n
    | n < 1 = error "Index out of bounds"
    | otherwise = kth xs (pred n)

-- P04 (*) Find the number of elements of a list.
size :: [a] -> Int
size x = size' x 0 where
    size' [] n = n
    size' (x:xs) n = size' xs (succ n)

-- P05 (*) Reverse a list.
reversed :: [a] -> [a]
reversed x = reversed' x [] where
    reversed' [] xs = xs
    reversed' (x:xs) xss = reversed' xs (x:xss)

-- P06 (*) Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reversed xs)
