-- P01 (*) Find the last element of a list.
my_last :: [a] -> a
my_last [] = error "Empty list"
my_last [x] = x
my_last (_:xs) = my_last xs


-- P02 (*) Find the last but one element of a list.
my_last_but_one :: [a] -> a
my_last_but_one [] = error "Empty list"
my_last_but_one [_] = error "Only one element"
my_last_but_one [x, _] = x
my_last_but_one [_, x, _] = x
my_last_but_one (_:xs) = my_last_but_one xs


-- P03 (*) Find the K'th element of a list.
kth :: [a] -> Int -> a
kth [] _ = error "Index out of bounds"
kth _ 0 = error "Zero index"
kth (x:xs) 1 = x
kth (_:xs) n
  | n < 1 = error "Index out of bounds"
  | otherwise = kth xs (pred n)
