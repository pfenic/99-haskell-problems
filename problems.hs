

-- problem 1
myLast [] = Nothing 
myLast [x] = Just x
myLast (_:xs) = myLast xs


-- problem 2
myButLast [] = Nothing 
myButLast [x] = Nothing 
myButLast [x, _] = Just x
myButLast (_:xs) = myButLast xs


-- problem 3
elementAt [] _ = Nothing 
elementAt _ 0  = Nothing 
elementAt (x:_) 1 = Just x
elementAt (_:xs) i = elementAt xs (i - 1)


-- problem 4
myLength xs = myLength_acc xs 0 where
    myLength_acc [] acc = acc
    myLength_acc (_:ys) acc = myLength_acc ys (acc + 1)


-- problem 5
myReverse xs = myReverse_acc xs [] where
    myReverse_acc [] acc = acc
    myReverse_acc (y:ys) acc = myReverse_acc ys (y:acc)


-- problem 6
isPalindrome xs = xs == myReverse xs


-- problem 7
data NestedList a = Elem a | List [NestedList a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- problem 8
compress xs = compress_acc xs [] where
    compress_acc [] acc = acc
    compress_acc (y:ys) acc
        | contains acc y = compress_acc ys acc
        | otherwise = compress_acc ys (acc ++ [y])
        where
            contains [] e = False 
            contains (z:zs) e = e == z || contains zs e


-- problem 9
pack xs = pack_acc xs [] where
    pack_acc [] acc = acc
    pack_acc (y:ys) acc = pack_acc zs (acc ++ [subList]) where
        (zs, subList) = gather ys [y] where
            gather _ [] = error "accumulator may not be empty!"
            gather [] gather_acc = ([], gather_acc)
            gather (w:ws) (g:gather_acc)
                | w == g = gather ws (w:g:gather_acc)
                | otherwise = (w:ws, g:gather_acc)


-- problem 10
encode xs = encode_sublist (pack xs) where
    encode_sublist [] = []
    encode_sublist (y:ys) = (myLength y, head y):encode_sublist ys


-- problem 11
data Encoding a = Single a | Multiple Int a deriving Show
encodeModified xs = map (\(c, e) -> if c == 1 then Single e else Multiple c e) (encode xs)


-- problem 12


-- problem 13


-- problem 14


-- problem 15


-- problem 16


-- problem 17


-- problem 18


-- problem 19


-- problem 20