

-- problem 1
myLast [] = Nothing 
myLast [x] = Just x
myLast (_ : xs) = myLast xs


-- problem 2
myButLast [] = Nothing 
myButLast [x] = Nothing 
myButLast [x, _] = Just x
myButLast (_ : xs) = myButLast xs


-- problem 3
elementAt [] _ = Nothing 
elementAt _ 0  = Nothing 
elementAt (x : _) 1 = Just x
elementAt (_ : xs) i = elementAt xs (i - 1)


-- problem 4
myLength xs = len xs 0 where
    len [] i = i
    len (_ : ys) i = len ys (i + 1)


-- problem 5
myReverse xs = myRev xs [] where
    myRev [] os = os
    myRev (y : ys) os = myRev ys (y : os)


-- problem 6