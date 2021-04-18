main::IO()
main = do
    print(countRats ")1)1)1) P" )
    print(countRats "P 1( 1( )1 1(")
    print (countRats "  P  1(   1(        )1      1(  ")
    print(countRats ")1)1)1)1P)1)11(")
    print(countRats "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(")
    --print(countRats "1( 1(  1( )1 )1 1( )1 1( )1 P )1 )1 1( )1 )1 1( 1( 1( )1 )1 )11(       1()1") -- my test
    print((josephus [1,2,3,4,5,6,7]) 3)
    print((josephus [1,2,3,4,5,6,7,8,9,10]) 1)
    print((josephus [1,2,3,4,5,6,7,8,9,10]) 2)
    print((josephus "fpFMIsu") 4)
    print((josephus [1,2,3,4,5,6,7]) (-1))

-- task 1

countRats :: String -> Int
countRats xs = countRatsHelper xs 0 0
    where countRatsHelper xs cnt cntForP -- list , a counter for the rats and counter for P
            |xs == [] && (cntForP == 0 || cntForP > 1)              = -1 -- for validation if there is a piper or more than one
            |xs == []                                               = cnt -- if the list is empty ,the counter is returned
            |not(head xs == 'P' || head xs == ' ' || head xs == '1' || head xs == '(' || head xs == ')') = -1 -- for  validation that there are only allowed characters
            |head xs == 'P'                                         = countRatsHelper (tail xs) cnt (cntForP+1) -- if P is found the cntForP is increased by 1
            |head xs == ')' && head(tail xs) == '1' && cntForP == 1 = countRatsHelper (tail (tail xs)) (cnt+1) cntForP -- if the rats are going right and P is already found, the cnt is increased by 1 because they are going the wrong direction
            |head xs == '1' && head(tail xs) == '(' && cntForP == 0 = countRatsHelper (tail (tail xs)) (cnt+1) cntForP -- if the rats are going left and P is not found ,the cnt is increased by 1 because they are going the wrong direction
            |otherwise                                              = countRatsHelper (tail xs) cnt cntForP -- if the rats are going the right direction ,the counter is not increased


-- task 2

myContains :: Eq a => a -> [a] -> Bool
myContains _ [] = False
myContains n xs = if head xs == n then True else myContains n (tail xs) -- function which check if list contains a specific element

 
removeElem1 :: Eq a => [a] -> Int -> [a]
removeElem1 [] _ = []          -- the helper function takes the list itself(xs), the Int n, cnt(1),copy of the list(xs),the length of the list (xs) and an empty list([])
removeElem1 xs n = removeElem1Helper xs n 1 xs (length xs)  [] -- ^
    where removeElem1Helper xs n cnt cpyXs size  ys
            |n < 1                                   = error "n is not natural"  -- validation for n if it is natural 
            |xs == []  && size == length ys          = reverse ys -- if the size of the original xs == size of ys ,then ys is returned and reversed
            |xs == []                                = removeElem1Helper cpyXs n cnt cpyXs size  ys -- if xs is empty and the size of xs /= size of ys,then  the copy is called in the fucntion so that the elements that have not been added to ys to be added 
            |myContains (head xs) ys                 = removeElem1Helper (tail xs) n cnt cpyXs size  ys -- if ys has the element head xs then the tail of xs is called without increasing the cnt
            |cnt == n                                = removeElem1Helper (tail xs) n (cnt-n+1) cpyXs size (head xs : ys) -- if cnt == n then the head xs is added to the list ys and counter is lowered to 1 again
            |otherwise                               = removeElem1Helper (tail xs) n (cnt+1) cpyXs size  ys -- if head xs is not the n-th element then the tail of xs is called and counter increased by 1


josephus :: Eq a => [a] -> (Int -> [a])
josephus xs = (\n -> removeElem1 xs n)
