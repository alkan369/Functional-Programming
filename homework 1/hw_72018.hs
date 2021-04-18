main :: IO()
main = do
    print(safePrimeCount 20 100)
    print(safePrimeCount 1 983)
    print(safePrimeCount 167 1892)
    print(safePrimeCount 1678 20097)
    print(specialSum 3 20)
    print(specialSum 5 31)
    --print(specialSum 8 10)   -- takes too long to execute this example
    --print(specialSum 10 128) -- takes too long to execute this example
    print(validate 1714)
    print(validate 12345)
    print(validate 891)
    print(validate 123)
    print(validate 2121)
    print(validate 4736778291034)
    print(validate 4485756008412)
    print(validate 4485756008422)
    print(validate 4214154976719)



-- task 1
-- a)
isPrime :: Int -> Bool
isPrime n = [1,n] == [d | d <- [1..n], n `mod` d == 0] -- checks if a number is prime


safePrime :: Int -> Bool
safePrime n = isPrime ((n-1) `div` 2) && isPrime n -- checks if a number is safePrime

safePrimeCount :: Int -> Int -> Int
safePrimeCount a b = safePrimeCountHelper a b 0
    where safePrimeCountHelper a b cnt
            |a > b       = cnt -- returns the cnt
            |safePrime a  = safePrimeCountHelper (a + 1) b (cnt + 1)
            |otherwise    = safePrimeCountHelper (a + 1) b cnt


-- b)
specialNum :: Int -> Bool
specialNum n = specialNumHelper n 2 -- pow must be pow > 1
    where specialNumHelper n pow  
            |(2^pow) -1 == n &&  isPrime pow      = True
            |(2^pow) - 1 > n                      = False
            |otherwise                            = specialNumHelper n (pow+1)

specialSum :: Int -> Int -> Int      
specialSum k m = specialSumHelper k m (m+1) 0
    where specialSumHelper k m num sum -- we are searching numbers bigger than m , so num  starts from m+1 and eventually it will be added to sum
            |k == 0         = sum -- at the end ,the function returns the sum
            |specialNum num = specialSumHelper (k-1) m (num+1) (sum+num)
            |otherwise      = specialSumHelper k m (num+1) sum


--task 2

numberDigits :: Int ->Int
numberDigits n = if n < 10 then 1 else 1 + numberDigits (n `div` 10) -- function which counts the digits of a number

reverseNum :: Int -> Int
reverseNum n = reverseNumHelper n 0
    where reverseNumHelper n reverse -- function that reverses a number
            | n == 0 = reverse
            |otherwise = reverseNumHelper (n `div` 10) (reverse*10 + n `mod` 10)

sumOfNumbers:: Int ->Int
sumOfNumbers n = if n == 0 then 0 else n `mod` 10 + sumOfNumbers (n `div` 10)  -- sums all the numbers in a num          

validateNumbers :: Int -> Int
validateNumbers n = validateNumbersHelper n 0 1 -- cnt starts from 1 which means the most right digit of the number
    where validateNumbersHelper n cpy cnt -- cpy will eventually get the numbers from n after they have been modified but cpy will be reversed
            |n == 0  = sumOfNumbers (reverseNum (cpy)) -- so we have to use the reverse function for cpy in order to get the right number and also sum all the numbers of cpy
            |cnt `mod` 2 == 0 && 2*(n `mod` 10) > 9 = validateNumbersHelper (n `div` 10) (cpy*10 + (2*(n `mod` 10) `mod` 10) + ((2*(n `mod` 10)) `div` 10) `mod` 10 ) (cnt + 1) -- if the modified digit is above 9, the sums of the digits will be added to cpy 
            |cnt `mod` 2 == 0 = validateNumbersHelper (n `div` 10) (cpy*10 + 2*(n `mod` 10)) (cnt + 1) -- every second digit of n is modified and be added to cpy
            |otherwise = validateNumbersHelper (n `div` 10) (cpy*10 + (n `mod` 10)) (cnt + 1) -- the digit of n  will be added to cpy without being modified  


validate :: Int -> Bool
validate n = numberDigits n <= 16 && (validateNumbers n) `mod` 10 == 0

