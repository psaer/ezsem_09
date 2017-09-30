nod :: Integral x => x -> x -> x
nod a 0 = a
nod a b = nod b (mod a b)

isSquareBetween :: Integer -> Integer -> Bool
isSquareBetween a b | a>b=(isSquareBetween b) a
                    | otherwise = (sqrt $ toDoube b) - (sqrt $ toDoube a ) > 1
                    where toDoube x = fromIntegral x::Double

myPow :: Integral x => x -> x -> x
myPow a b | b < 0 = error "Negative component"
          | b==0 = 1
          | b==1 = a
          | odd b = a * (myPow a) (b-1)
          | even b = (myPow (a*a)) (b `div` 2)