data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber) | Pred (WeirdPeanoNumber)

wpnToInteger :: WeirdPeanoNumber -> Integer
wpnToInteger Zero = 0
wpnToInteger (Succ a) = 1 + (wpnToInteger a)
wpnToInteger (Pred a) = (wpnToInteger a) - 1

integerToWpn :: Integer -> WeirdPeanoNumber
integerToWpn x | x > 0 = Succ $ integerToWpn $ x - 1
           | x > 0 = Pred $ integerToWpn $ x + 1
           | otherwise = Zero

instance Eq WeirdPeanoNumber where
    (==) a b = ( wpnToInteger a) == (wpnToInteger b)

instance Ord WeirdPeanoNumber where
    (<=) a b = (wpnToInteger a) <= (wpnToInteger b)

instance Show WeirdPeanoNumber where
    show a = show $ wpnToInteger a

instance Num WeirdPeanoNumber where
    a + b = integerToWpn $ (wpnToInteger a) + (wpnToInteger b) 
    a * b = integerToWpn $ (wpnToInteger a) * (wpnToInteger b)
    abs = integerToWpn.abs.wpnToInteger
    signum = integerToWpn.signum.wpnToInteger
    negate = integerToWpn.negate.wpnToInteger
    fromInteger = integerToWpn

instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral
    fromEnum = fromInteger.wpnToInteger

instance Real WeirdPeanoNumber where
    toRational x = toRational $ wpnToInteger x

instance Integral WeirdPeanoNumber where
    toInteger = wpnToInteger
    quotRem x y = (integerToWpn a, integerToWpn b) 
             where (a, b) = quotRem (wpnToInteger x) (wpnToInteger y)
            
test1 = show $ (integerToWpn 2) + fst ( quotRem (integerToWpn 15) (integerToWpn 4) )
test2 = show $ (integerToWpn 2) - fst ( quotRem (integerToWpn 15) (integerToWpn 4) )
test3 = show $ (integerToWpn 2) * fst ( quotRem (integerToWpn 15) (integerToWpn 4) )