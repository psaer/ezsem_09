data WeirdPeanoNumber = Zero 
                      | Succ (WeirdPeanoNumber) 
                      | Pred (WeirdPeanoNumber)

wpnToInteger :: WeirdPeanoNumber -> Integer
wpnToInteger Zero = 0
wpnToInteger (Succ a) = 1 + (wpnToInteger a)
wpnToInteger (Pred a) = (wpnToInteger a) - 1

integerToWpn :: Integer -> WeirdPeanoNumber
integerToWpn x | x > 0 = Succ $ integerToWpn $ x - 1
           | x < 0 = Pred $ integerToWpn $ x + 1
           | otherwise = Zero

reduce :: WeirdPeanoNumber -> WeirdPeanoNumber
reduce Zero = Zero
reduce (Succ (Pred x)) = reduce x
reduce (Pred (Succ x)) = reduce x
reduce (Succ a) = case reduce a of (Pred b) -> b
                                   _ -> Succ $ reduce a
reduce (Pred a) = case reduce a of (Succ b) -> b
                                   _ -> Pred $ reduce a

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b    
    (==) (Pred a) (Pred b) = a == b
    (==) this other = False

instance Ord WeirdPeanoNumber where
    (<=) a b = reduce a `lessOrEqual` reduce b where
      lessOrEqual (Succ a) (Succ b) = lessOrEqual a b
      lessOrEqual (Pred a) (Pred b) = lessOrEqual a b
      lessOrEqual (Pred _) Zero = True
      lessOrEqual Zero (Succ _) = True
      lessOrEqual a b = a == b

instance Show WeirdPeanoNumber where
    show a = show (wpnToInteger a)

instance Num WeirdPeanoNumber where
    (+) Zero a = a
    (+) a Zero = a
    (+) (Pred a) b = (+) a (Pred b)
    (+) (Succ a) b = (+) a (Succ b)

    signum a = case reduce a of (Succ _) -> Succ Zero
                                (Pred _) -> Pred Zero
                                _        -> Zero

    negate Zero = Zero
    negate (Pred a) = Succ $ negate a
    negate (Succ a) = Pred $ negate a
	
    abs a = if signum a < Zero then negate a else a

    a * b = case signum b of (Succ Zero) -> a + a * (Pred b)
                             (Pred Zero) -> negate $ a * (abs b)
                             (Zero)      -> Zero

    fromInteger = integerToWpn

instance Enum WeirdPeanoNumber where
    toEnum = fromIntegral
    fromEnum = fromInteger.wpnToInteger

instance Real WeirdPeanoNumber where
    toRational x = toRational $ wpnToInteger x


instance Integral WeirdPeanoNumber where
    toInteger = wpnToInteger
    quotRem a b | signum a == signum b = result
                | otherwise = (negate $ fst result, snd result)
                   where absQuotRem res@(quot, rem) b | rem >= b = absQuotRem (quot + 1, rem - b) b
                                                      | otherwise = res
                         result = absQuotRem (Zero, abs a) (abs b)
           
test4 = show $ (integerToWpn 42) + fst ( quotRem (integerToWpn (-42)) (integerToWpn (-42)) ) - (integerToWpn 1)
test2 = show $ (integerToWpn 36) + fst ( quotRem (integerToWpn (-37)) (integerToWpn (6)) ) * (integerToWpn (-1))