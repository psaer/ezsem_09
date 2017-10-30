newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
newtype PSet3 a = PSet3{ contains3 :: (a -> Bool) }

-- есть ли в одном из множеств элемент
-- (сложение множеств)
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet p1) (PSet p2) = PSet (\a -> p1 a || p2 a)

-- есть ли элемент в обоих множествах
-- (пересечение множеств)
instance Monoid (PSet2 a) where
    mempty = PSet2 (\x -> False)
    mappend (PSet2 x) (PSet2 y) = PSet2 (\z -> (&&) (x z) (y z))

-- разность множеств
instance Monoid (PSet3 a) where
  mempty = PSet3 (\a -> False)
  mappend (PSet3 f1) (PSet3 f2) = PSet3 (\a -> ((f1 a) && (not $ f2 a)) || ((not $ f1 a) && (f2 a)))

-- Результат всегда False, так как возможно преобразование лишь a->b
-- поэтому о множестве b ничего нельзя сказать
instance Functor PSet where
  fmap f (PSet fa) = PSet (\b -> False)