-- Functor & Applicative for Either
instance Functor (Either a) where
  fmap g (Right x) = Right (g x)
  fmap g (Left x) = Left x

instance Applicative (Either a) where
  pure x = Right x
  Left g <*> x = Left g
  Right g <*> x = fmap g x
