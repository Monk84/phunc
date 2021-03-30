-- 3) Определить экземпляры классов типов Functor и Applicative для типа данных Fun.

-- 3
module Fun where
  newtype Fun a b = Fun { getFun :: a -> b }
  
  instance Functor (Fun a) where
    fmap f (Fun b) = Fun (f . b)
  
  instance Applicative (Fun a) where
    pure a = Fun (\x -> a)
	  (<*>) (Fun f) a = Fun (\x -> f x ((getFun a) x))
