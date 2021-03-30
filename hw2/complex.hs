-- import
-- Задание:
-- 1)	Разработать тип данных Complex для представления комплексных чисел.
-- Создать селекторы для значений разработанного типа данных realPart и imagPart,
-- которые возвращают действительную и мнимую части комплексного числа соответственно.
-- Complex должен быть экземпляром классов типов Eq и Show.
-- 2)	Создать реализацию класса типов Num для типа данных Complex

-- 1
data Complex a = Complex a a deriving (Eq, Show)

realPart :: (Complex a) -> a
realPart (Complex a b) = a

imagPart :: (Complex a) -> a
imagPart (Complex a b) = b

-- 2
instance (Floating a) => Num (Complex a) where
  Complex a b + Complex c d = Complex (a+c) (b+d)
  Complex a b - Complex c d = Complex (a-c) (b-d)
  Complex a b * Complex c d = Complex (a*c - b*d) (a*d + b*c)
  abs (Complex a b) = Complex (sqrt(a*a + b*b)) ( 0 )
  signum (Complex a b) = Complex ( a / sqrt(a*a + b*b)) ( b / sqrt(a*a + b*b) )
  fromInteger n = Complex (fromInteger n) (fromInteger 0)
