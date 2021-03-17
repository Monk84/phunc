-- import

-- Список чисел Фибоначчи, от первого до n-го включительно
fib2 n =
	let
		fib n
			| n < ( -1 ) = ( fib ( n + 2 ) ) - ( fib ( n + 1 ) )
			| n == -1 = 1
			| n == 0 = 0
			| n == 1 = 1
			| otherwise = ( fib ( n - 1 ) ) + ( fib ( n - 2 ) )

		fibh list = list ++ [ fib $ (length list) + 1 ]
	in
		last $ take n $ iterate fibh [1]

-- Список треугольных чисел
triNums n =
	let
		triNum num res
			| num <= 0 = res
			| otherwise = triNum (num - 1) (res + num)
		trin' num = triNum num 0
	in
		map trin' ( take n $ iterate (+1) 1 )

-- Список пирамидальных чисел
pyrNums n =
	let
		pyrNum n = sum $ triNums n
	in
		map pyrNum ( take n $ iterate (+1) 1 )
