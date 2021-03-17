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


-- Список пирамидальных чисел

