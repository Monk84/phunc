-- НОД
mgcd x y =
	let mygcd a b
		| a < 0 || b < 0 = 0
		| a == 0 = b
		| b == 0 = a
		| a == b = a
		| a > b = mygcd (a-b) b
		| otherwise = mygcd a (b-a)
	in
		mygcd x y

-- Функция delete :: Char -> String -> String, 
-- которая принимает на вход строку и символ и возвращает строку, 
-- в которой удалены все вхождения символа. 
-- Пример: delete ’l’ "Hello world!" должно возвращать "Heo word!".
delete' c [] = []
delete' c (s:ss)
	| s == c = delete' c ss
	| otherwise = ( s ) : ( delete' c ss )

-- Функция substitute :: Char -> Char -> String -> String, 
-- которая заменяет в строке указанный символ на заданный. 
-- Пример: substitute ’e’ ’i’ "eigenvalue" возвращает "iiginvalui"
substitute' cha chb [] = []
substitute' cha chb (s:ss)
  | s == chb = ( cha ) : ( substitute' cha chb ss )
  | otherwise = ( s ) : ( substitute' cha chb ss )

-- Числа Фибоначчи
fib n
	| n < ( -1 ) = ( fib ( n + 2 ) ) - ( fib ( n + 1 ) )
	| n == -1 = 1
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = ( fib ( n - 1 ) ) + ( fib ( n - 2 ) )

-- Обращение списка
reverse' [] = []
reverse' [ a ] = [ a ]
reverse' (s:ss) = ( reverse' ss ) ++ [ s ]

reverse'' list =
	let
		rev [] acc = acc
		rev (x:xs) acc = rev xs (x:acc)
	in
		rev list []

-- Подсчет баланса скобок в строке
brbal str = 
	let
		bal (s:ss) br
			| br < 0 = False
			| s == '(' = bal ss ( br + 1 )
			| s == ')' = bal ss ( br - 1 )
		bal [] br
			| br == 0 = True
			| otherwise = False
	in
		bal str 0

-- разворачивает списочную структуру и все списки в ней
reverseAll list = reverse' ( map reverse' list )

-- вовзращает позицию первого совпадающего с el элемента списка list
firstElem el list =
	let
		srch num el [] = 0
		srch num el [s] 
			| el == s = ( num + 1 )
 		srch num el (s:ss)
			| el == s = ( num + 1 )
			| otherwise = srch ( num + 1 ) el ss
	in
		srch 0 el list

-- set — функция, возвращающая список из всех атомов, 
-- содержащихся в заданном списке. Каждый атом должен 
-- присутствовать в результирующем списке в единственном числе.
set' list =
	let
		chck lst
			| null lst = lst
			| notElem ( head lst ) ( tail lst ) = ( head lst ) : chck ( tail lst )
			| otherwise = chck ( tail lst )
	in
		chck list

-- freq — функция, возвращающая список пар (символ, частота). 
-- Каждая пара определяет атом из заданного списка и частоту его вхождения в этот список.
freq' 
