import Data.List

-- permute - генерация всех перестановок списка
permute' :: [a] -> [[a]]
permute' [] = [[]]
permute' list = 
	let
		permutate target sample = map ( target !! ) sample
		
	in
		map (permutate list) 
-- ниасилил

-- concat - объединяет списочную структуру в единый список
-- не совсем понятно, как и для чего здесь привязать мемоизацию
concat' :: [[a]] -> [a]
concat' list =
	let
		conc res lst
			| null lst = res
			| otherwise = conc ( res ++ ( head lst ) ) ( tail lst )
	in
		conc [] list
