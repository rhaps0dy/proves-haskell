newtype Taulell = [[ Int ]]
newtype Sudoku = (Taulell, [Int])

elimina :: Int -> Taulell -> Sudoku
elimina x t =
    let vs = veins x
        n = head (t !! x) -- t !! x conte un sol valor
        t' = map (\v -> if v `elem` vs then filter (/= n) (t !! v) else (t !! v))  [0..80]
        l' = filter (\v -> length (t !! v) == 1) vs
    in (t', l')
        
propaga :: Sudoku -> Taulell
propaga (t, []) =  t
propaga (t, x:xs) = propaga $ (t', xs ++ xs')
    where (t', xs') = elimina x t

fila :: Int -> [Int]
fila x = [a .. (a+8)]
    where a = (x `div` 9)*9

columna :: Int -> [Int]
columna x = map (+a) [0,9..80]
    where a = x `mod` 9

quadrat :: Int -> [Int]
quadrat x =
    let j = x `div` 9
        i = x `mod` 9
        j' = (j `div` 3)*3
        i' = (i `div` 3)*3
        x' = j' * 9 + i'
    in map (+ x') [0,1,2, 9,10,11, 18,19,20]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

veins :: Int -> [Int]
veins x :: (filter (/=x) . rmdups) $ (fila x) ++ (columna x) ++ (quadrat x)
