import Data.List (sort, group)
import Data.Array
import Data.Char (digitToInt)

nCells :: Int
nCells = 9*9

newtype Cell = Cell { unCell :: [Int] } deriving Eq
instance Show Cell where
        showsPrec _ (Cell [x]) = shows x
        showsPrec _ _          = (++ ".")
instance Read Cell where
        readsPrec _ ('.':xs) = [(Cell [1..9], xs)]
        readsPrec _ (x:xs) = [(Cell [digitToInt x], xs)]

isDeterminated :: Cell -> Bool
isDeterminated (Cell c) = length c == 1

newtype Board = Board { unBoard :: (Array Int Cell) } deriving Eq
instance Show Board where
        showsPrec _ (Board b) =
            let isHorEdge n = and [(n `mod` 3 == 0), (n `mod` 9 /= 0)]
                row s n = s ++ (if isHorEdge n then " | " else "") ++ show (b!n)
                afterRow n = if n `elem` [2, 5] then (take 15 $ repeat '-') ++ "\n" else ""
                board s n = s ++ foldl row "" (map (+(n*9)) [0..8]) ++ "\n" ++ afterRow n
            in  (++ foldl board "" [0..8])
instance Read Board where
        readsPrec _ _s =
            let s = filter (`elem` ".123456789") _s
                addcell l c = (read [c] :: Cell):l
                cells = (take nCells . reverse) (foldl addcell [] s)
            in  [(Board $ array (0,nCells-1) (zip [0..nCells-1] cells), drop nCells s)]

newtype Sudoku = Sudoku { unSudoku :: (Board, [Int]) } deriving Eq
instance Read Sudoku where
        readsPrec prec s =
            let [(Board b, s')] = readsPrec prec s :: [(Board, String)]
                l = filter (\i -> isDeterminated (b!i)) $ indices b
            in  [(Sudoku (Board b, l), s')]
instance Show Sudoku where
        showsPrec _ (Sudoku (b, xs)) = (++ show b ++ show xs)

elimina :: Int -> Board -> Sudoku
elimina x (Board b) =
    let vs = veins x
        n = head . unCell $ (b!x)
        changes = zip vs $ map Cell [filter (/= n) (unCell (b!v)) | v <- vs]
        b' = b // changes
        l' = filter (\v -> and [(not $ isDeterminated (b!v)), (isDeterminated (b'!v))]) vs
    in Sudoku (Board b', l')

propaga :: Sudoku -> Board
propaga (Sudoku (b, [])) =  b
propaga (Sudoku (b, x:xs)) = propaga $ Sudoku (b', xs ++ xs')
    where (Sudoku (b', xs')) = elimina x b

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
veins x = (filter (/= x) . rmdups) $ (fila x) ++ (columna x) ++ (quadrat x)

main = interact $ show . propaga . read
