import Data.List (sort, group, intersperse, partition)
import Data.Array
import Data.Char (digitToInt)
import Data.Maybe
import Control.Monad

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

newtype Board = Board { unBoard :: Array Int Cell } deriving Eq
instance Show Board where
        showsPrec _ (Board b) =
            let isVerEdge n = (n `mod` 3 == 0 && n `mod` 9 /= 0)
                row s n = s ++ (if isVerEdge n then " | " else "") ++ show (b!n)
                afterRow n = if n `elem` [2, 5] then replicate 15 '-' ++ "\n" else ""
                board s n = s ++ foldl row "" (map (+(n*9)) [0..8]) ++ "\n" ++ afterRow n
            in  (++ foldl board "" [0..8])
instance Read Board where
        readsPrec _ _s =
            let s = filter (`elem` ".123456789") _s
                addcell l c = (read [c] :: Cell):l
                cells = (take nCells . reverse) (foldl addcell [] s)
            in  [(Board $ array (0,nCells-1) (zip [0..nCells-1] cells), drop nCells s)]

isComplete :: Board -> Bool
isComplete = all isDeterminated . elems . unBoard

-- the list in Sudoku contains the numbers that are newly determined,
-- and thus must be propagated
newtype Sudoku = Sudoku { unSudoku :: (Board, [Int]) } deriving Eq
instance Read Sudoku where
        readsPrec prec s =
            let [(Board b, s')] = readsPrec prec s :: [(Board, String)]
                l = filter (\i -> isDeterminated (b!i)) $ indices b
            in  [(Sudoku (Board b, l), s')]
instance Show Sudoku where
        showsPrec _ (Sudoku (b, xs)) = (++ show b ++ show xs)

eliminateCell :: Int -> Cell -> Maybe Cell
eliminateCell n (Cell c) = case filter (/= n) c of
  [] -> Nothing
  c' -> Just (Cell c')

eliminate :: Int -> Board -> Maybe Sudoku
eliminate x (Board b) =
    let vs = neighbors x
        elim = eliminateCell (head . unCell $ (b!x))
        b' = fmap ((b //) . zip vs) (sequence [elim (b!v) | v <- vs])
        regChanges = \arr -> Sudoku (Board arr, filter (\v -> not (isDeterminated (b!v)) && isDeterminated (arr!v)) vs)
    in fmap regChanges b'

propagate :: Sudoku -> Maybe Board
propagate (Sudoku (b, [])) =  Just b
propagate (Sudoku (b, x:xs)) = eliminate x b >>= (\(Sudoku (b', xs')) -> propagate (Sudoku (b', xs ++ xs')))

row :: Int -> [Int]
row x = [a .. (a+8)]
    where a = (x `div` 9)*9

column :: Int -> [Int]
column x = map (+a) [0,9..80]
    where a = x `mod` 9

square :: Int -> [Int]
square x =
    let j = x `div` 9
        i = x `mod` 9
        j' = (j `div` 3)*3
        i' = (i `div` 3)*3
        x' = j' * 9 + i'
    in map (+ x') [0,1,2, 9,10,11, 18,19,20]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

neighbors :: Int -> [Int]
neighbors x = (filter (/= x) . rmdups) $ row x ++ column x ++ square x

solve' :: Board -> Maybe Board
solve' (Board b)
    | isComplete (Board b) = Just (Board b)
    | otherwise            = listToMaybe . map fromJust $ possibilities
    where idx = head . filter (not . isDeterminated . (b!)) $ indices b
          arrs = [b // [(idx, Cell [n])] | n <- unCell (b!idx)]
          possibilities = filter (not . isNothing) . map (\a -> solve $ Sudoku (Board a, [idx])) $ arrs 
solve x = propagate x >>= solve'

solveSteps :: Sudoku -> [Board]
solveSteps s@(Sudoku (b, _)) = b:(if isNothing b' then [] else [fromJust b'])
    where b' = solve s

asciiArrow :: String
asciiArrow = lines5 ++ " ==> \n" ++ lines5
    where lines5 = concat (replicate 5 "     \n")

horCat :: String -> String -> String
horCat s1 s2 = foldl (\acc (a, b) -> acc ++ a ++ b ++ "\n") "" $ zip (lines s1) (lines s2)

horCat' :: [String] -> String
horCat' = foldr horCat (repeat '\n')

main = interact $ horCat' . intersperse asciiArrow . map show . solveSteps . read
