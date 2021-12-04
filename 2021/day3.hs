import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

mostCommonBits :: [[Int]] -> [Int]
mostCommonBits nums = map (\c -> if c*2 >= length nums then 1 else 0) counts
    where
        counts = foldl1 (zipWith (+)) nums

mostCommonMatch :: [[Int]] -> [Int]
mostCommonMatch nums = match nums 0
    where
        match [num] _ = num
        match nums bit = match (filter (sameBit bit mcbs) nums) (bit + 1)
            where mcbs = mostCommonBits nums

leastCommonMatch :: [[Int]] -> [Int]
leastCommonMatch nums = match nums 0
    where
        match [num] _ = num
        match nums bit = match (filter (not . sameBit bit mcbs) nums) (bit + 1)
            where mcbs = mostCommonBits nums

sameBit :: Int -> [Int] -> [Int] -> Bool
sameBit bit a b = a !! bit == b !! bit

bit :: Num p => Char -> p
bit '1' = 1
bit '0' = 0
bit _ = error "invalid bit"

invert :: [Int] -> [Int]
invert = map (1 -)

binToNum :: [Int] -> Int
binToNum = foldl (\n b -> b + 2*n) 0

main :: IO ()
main = do
    handle <- openFile "./input3.txt" ReadMode
    content <- hGetContents handle
    let nums = map (map bit) $ lines content
    let mcbs = mostCommonBits nums
    let gamma = binToNum mcbs
    let epsilon = binToNum . invert $ mcbs
    putStr ("gamma: " ++ show gamma ++ "\n")
    putStr ("epsilon: " ++ show epsilon ++ "\n")
    putStr ("product: " ++ show (gamma * epsilon) ++ "\n\n")

    let ox = binToNum . mostCommonMatch $ nums
    let co2 = binToNum . leastCommonMatch $ nums
    putStr ("ox: " ++ show ox ++ "\n")
    putStr ("co2: " ++ show co2 ++ "\n")
    putStr ("product: " ++ show (ox * co2) ++ "\n")
    hClose handle