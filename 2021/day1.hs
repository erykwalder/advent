import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

countIncreases :: Ord a => [a] -> Int
countIncreases nums = length . filter (==True) $ zipWith (<) nums (tail nums)

countSlidingIncreases :: Ord a => [a] -> Int
countSlidingIncreases nums = length . filter (==True) $ zipWith (<) nums (tail $ tail $ tail nums)
  
main :: IO ()
main = do  
    handle <- openFile "./input1.txt" ReadMode
    content <- hGetContents handle
    let depthStrings = lines content
    let depths = map (read::String->Int) depthStrings
    putStr ((show . countIncreases $ depths) ++ "\n")
    putStr ((show . countSlidingIncreases $ depths) ++ "\n")
    hClose handle