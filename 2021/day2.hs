import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )

moveSub :: Num a => (a, a) -> ([Char], a) -> (a, a)
moveSub (x, y) ("forward", qty) = (x + qty, y)
moveSub (x, y) ("down", qty) = (x, y + qty)
moveSub (x, y) ("up", qty) = (x, y - qty)
moveSub (_, _) (_, _) = error "bad direction"

aimSub :: Num a => (a, a, a) -> ([Char], a) -> (a, a, a)
aimSub (x, y, aim) ("forward", qty) = (x + qty, y + (aim * qty), aim)
aimSub (x, y, aim) ("down", qty) = (x, y, aim + qty)
aimSub (x, y, aim) ("up", qty) = (x, y, aim - qty)
aimSub (_, _, _) (_, _) = error "bad direction"

main :: IO ()
main = do
    handle <- openFile "./input2.txt" ReadMode
    content <- hGetContents handle
    let directions = map ((\dir -> (head dir, read (last dir))) . words) $ lines content
    let pos1 = foldl moveSub (0, 0) directions
    let pos2 = foldl aimSub (0, 0, 0) directions
    print pos1
    let (x, y) = pos1 in print (x * y)
    print pos2
    let (x, y, _) = pos2 in print (x * y)
    hClose handle