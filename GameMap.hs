
module GameMap where
import System.Console.ANSI
import Data.List

import Block

newtype GameMap = GameMap {unGameMap::[[Char]]}

type Point = (Int, Int)
data Shape = Square Point Point Char 
           | Text Point [Char]
           | Group [Shape]

createGameMap l h c = GameMap $ createBlock l h c

clear = do 
    clearScreen
    setCursorPosition 0 0 

draw :: Shape -> GameMap -> GameMap
draw (Square (x, y) (x', y') c) (GameMap block) =
    GameMap (blockReplace (x, y) (x', y') c block)
draw (Group group) gm = (foldl (.) id $ map draw group) gm

render :: GameMap -> IO ()
render gm = do
    setCursorPosition 0 0 
    mapM_ putStrLn $ unGameMap gm
