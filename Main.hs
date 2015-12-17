
module Main where

import System.IO
import System.Timeout
import System.Random
import Control.Concurrent
import Control.Concurrent.Async

import GameMap

data State = State {
        snake :: [Point],
        movement :: Point,
        fruit :: Maybe Point
    }

movePoint (x, y) (x', y') = (x+x', y+y') 
update state = State {snake=next_snake, movement=movement state, fruit=next_fruit}
    where next_point = (head $ snake state) `movePoint` (movement state)
          (next_snake, next_fruit) = 
              if Just next_point == fruit state
              then (next_point : (snake state), Nothing)
              else (next_point : (init $ snake state), fruit state)

drawSnake snake gm = 
    draw (Square (hx, hy) (hx+1, hy+1) '@') .
    draw (Group $ map (\(x,y)->Square (x,y) (x+1,y+1) '#') body ) $ gm 
        where (hx, hy) = head snake
              body = tail snake

drawFruit (Just (x, y)) gm =
    draw (Square (x, y) (x+1, y+1) '$') gm
drawFruit (Nothing) gm = gm

drawState state gm = drawFruit (fruit state) . drawSnake (snake state) $ gm

b_h = 10
b_l = 20
background = createGameMap b_l b_h '.'

start_state = State {
        snake = [(9, 4), (9, 5), (9, 6)],
        movement = (0, -1),
        fruit = Nothing
    }

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

frame :: Int
frame = oneSecond `div` 4

getInput :: IO Char
getInput = do 
    hSetEcho stdin False 
    hSetBuffering stdin NoBuffering
    getChar

sample :: Int -> IO a -> IO (Maybe a)
sample n f = do
    (result, _) <- concurrently (timeout n f) (threadDelay n) 
    return result

updateMovement char state = update' $ case char of
    Just 'w' -> (0, -1)
    Just 's' -> (0, 1)
    Just 'a' -> (-1, 0)
    Just 'd' -> (1, 0)
    _ -> movement state
    where update' p = if movePoint (movement state) p == (0, 0)
                      then state
                      else State {snake=snake state, movement=p, fruit=fruit state}

setFruit :: State -> IO State
setFruit state = case fruit state of 
    Just _ -> return state
    Nothing -> do
        x <- randomRIO (0, b_l-1)
        y <- randomRIO (0, b_h-1)
        return $ State {snake=snake state, movement=movement state, fruit=Just (x,y)}


gameOver state = outRange next_point || hitItsef next_point
    where next_point = head . snake $ state
          body = tail . snake $ state
          outRange (x, y) = x >= b_l || x < 0 || y >= b_h || y < 0
          hitItsef p = any (==p) $ body
                 

mainLoop :: State -> IO State
mainLoop state = 
    if gameOver state
    then return state
    else do
        state <- setFruit state
        render $ drawState state background
        char <- sample frame getInput
        if char == Just 'q'
        then return state
        else mainLoop $ update (updateMovement char state)

main :: IO ()
main = do
    clear
    state <- mainLoop start_state
    let score = (length . snake $ state) - (length . snake $ start_state)
    putStrLn $ "Game Over! Your score is " ++ show score
