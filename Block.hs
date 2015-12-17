
module Block where

printBlock b = mapM_ putStrLn b

createBlock l h c = take h $ cycle [take l $ cycle [c]]

lineReplace (x, x') c line 
    | x' - x > 0 && x' <= length line && x >= 0 =
        let line_head = take x line
            line_tail = drop x' line 
        in line_head ++ (take (x'-x) (cycle [c])) ++ line_tail
    | otherwise = error "Error Point"

blockReplace (x, y) (x', y') c block
    | y' - y > 0 && y' <= length block && y >= 0=
        let block_head = take y block
            block_tail = drop y' block
            target_body = drop y . take y' $ block
        in block_head ++ (map (lineReplace (x, x') c) target_body) ++ block_tail
    | otherwise = error "Error Point"
