module Graph
    (drawGraph
     ) where

import Graphics.Gloss

drawGraph :: (Float -> Float) ->Picture
drawGraph f = axis <> graph
    where 
        graph = line $ map (\x -> (x,f x) ) [from,from+delta..to]
        from = -100
        to = 100
        delta = 0.1

axis = x <> y
    where 
        x = line  [(-100,0),(100,0)]
        y =  line  [(0,-100),(0,100)]
