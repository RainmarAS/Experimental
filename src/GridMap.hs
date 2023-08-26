module GridMap
    (drawGridMap
     ) where

import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

purple :: Int ->  [Word8]
purple i =  [0 , 0, 128, round $ max 0 f]
    where x = i `div` 100
          y = i `mod` 100
          f =  100 -  (fromIntegral $ x*x + y *y)**(1/4)*10  :: Float
           --(100 - (x * x + y* y) `div` 100)
n =  400000


bitmapData :: ByteString
bitmapData = pack $ concat $ map purple [0..n]

cell = bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) bitmapData True

drawGridMap :: Picture
drawGridMap = pictures [translate (x*10) (y*10) cell | x <-  [0..0],y <- [0..0]]




