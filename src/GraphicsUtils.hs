module GraphicsUtils (drawFunction
                     ,drawCharacter
                     ,drawCharacterWXY
                     ,p,pc,fp) where
import SDL
import Character
import SDL.Video.Renderer
import Data.Maybe
import Foreign
import Control.Lens
import SDL.Video.Vulkan
import qualified SDL.Raw.Types as RT
import Linear (V2(..),V4(..))
import Control.Monad (unless)    
import Foreign.C.Types
import qualified Data.Vector.Storable as VecStor



drawFunction renderer f  = drawLines renderer v
    where 
        v = VecStor.generate 100 (\i_ -> let i = i_ -50 in pcr (fromIntegral (i*dx)) (fromIntegral (dy * f i) ))
        dx = 30
        dy = 1

drawCharacterWXY renderer _semi_w _x _y = fillRect renderer $ Just ( Rectangle (p  (x-semi_w) (y-semi_w) ) (V2 (CInt w) (CInt w) ) )
    where w = semi_w*2
          semi_w = fromIntegral _semi_w
          x = fromIntegral _x
          y = fromIntegral _y
drawCharacter renderer (Character  _x _y  _semi_w _)= fillRect renderer $ Just ( Rectangle (p  (x-semi_w) (y-semi_w) ) (V2 (CInt w) (CInt w) ) )
    where w = semi_w*2
          semi_w = fromIntegral _semi_w
          x = fromIntegral _x
          y = fromIntegral _y

pcr x y = (P (V2 (CInt (x+960) ) (CInt (-1*y+540))) )
p x y  = (P (V2 (CInt x) (CInt y)) )
pc x y = P (V2 x y)
fp x y  = (P (V2 (CFloat x) (CFloat y)) )