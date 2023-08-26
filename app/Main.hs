{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Graph
import GridMap
import SDL
import SDL.Video.Renderer
import Data.Maybe
import Foreign
import SDL.Raw.Video (getDisplayMode)
import Control.Lens
import SDL.Video.Vulkan
import qualified SDL.Raw.Types as RT
import Linear (V2(..),V4(..))
import Control.Monad (unless)

import Foreign.C.Types
import qualified Data.Vector.Storable as VecStor

winConf = WindowConfig { windowBorder          = True
                              , windowHighDPI         = False
                              , windowInputGrabbed    = False
                              , windowMode            = FullscreenDesktop
                              , windowGraphicsContext = NoGraphicsContext
                              , windowPosition        = Wherever
                              , windowResizable       = False
                              , windowInitialSize     = V2 1920 1080
                              , windowVisible         = True }

--mode = DisplayMode {}
mode = nullPtr :: Ptr RT.DisplayMode 

main = do
  initializeAll
  window <- createWindow "My SDL Application" winConf
  renderer <- createRenderer window 0 defaultRenderer
  --useless <- getDisplayMode (CInt 0) (CInt 0) mode 
  displays <- getDisplays 
  clear renderer
  rendererDrawColor renderer $= V4 150 150 250 0
  clear renderer
  rendererDrawColor renderer $= V4 50 50 250 255
  texture <- createTexture renderer RGBA8888 TextureAccessStatic (V2 (CInt 100) (CInt 100))
  --rendererScale renderer $= V2 (CFloat 1) (CFloat (1))
  rendererLogicalSize renderer $= Just (V2 (CInt 1920) (CInt 1080))
  rendLogicSize <- get . rendererLogicalSize $ renderer
  let v = fromMaybe (V2 (CInt 0) (CInt 0)) rendLogicSize
      w = v ^. _x `div` 2
      h = v ^. _y `div` 2
    in drawLine renderer (pc 0 h) (pc (w*2) h) 
       >> drawLine renderer (pc w 0) (pc w (h*2))

  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  --rendererRenderTarget renderer $= Nothing
  
  --
 {- let 
      --rect = Rectangle (p 0 0) (V2 500 500)
      --vert = Vertex (fp 100 100) (RT.Color 0 0 0 0) (fp 0 0)
      --verts = VS.singleton vert
      --alphas = VS.singleton (CInt 0)
      --v = VecStor.generate 100 (\i -> p (fromIntegral (i*i)) (fromIntegral i))
      --v = VecStor
    in drawLine renderer (p 0 300) (p 1000 300)-}
  --clear renderer
  present renderer
  
  unless qPressed (appLoop renderer)


p x y  = (P (V2 (CInt x) (CInt y)) )
pc x y = P (V2 x y)
fp x y  = (P (V2 (CFloat x) (CFloat y)) )