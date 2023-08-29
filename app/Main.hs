{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import GraphicsUtils
import Character
import Graph
import GridMap
import SDL
import SDL.Video.Renderer
import Data.Maybe
import Data.Complex
import Foreign
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
  rendererDrawColor renderer $= V4 150 150 250 0
  clear renderer
  rendererDrawColor renderer $= V4 250 250 50 255
  texture <- createTexture renderer RGBA8888 TextureAccessStatic (V2 (CInt 100) (CInt 100))
  rendererLogicalSize renderer $= Just (V2 (CInt 1920) (CInt 1080))
  rendLogicSize <- get . rendererLogicalSize $ renderer
  let v = fromMaybe (V2 (CInt 0) (CInt 0)) rendLogicSize
      w = v ^. _x `div` 2
      h = v ^. _y `div` 2
    in drawLine renderer (pc 0 h) (pc (w*2) h) 
       >> drawLine renderer (pc w 0) (pc w (h*2))
  drawFunction renderer (\x -> x*x)


  appLoop renderer [(Character {x = x, y = y, semi_w = 50 , state = (x :+ y)}) | x<-[100,200..2200] , y <- [100,200..1100]]
  destroyWindow window

appLoop :: Renderer -> [Character] -> IO ()
appLoop renderer chrs = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      eventIsFPress event = 
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeF  
            _ -> False      
      eventIsRPress event = 
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeR 
            _ -> False      
      eventIsEPress event = 
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeE 
            _ -> False      
      eventIsDPress event = 
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeD 
            _ -> False
        
      qPressed = any eventIsQPress events
      fPressed = any eventIsFPress events
      rPressed = any eventIsRPress events
      ePressed = any eventIsEPress events
      dPressed = any eventIsDPress events
      x' = 10*(fromEnum fPressed) - 10*(fromEnum ePressed) + x (head chrs)
      y' = -10*(fromEnum rPressed) + 10*(fromEnum dPressed) + y  (head chrs)
      semi_w' =  semi_w  (head chrs)
      chrs' = [(head chrs) {x = x',y = y'}] ++ tail chrs

  --rendererRenderTarget renderer $= Nothing
  
  mapM (\ ch ->  rendererDrawColor renderer $= character'sColor ch
          >>  drawCharacter renderer ch) (tail chrs')
  rendererDrawColor renderer $= character'sColor (head chrs')
  drawCharacter renderer (head chrs')
  present renderer
  rendererDrawColor renderer $= V4 150 150 250 255
  clear renderer
  unless qPressed (appLoop renderer  chrs')


