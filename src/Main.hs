module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import qualified SDL as SDL
import qualified SDL.Mixer as Audio


main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  Audio.withAudio Audio.defaultAudio 256 $ do
    coin1 <- Audio.load "assets/sounds/coin.wav"
    playIO (InWindow "Nice Window" (200, 200) (10, 10))
           white
           30
           ()
           draw
           (reactEvent coin1)
           reactTime

draw :: () -> IO Picture
draw () = pure $ circle 80

reactEvent :: Audio.Chunk -> Event -> () -> IO ()
reactEvent coin1 event () = case event of
  EventKey (Char 'q')          Down _ _ -> exitSuccess
  EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess
  EventKey _                   Down _ _ -> Audio.play coin1
  _                                     -> pure ()

reactTime :: Float -> () -> IO ()
reactTime _ () = pure ()
