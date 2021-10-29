module Main where

import Controller
import Model
import View
import Constants

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (screenWidth, screenHigth) (0, 0)) 
  black            -- Background color
  60               -- Frames per second
  initialState     -- Initial state
  view             -- View function
  input            -- Event function
  step             -- Step function