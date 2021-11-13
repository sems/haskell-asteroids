module Main where

import Controller ( step, input )
import Model ( initialState )
import View ( view )

import Graphics.Gloss.Interface.IO.Game
    ( black, Display(FullScreen), playIO )

main :: IO ()
main = playIO FullScreen
  black            -- Background color
  60               -- Frames per second
  initialState     -- Initial state
  view             -- View function
  input            -- Event function
  step             -- Step function