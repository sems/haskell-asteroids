module Buttons where

import Model
    (Button(Button),bContinueM,bContinueP,bCoop,bLeaderG,bLeaderM,bLeaderP,bMainG,bMainL,bMainP,bNewGame,bSingle,
    GameState(GameState,currentState,player1, player2,playerName), initialState,
    State(Main,Choose,Pause,GameOver,Leaderboard,Playing,GetName),
    Player(lives, time))
import Constants(bHeight)

import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey,MouseButton),
      KeyState( Down),
      MouseButton(LeftButton),
      SpecialKey(KeyEsc),
      Event(EventKey))

--replacing stateflow, for each state check whether one of it's buttons is pressed and update the gamestate accordingly
handleButtons :: Event -> GameState -> GameState 
handleButtons e gstate@(GameState Main _ _ _ _ _ _ _) = fromMain e gstate
handleButtons e gstate@(GameState Choose _ _ _ _ _ _ _) = fromChoose e gstate
handleButtons e gstate@(GameState Pause _ _ _ _ _ _ _) = fromPause e gstate
handleButtons e gstate@(GameState GameOver _ _ _ _ _ _ _) = fromGameover e gstate
handleButtons e gstate@(GameState Leaderboard _ _ _ _ _ _ _) = fromLeader e gstate
handleButtons (EventKey (SpecialKey KeyEsc) _ _ _) gstate@(GameState Playing _ _ _ _ _ _ _) = gstate {currentState = Pause}
handleButtons _ gstate = gstate


fromMain :: Event -> GameState -> GameState
fromMain e gstate | isClicked e bContinueM && isPlaying gstate = gstate{currentState = Playing}
                  | isClicked e bNewGame = initialState {currentState = GetName, playerName = ""} 
                  | isClicked e bLeaderM = gstate {currentState = Leaderboard}
                  | otherwise = gstate

isPlaying :: GameState -> Bool --bool whether a game has already started 
isPlaying gstate = (time $ player1 gstate) > 0
        
fromChoose :: Event -> GameState -> GameState
fromChoose e gstate | isClicked e bSingle = gstate {currentState = Playing, player2 = (player2 gstate){lives = 0}}
                    | isClicked e bCoop = gstate {currentState = Playing } 
                    | otherwise = gstate    

fromPause :: Event -> GameState -> GameState
fromPause e gstate | isClicked e bContinueP =  gstate {currentState = Playing} 
                   | isClicked e bMainP = gstate{currentState = Main}
                   | isClicked e bLeaderP = gstate{currentState = Leaderboard}
                   | otherwise = gstate

fromGameover :: Event -> GameState -> GameState
fromGameover e gstate | isClicked e bMainG = initialState
                      | isClicked e bLeaderG = initialState{currentState = Leaderboard}
                      | otherwise = gstate             

fromLeader :: Event -> GameState -> GameState
fromLeader e gstate | isClicked e bMainL = gstate{currentState = Main}
                    | otherwise = gstate


isClicked :: Event -> Button -> Bool -- checks whether a button was clicked
isClicked  (EventKey (MouseButton LeftButton) Down _ (posx, posy)) (Button width (x,y) _) = posx >= minx && posx <= maxx && posy >= miny && posy <= maxy
  where minx = x
        maxx = x + width
        miny = y
        maxy = y + bHeight
isClicked _ _ = False
