{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow
import Board

import qualified Data.Text as T
import qualified Monomer.Lens as L
import Monomer.Main.Platform (initSDLWindow)
import GHC.Base (VecElem(Int16ElemRep))

-- the model representation that indicates the state of the application
-- information could be stored here that models the subject
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing, could also defines the start and end of the game
  _turnS :: Int,
  -- the string that represents the winning player's color
  _winner :: String,
  -- the representation of the board
  _boardE :: [[Int]],
  -- the players of the game
  _playersAmount :: Int
} deriving (Eq, Show)

-- the event type, representing different action the handler could react to
data AppEvent
  -- the initialize of the model status
  = AppInit
  -- the round change between players
  | AppTurnChange
  | ButtonClick
  | DoNothing
  deriving (Eq, Show)

makeLenses 'AppModel

-- produce the title text
printText :: AppModel -> String
printText model
  | model ^. turnS == -1 = "Weclome"
  | model ^. turnS == 1 = "Red's turn"
  | model ^. turnS == 2 = "Blue's turn"
  | model ^. turnS == 3 = "Green's turn"
  | otherwise = model ^. winner ++ " wins"

-- construct the user interface layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  -- render the color of different pieces on the board based on the labels
  colouredLabel :: Int -> WidgetNode s AppEvent
  colouredLabel ch
    | ch == 0 = spacer -- button "00" DoNothing `styleBasic` [radius 25]
    | otherwise = button_ (T.pack $ show ch) DoNothing [ellipsis]
                  `styleBasic` [radius 30, bgColor white,
                                styleIf (ch `elem` threePlayerFirst)(bgColor red),
                                styleIf (ch `elem` threePlayerSecond) (bgColor green),
                                styleIf (ch `elem` threePlayerThrid) (bgColor blue),
                                styleIf (ch `elem` (threePlayerFirst ++ threePlayerSecond ++ threePlayerThrid))(textColor white)
                                ]`nodeKey` showt ch

  -- -- display a row of elements on the board
  makeRowState :: [Int] -> WidgetNode s AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button

  -- call functions to render the application
  widgetTree = vstack [
      -- print the title
      -- box $ label_ (T.pack $ printText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 40],
      -- spacer,
      -- -- label $ "Click count: " <> showt (model ^. turnS),
      -- -- display the board
      -- box $ hstack[
      --   vstack[
      --     button "New Game" DoNothing,
      --     hstack[
      --       labeledRadio "3" 3 playersAmount,
      --       labeledRadio "4" 4 playersAmount,
      --       labeledRadio "6" 6 playersAmount
      --     ]
      --   ],
      --   spacer,
      --   button "End Game" DoNothing
      -- ],
      -- spacer,
      vgrid_ [childSpacing_ 11] (makeRowState <$> (model ^. boardE)) -- render the whole board state row by row
    ] `styleBasic` [padding 10]

-- declare how the events are handled respectively
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppTurnChange -> []
  ButtonClick -> [Model (model & turnS +~ 1)]
  DoNothing -> []

-- load the configuration options as well as define the initial state of the application
main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Program",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
      -- appWindowResizable False, -- disable resizing windows
      -- appWindowState $ MainWindowNormal (800, 700),
      appInitEvent AppInit
      ]
    -- provide an initial model of the application
    model = AppModel {
      _turnS = -1,
      _winner = "",
      _boardE = myList2,
      _playersAmount = 3
    }
