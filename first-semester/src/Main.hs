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

-- the model representation that indicates the state of the application
-- information could be stored here that models the subject
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing, could also defines the start and end of the game
  _turnS :: Int,
  -- the string that represents the winning player's color
  _winner :: String,
  -- the representation of the board
  _boardE :: [RowBoardState],
  -- the players of the game
  _playersAmount :: Int,
  sampleText :: Text
} deriving (Eq, Show)

-- the event type, representing different action the handler could react to
data AppEvent
  -- the initialize of the model status
  = AppInit
  -- the round change between players
  | AppTurnChange
  | ButtonClick
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
  colouredLabel :: BoardType -> WidgetNode s AppEvent
  colouredLabel ch
    | ch == U = spacer -- button "00" DoNothing `styleBasic` [radius 25]
    | otherwise = button_ (T.pack $ show $ getIndex ch) ButtonClick [ellipsis]
                  `styleBasic` [radius 30, bgColor white,
                                styleIf (isRed ch)(bgColor red),
                                styleIf (isBlue ch) (bgColor blue),
                                styleIf (isGreen ch) (bgColor green),
                                styleIf (isPurple ch)(bgColor purple),
                                styleIf (isOrange ch) (bgColor orange),
                                styleIf (isYellow ch) (bgColor yellow),
                                styleIf (isOccupied ch == Just True && not (isYellow ch))(textColor white)
                                ]--`nodeKey` showt ch

  -- -- display a row of elements on the board
  makeRowState :: RowBoardState -> WidgetNode s AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button

  -- call functions to render the application
  widgetTree = vstack [
      -- print the title
      box $ label_ (T.pack $ printText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 40],
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
      vgrid_ [childSpacing_ 5] (makeRowState <$> (model ^. boardE)) -- render the whole board state row by row
    ] `styleBasic` [padding 20]

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
  ButtonClick -> [] --[Model (model & turnS +~ 1)]
  

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
      -- appWindowState $ MainWindowNormal (850, 800),
      appInitEvent AppInit
      ]
    -- provide an initial model of the application
    model = AppModel {
      _turnS = -1,
      _winner = "",
      _boardE = externalBoard,
      _playersAmount = 3,
      sampleText = ""
    }
