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

data AppModel = AppModel {
  _clickCount :: Int,
  _turnS :: Int,
  _winner :: String,
  _boardE :: [RowBoardState]
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppTurnChange
  deriving (Eq, Show)

makeLenses 'AppModel

printText :: AppModel -> String
printText model
  | model ^. turnS == -1 = "Weclome"
  | model ^. turnS == 1 = "Red's turn"
  | model ^. turnS == 2 = "Blue's turn"
  | model ^. turnS == 3 = "Green's turn"
  | otherwise = model ^. winner ++ " wins"

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  colouredLabel :: BoardType -> WidgetNode s e
  colouredLabel ch 
    | ch == U = spacer
    | otherwise = label (T.pack $ show ch)
      `styleBasic` [styleIf (ch == R) (textColor red),
                    styleIf (ch == G) (textColor green),
                    styleIf (ch == B) (textColor blue),
                    styleIf (ch == E) (textColor white)]

  -- display an element in a row of the board
  makeOneState :: [Int] -> [BoardType] -> WidgetNode s e
  makeOneState [] rowState = label "\n"
  makeOneState (i:idx) rowState = hstack[
      colouredLabel $ rowState !! i,
      spacer,
      makeOneState idx rowState]

  makeRowState :: [Int] -> [Int] -> [RowBoardState] -> WidgetNode s e
  makeRowState _ [] boardState = label ""
  makeRowState rs (i:idx) boardState = vstack[
    makeOneState rs (boardState !! i),
    spacer,
    spacer,
    makeRowState rs idx boardState]
  -- display a row's elements of a board
  -- makeRowState idx boardState = hgrid $ zipWith makeOneState [0..] (boardState !! idx)

  widgetTree = vstack [
      box $ label_ (T.pack $ printText model) [ellipsis]
      `styleBasic` [textFont "Bold", textSize 40],
      spacer,
      box $ makeRowState [0..boardWidth-1] [0..boardHeight-1] (model ^. boardE)
      -- hstack [
      --   label $ "Click count: " <> showt (model ^. clickCount),
      --   spacer,
      --   button "Increase count" AppIncrease
      -- ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppIncrease -> [Model (model & clickCount +~ 1)]
  AppTurnChange -> []

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
      appInitEvent AppInit
      ]
    model = AppModel {
      _clickCount = 0,
      _turnS = -1,
      _winner = "",
      _boardE = myList
    }
