{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- module Main where
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
    ( nodeKey,
      nodeVisible,
      darkTheme,
      blue,
      green,
      orange,
      purple,
      red,
      white,
      yellow,
      startApp,
      appFontDef,
      appInitEvent,
      appTheme,
      appWindowIcon,
      appWindowTitle,
      styleIf,
      box,
      box_,
      hgrid,
      vgrid_,
      vstack,
      button,
      button_,
      label_,
      labeledRadio,
      filler,
      spacer,
      CmbAlignLeft(alignLeft),
      CmbBgColor(bgColor),
      CmbChildSpacing(childSpacing_),
      CmbEllipsis(ellipsis),
      CmbPadding(padding),
      CmbRadius(radius),
      CmbStyleBasic(styleBasic),
      CmbTextColor(textColor),
      CmbTextFont(textFont),
      CmbTextSize(textSize),
      WidgetEnv,
      WidgetNode,
      AppEventResponse,
      EventResponse(Model), nodeInfoFromKey, label, CmbMultiline (multiline) )
import TextShow
import Board

import qualified Data.Text as T
import qualified Monomer.Lens as L

-- the model representation that indicates the state of the application
-- information could be stored here that models the subject
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing, could also defines the start and end of the game
  -- 0 means the first player in the list
  _turnS :: Int,
  _startGame :: Bool,
  -- the string that represents the winning player's color
  _winner :: String,
  -- the representation of the board
  _boardE :: Board,
  -- the players of the game
  _playersAmount :: Int,
  _fromIndex :: Int,
  _toIndex :: Int,
  _errorMessage :: String
} deriving (Eq, Show)

-- the event type, representing different action the handler could react to
data AppEvent
  -- the initialize of the model status
  = AppInit
  -- the round change between players
  | AppTurnChange BoardType
  | StartGameButtonClick
  | DoNothing
  | EndGameButtonClick
  deriving (Eq, Show)

makeLenses 'AppModel

-- produce the title text
-- if the game does not start then just print welcome title
-- if an error is made then print it
-- finally, if everything is normal and the game is started, then print the turn
titleText :: AppModel -> String
titleText model
  | not (model ^. startGame) = "Haskell Chinese Checkers"
  | otherwise = show (turnText model) ++ "'s turn"

turnText :: AppModel -> Colour
turnText model
  | model ^. playersAmount == 3 = threePlayersSet !! turn
  | model ^. playersAmount == 4 = fourPlayersSet !! turn
  | otherwise = sixPlayersSet !! turn
  where turn = model ^. turnS

printHint :: AppModel -> String
printHint model
  | null (model ^. errorMessage) = "From " ++ printIndex (model ^. fromIndex) ++ " to " ++ printIndex (model ^. toIndex)
  | otherwise = "From " ++ printIndex (model ^. fromIndex) ++ " to " ++ printIndex (model ^. toIndex) ++ ", " ++ model ^. errorMessage
    where
      printIndex :: Int -> String
      printIndex idx = if idx /= -1 then show idx else " "

-- construct the user interface layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  -- render the color of different pieces on the board based on the labels
  colouredLabel :: BoardType -> WidgetNode s AppEvent
  colouredLabel ch
    | isNothing(isOccupied ch) = spacer
    | otherwise = button_ (T.pack $ show $ getIndex ch) (AppTurnChange ch ) [ellipsis]
                  `styleBasic` [radius 30, bgColor white,
                                styleIf (compareColour ch Red)(bgColor red),
                                styleIf (compareColour ch Blue) (bgColor blue),
                                styleIf (compareColour ch Green) (bgColor green),
                                styleIf (compareColour ch Purple)(bgColor purple),
                                styleIf (compareColour ch Orange) (bgColor orange),
                                styleIf (compareColour ch Yellow) (bgColor yellow),
                                styleIf (isOccupied ch == Just True && not (compareColour ch Yellow))(textColor white)
                                ]

  -- -- display a row of elements on the board
  makeRowState :: [BoardType] -> WidgetNode s AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button

  -- call functions to render the application
  widgetTree = vstack [
      box $ label_ (T.pack $ titleText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],
      spacer,
      box $ label_ (T.pack $ printHint model) [ellipsis] `styleBasic` [textFont "Italic", textSize 20] `nodeVisible` model ^. startGame,
      filler,
      box_ [alignLeft] $ button "Quit Game" EndGameButtonClick `styleBasic`[textSize 20] `nodeVisible` model ^. startGame,
      box $ vstack[
        hgrid[
          labeledRadio "3" 3 playersAmount `styleBasic`[textSize 30],
          spacer,
          labeledRadio "4" 4 playersAmount `styleBasic`[textSize 30],
          spacer,
          labeledRadio "6" 6 playersAmount `styleBasic`[textSize 30]
        ],
        spacer,
        button "New Game" StartGameButtonClick `styleBasic`[textSize 50]
      ] `nodeVisible` not (model ^. startGame),
      vgrid_ [childSpacing_ 5] (makeRowState <$> (model ^. boardE)) `nodeVisible` (model ^. startGame),
      filler
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
  StartGameButtonClick
    | model ^. playersAmount == 3 -> [Model $ model & startGame .~ True & boardE .~ eraseBoard False threePlayersSet (model ^. boardE)]
    | model ^. playersAmount == 4 -> [Model $ model & startGame .~ True & boardE .~ eraseBoard False fourPlayersSet (model ^. boardE)]
    | otherwise -> [Model(model & startGame .~ True)]
  DoNothing -> []
  EndGameButtonClick -> [Model $ model & turnS .~ 0
                                       & winner .~ ""
                                       & boardE .~ externalBoard
                                       -- & playersAmount .~ 4
                                       & startGame .~ False
                                       & fromIndex .~ -1
                                       & toIndex .~ -1
                                       & errorMessage .~ ""]

  -- first enter the "from position", and check for the correct input
  -- then enter the "to position", if no error is made then process normal turn change, otherwise, reset and print the error message 
  AppTurnChange b -> case model ^. fromIndex of
                      -1 -> if Just (turnText model) == getColour b then [Model $ model & fromIndex .~ getIndex b & errorMessage .~ ""]
                            else [Model (model & errorMessage .~ show (turnText model) ++ ": invalid start")]
                      _  -> case model ^. fromIndex == getIndex b of
                              True -> [Model (model & errorMessage .~ show (turnText model) ++ ": no move made")]
                              False -> case isOccupied b of
                                        Just False -> []
                                        _ -> [Model (model & errorMessage .~ show (turnText model) ++ ": destination occupied")]

turnChange :: AppModel -> Int
turnChange model
    | model ^. turnS == model ^. playersAmount - 1 = 0
    | otherwise = model ^. turnS + 1

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
      _turnS = 0,
      _winner = "",
      _boardE = externalBoard,
      _playersAmount = 3,
      _startGame = False,
      _fromIndex = -1,
      _toIndex = -1,
      _errorMessage = ""
    }
