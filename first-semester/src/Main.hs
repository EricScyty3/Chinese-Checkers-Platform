{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

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
      EventResponse(Model), nodeInfoFromKey, label, CmbMultiline (multiline), CmbPaddingB (paddingB), CmbOnClick (onClick), CmbBorder (border) )
import TextShow
import Board

import qualified Data.Text as T
import qualified Monomer.Lens as L
import Monomer.Widgets
import Monomer.Main

-- the model representation that indicates the state of the application
-- information could be stored here that models the subject
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing, could also defines the start and end of the game
  -- 0 means the first player in the list
  _turnS :: Int,
  _startGame :: Bool,
  -- the string that represents the winning player's color
  _ifWin :: Bool,
  -- the representation of the board
  _boardE :: Board,
  _boardI :: Board,
  -- the players of the game
  _playersAmount :: Int,
  _fromPiece :: BoardType,
  _toPiece :: BoardType,
  _errorMessage :: String,
  _movesList :: [BoardType]
  -- _computerPlayersAmount :: Int
} deriving (Eq, Show)

-- the event type, representing different action the handler could react to
data AppEvent
  -- the initialize of the model status
  = AppInit
  -- the round change between players
  | MoveCheck BoardType
  | StartGameButtonClick
  | RenderMove
  | CancelMove
  | EndGameButtonClick
  | WinStateDeclare
  deriving (Eq, Show)

makeLenses 'AppModel

-- produce the title text
-- if the game does not start then just print welcome title
-- if an error is made then print it
-- finally, if everything is normal and the game is started, then print the turn
titleText :: AppModel -> String
titleText model
  | not (model ^. startGame) = "Haskell Chinese Checkers"
  | model ^. ifWin = show (turnText model) ++ " wins"
  | otherwise = show (turnText model) ++ "'s turn"

turnText :: AppModel -> Colour
turnText model
  | model ^. playersAmount == 2 = twoPlayersSet !! turn
  | model ^. playersAmount == 3 = threePlayersSet !! turn
  | model ^. playersAmount == 4 = fourPlayersSet !! turn
  | otherwise = sixPlayersSet !! turn
  where turn = model ^. turnS

printHint :: AppModel -> String
printHint model
  | null (model ^. errorMessage) = "From " ++ printIndex (model ^. fromPiece) ++ " to " ++ printIndex (model ^. toPiece)
  | otherwise = "From " ++ printIndex (model ^. fromPiece) ++ " to " ++ printIndex (model ^. toPiece) ++ ", " ++ model ^. errorMessage
    where
      printIndex :: BoardType -> String
      printIndex p = if ifInitial p then " " else show $ getIndex p

ifInitial :: BoardType -> Bool
ifInitial (U (-1, -1)) = True
ifInitial _ = False

-- borderColour :: AppModel -> Colour -> Colour
-- borderColour model c = 

-- buttonColour :: Colour 

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
    | otherwise = button_ (T.pack $ show $ getIndex ch) (MoveCheck ch) [ellipsis]
                  `styleBasic` [radius 45, bgColor white,
                                styleIf (isRed ch)(bgColor red),
                                styleIf (isBlue ch) (bgColor blue),
                                styleIf (isGreen ch) (bgColor green),
                                styleIf (isPurple ch)(bgColor purple),
                                styleIf (isOrange ch) (bgColor orange),
                                styleIf (isYellow ch) (bgColor yellow),
                                styleIf (isJustTrue (isOccupied ch) 
                                         && not (isYellow ch) 
                                         {-&& not (isOrange ch)-})
                                         (textColor white)
                                ]

  -- -- display a row of elements on the board
  makeRowState :: [BoardType] -> WidgetNode s AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button

  -- call functions to render the application
  widgetTree = vstack [
      box $ label_ (T.pack $ titleText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],
      spacer,
      box $ label_ (T.pack $ printHint model) [ellipsis] `styleBasic` [textFont "Italic", textSize 20] `nodeVisible` model ^. startGame,
      spacer,
      box $ hgrid[
          button "Quit" EndGameButtonClick `styleBasic`[textSize 20],
          filler,
          button_ "Confirm" RenderMove [onClick WinStateDeclare] `styleBasic`[textSize 20],
          filler,
          button "Cancel" CancelMove `styleBasic`[textSize 20]
      ]`nodeVisible` (model ^. startGame),
     filler,

      box $ vstack[
        hgrid[
          labeledRadio "2" 2 playersAmount `styleBasic`[textSize 30],
          spacer,
          labeledRadio "3" 3 playersAmount `styleBasic`[textSize 30],
          spacer,
          labeledRadio "4" 4 playersAmount `styleBasic`[textSize 30],
          spacer,
          labeledRadio "6" 6 playersAmount `styleBasic`[textSize 30]
        ],
        spacer,
        button "New Game" StartGameButtonClick `styleBasic`[textSize 50]
      ] `nodeVisible` not (model ^. startGame),
      spacer,

      filler,
      vgrid_ [childSpacing_ 5] (makeRowState <$> (model ^. boardE)) `nodeVisible` (model ^. startGame)--,
      -- filler
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
   | model ^. playersAmount == 2 ->  [Model $ model & startGame .~ True
                                                    & boardE .~ eraseBoard False twoPlayersSet externalBoard
                                                    & boardI .~ eraseBoard False twoPlayersSet externalBoard]
    | model ^. playersAmount == 3 -> [Model $ model & startGame .~ True
                                                    & boardE .~ eraseBoard False threePlayersSet externalBoard
                                                    & boardI .~ eraseBoard False threePlayersSet externalBoard]
    | model ^. playersAmount == 4 -> [Model $ model & startGame .~ True
                                                    & boardE .~ eraseBoard False fourPlayersSet externalBoard
                                                    & boardI .~ eraseBoard False fourPlayersSet externalBoard]
    | otherwise -> [Model $ model & startGame .~ True
                                  & boardE .~ externalBoard
                                  & boardI .~ externalBoard]
  CancelMove
    | model ^. ifWin -> []
    | not $ ifInitial $ model ^. toPiece -> [Model $ model & toPiece .~ U(-1, -1) & errorMessage .~ ""]
    | otherwise -> [Model $ model & fromPiece .~ U(-1, -1) & errorMessage .~ ""]
  
  RenderMove
    | model ^. ifWin -> []
    -- if valid movement is set
    | null (model ^. errorMessage) && not (ifInitial (model ^. fromPiece)) && not (ifInitial (model ^. toPiece))
      -> [Model $ model & boardE .~ updateBoard model
                        & fromPiece .~ U(-1, -1)
                        & toPiece .~ U(-1,-1)]
    | otherwise -> []
  
  EndGameButtonClick -> [Model $ model & turnS .~ 0
                                       & ifWin .~ False
                                       -- & playersAmount .~ 4
                                       & startGame .~ False
                                       & fromPiece .~ U (-1, -1)
                                       & toPiece .~ U (-1, -1)
                                       & errorMessage .~ ""
                                       & movesList .~ []]

  -- first enter the "from position", and check for the correct input
  -- then enter the "to position", if no error is made then process normal turn change, otherwise, reset and print the error message 
  MoveCheck b -> case model ^. errorMessage /= "" || model ^. ifWin of
                  True -> []
                  False -> case ifInitial $ model ^. fromPiece of
                              True  -> if Just (turnText model) == getColour b then [Model $ model & fromPiece .~ b & errorMessage .~ ""]
                                       else [Model $ model & errorMessage .~ show (turnText model) ++ ": invalid start" & fromPiece .~ b]
                              False -> case model ^. fromPiece == b of
                                          True  -> [Model $ model & errorMessage .~ show (turnText model) ++ ": no move made" & toPiece .~ b]
                                          False -> case isOccupied b of
                                                      Just False -> case testJumpValid (model ^. boardE) (model ^. fromPiece) b of
                                                                        True  -> [Model $ model & toPiece .~ b 
                                                                                                & errorMessage .~ "" 
                                                                                                & movesList .~ destinationList (model ^. boardE) b]
                                                                        False -> [Model $ model & errorMessage .~ show (turnText model) ++ ": destination unreacbable" & toPiece .~ b]
                                                      _ -> [Model $ model & errorMessage .~ show (turnText model) ++ ": destination occupied" & toPiece .~ b]
  
  WinStateDeclare
    | winStateDetect (model ^. boardE) (turnText model) (model ^. boardI) -> [Model $ model & ifWin .~ True]
    | null $ model ^. errorMessage -> [Model $ model & turnS .~ turnChange model]
    | otherwise -> []

-- Check before a path is made and finally repainted to the board:
-- 1. the first entered piece should be valid: correct colour
-- 2. the second enter piece should be valid: no repeated click, no occupied position, reachabld position
-- finally, settle the movement path and update the turn

turnChange :: AppModel -> Int
turnChange model
    | model ^. turnS == model ^. playersAmount - 1 = 0
    | otherwise = model ^. turnS + 1

updateBoard :: AppModel -> Board
updateBoard model = repaintPath (model ^. fromPiece) (model ^. toPiece) (model ^. boardE)

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
      appWindowResizable False, -- disable resizing windows
      appWindowState $ MainWindowNormal (800, 700),
      appInitEvent AppInit
      ]
    -- provide an initial model of the application
    model = AppModel {
      _turnS = 0,
      _ifWin = False,
      _boardE = externalBoard,
      _boardI = externalBoard,
      _playersAmount = 2,
      _startGame = False,
      _fromPiece = U (-1, -1),
      _toPiece = U (-1, -1),
      _errorMessage = "",
      _movesList = []
    }
