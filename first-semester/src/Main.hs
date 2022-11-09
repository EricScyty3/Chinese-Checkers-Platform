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
      EventResponse(Model), nodeInfoFromKey, label, CmbMultiline (multiline), CmbPaddingB (paddingB), CmbOnClick (onClick), CmbBorder (border), black, gray )
import TextShow
import Board

import qualified Data.Text as T
import qualified Monomer.Lens as L
import Monomer.Widgets
import Monomer.Main
import Monomer.Graphics.ColorTable

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
  _previousFromPos :: Pos,
  _previousToPos :: Pos,
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

-- printHint :: AppModel -> String
-- printHint model
--   | null (model ^. errorMessage) = "From " ++ printIndex (model ^. fromPiece) ++ " to " ++ printIndex (model ^. toPiece)
--   | otherwise = "From " ++ printIndex (model ^. fromPiece) ++ " to " ++ printIndex (model ^. toPiece) ++ ", " ++ model ^. errorMessage
--     where
--       printIndex :: BoardType -> String
--       printIndex p = if ifInitial p then " " else show $ getIndex p

ifInitialPiece :: BoardType -> Bool
ifInitialPiece (U (-1, -1)) = True
ifInitialPiece _ = False

ifInitialPos :: Pos -> Bool
ifInitialPos (-1, -1) = True
ifInitialPos _ = False

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
    | otherwise = button_ {-(T.pack $ show $ getIndex ch)-} "" (MoveCheck ch) [onClick RenderMove]
                  `styleBasic` [radius 45, bgColor white, border 2 white,
                                styleIf (isRed ch)(bgColor red),
                                styleIf (isBlue ch) (bgColor blue),
                                styleIf (isGreen ch) (bgColor green),
                                styleIf (isPurple ch)(bgColor purple),
                                styleIf (isOrange ch) (bgColor darkOrange),
                                styleIf (isBlack ch) (bgColor black),
                                styleIf (ch == p || ch `elem` model ^. movesList) (border 2 pc)
                                ]
    where 
      p = model ^. fromPiece

      pc 
        | isRed p = red
        | isBlue p = blue
        | isGreen p = green
        | isPurple p = purple
        | isOrange p = darkOrange
        | isBlack p = black
        | otherwise = white 


  -- -- display a row of elements on the board
  makeRowState :: [BoardType] -> WidgetNode s AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button

  -- call functions to render the application
  widgetTree = vstack [
      box $ label_ (T.pack $ titleText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],
      spacer,
      box $ label_ (T.pack $ model ^. errorMessage) [ellipsis] `styleBasic` [textFont "Italic", textSize 20] `nodeVisible` model ^. startGame,
      spacer,
      box $ hgrid[
          button "Quit" EndGameButtonClick `styleBasic`[textSize 20],
          -- filler,
          -- button_ "Confirm" RenderMove [onClick WinStateDeclare] `styleBasic`[textSize 20],
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
    | not (ifInitialPos pfp) && not (ifInitialPos ptp) -> [Model $ model & boardE .~ newBoard
                                                                 & previousFromPos .~ (-1, -1)
                                                                 & previousToPos .~ (-1, -1)
                                                                 & turnS .~ revertTurnChange model]
    | otherwise -> []
    where
      pfp = model ^. previousFromPos
      ptp = model ^. previousToPos
      pf = getElement (model ^. boardE) pfp
      pt = getElement (model ^. boardE) ptp
      newBoard = repaintPath pt pf (model ^. boardE)
  
  RenderMove
    | model ^. ifWin -> [] -- if already won then do nothing
    | not (ifInitialPiece (model ^. toPiece)) && winStateDetect newBoard currentColour (model ^. boardI) -> 
      [Model $ model & ifWin .~ True & boardE .~ newBoard] -- else then first determine the win state
    -- otherwise just update the turn 
    | not (ifInitialPiece (model ^. toPiece)) -> [Model $ model & boardE .~ newBoard
                                                           & turnS .~ turnChange model                         
                                                           & previousFromPos .~ getPos(model ^. fromPiece) 
                                                           & previousToPos .~ getPos(model ^. toPiece)
                                                           & fromPiece .~ U(-1, -1)
                                                           & toPiece .~ U(-1,-1)
                                                           & movesList .~ []]
    | otherwise -> []
      where 
        newBoard = repaintPath (model ^. fromPiece) (model ^. toPiece) (model ^. boardE)
        currentColour = turnText model
  
  EndGameButtonClick -> [Model $ model & turnS .~ 0
                                       & ifWin .~ False
                                       & startGame .~ False
                                       & fromPiece .~ U (-1, -1)
                                       & toPiece .~ U (-1, -1)
                                       & previousFromPos .~ (-1, -1)
                                       & previousToPos .~ (-1, -1)
                                       & errorMessage .~ ""
                                       & movesList .~ []]

  -- first enter the "from position", and check for the correct input
  -- then enter the "to position", if no error is made then process normal turn change, otherwise, reset and print the error message 
  -- the error message appearing means the entered move will be discarded and need to be entered correctly
  MoveCheck b -> case model ^. ifWin of
                  True -> []
                  False -> case ifInitialPiece $ model ^. fromPiece of
                              True  -> if Just (turnText model) == getColour b then [Model $ model & fromPiece .~ b 
                                                                                                   & errorMessage .~ ""
                                                                                                   & movesList .~ destinationList (model ^. boardE) b]
                                       else [Model $ model & errorMessage .~ show (turnText model) ++ ": invalid start" 
                                                            & fromPiece .~ U (-1, -1)]
                              False -> case model ^. fromPiece == b of
                                          True  -> [Model $ model & errorMessage .~ show (turnText model) ++ ": no move made" 
                                                                  & fromPiece .~ U (-1, -1)]
                                          False -> case isOccupied b of
                                                      Just False -> case b `elem` model ^. movesList of
                                                                        True  -> [Model $ model & toPiece .~ b 
                                                                                                & errorMessage .~ ""]
                                                                        False -> [Model $ model & errorMessage .~ show (turnText model) ++ ": destination unreacbable" 
                                                                                                & fromPiece .~ U (-1, -1)]
                                                      _ -> [Model $ model & errorMessage .~ show (turnText model) ++ ": destination occupied" 
                                                                          & fromPiece .~ U (-1, -1)]
      

-- Check before a path is made and finally repainted to the board:
-- 1. the first entered piece should be valid: correct colour
-- 2. the second enter piece should be valid: no repeated click, no occupied position, reachabld position
-- finally, settle the movement path and update the turn


turnChange :: AppModel -> Int
turnChange model
    | model ^. turnS == model ^. playersAmount - 1 = 0
    | otherwise = model ^. turnS + 1

revertTurnChange :: AppModel -> Int
revertTurnChange model
  | model ^. turnS == 0 = model ^. playersAmount - 1
  | otherwise = model ^. turnS - 1

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
      appWindowState $ MainWindowNormal (800, 750),
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
      _previousFromPos = (-1, -1),
      _previousToPos = (-1, -1),
      _errorMessage = "",
      _movesList = []
    }
