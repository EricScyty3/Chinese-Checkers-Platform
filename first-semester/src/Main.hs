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
import Monomer.Core.Combinators

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
  _displayBoard :: Board,
  _internalStates :: [OccupiedBoard],
  -- the players of the game
  _playersAmount :: Int,
  _computerPlayersAmount :: Int,
  _fromPiece :: BoardType,
  _toPiece :: BoardType,
  _previousFromPiece:: BoardType,
  _previousToPiece :: BoardType,
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
  | ResetChoice Int
  deriving (Eq, Show)

makeLenses 'AppModel

-- produce the title text
-- if the game does not start then just print welcome title
-- if an error is made then print it
-- finally, if everything is normal and the game is started, then print the turn
titleText :: AppModel -> String
titleText model
  | not (model ^. startGame) = "Haskell Chinese Checkers"
  | model ^. ifWin = show (turnColour model turn) ++ " wins"
  | otherwise = show (turnColour model turn) ++ "'s turn"
  where turn = model ^. turnS

turnColour :: AppModel -> Int -> Colour
turnColour model turn
  | model ^. playersAmount == 2 = twoPlayersSet !! turn
  | model ^. playersAmount == 3 = threePlayersSet !! turn
  | model ^. playersAmount == 4 = fourPlayersSet !! turn
  | otherwise = sixPlayersSet !! turn

ifInitialPiece :: BoardType -> Bool
ifInitialPiece (U (-1, -1)) = True
ifInitialPiece _ = False

-- construct the user interface layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  -- render the color of different pieces on the board based on the labels
  colouredLabel :: BoardType -> WidgetNode AppModel AppEvent
  colouredLabel ch
    | isNothing(isOccupied ch) = spacer
    | otherwise = button_ {-(T.pack $ show $ getIndex ch)-} "" (MoveCheck ch) [onClick RenderMove]
                  `styleBasic` [radius 45, bgColor white, border 2 white, textSize 20,
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
  makeRowState :: [BoardType] -> WidgetNode AppModel AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button

  computerPlayersChoices :: Int -> WidgetNode AppModel AppEvent
  computerPlayersChoices amount = labeledRadio (showt amount) amount computerPlayersAmount `styleBasic`[textSize 30]

  -- call functions to render the application
  widgetTree = vstack [
      box $ label_ (T.pack $ titleText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],
      spacer,
      box $ label_ (T.pack $ model ^. errorMessage) [ellipsis] `styleBasic` [textFont "Italic", textSize 20] `nodeVisible` model ^. startGame,
      spacer,
      box $ hgrid[
          filler,
          button "Quit" EndGameButtonClick `styleBasic`[textSize 20],
          filler,
          button "Cancel" CancelMove `styleBasic`[textSize 20],
          filler
      ]`nodeVisible` (model ^. startGame),
      filler,

      box $ vstack[
        label "Players Amount" `styleBasic`[textSize 30],
        hgrid_ [childSpacing_ 10][
          labeledRadio_ "2" 2 playersAmount [onChange ResetChoice] `styleBasic`[textSize 30],
          labeledRadio_ "3" 3 playersAmount [onChange ResetChoice] `styleBasic`[textSize 30],
          labeledRadio_ "4" 4 playersAmount [onChange ResetChoice] `styleBasic`[textSize 30],
          labeledRadio_ "6" 6 playersAmount [onChange ResetChoice] `styleBasic`[textSize 30]
        ],
        spacer,

        label "Computer Players Amount" `styleBasic`[textSize 30],
        hgrid_ [childSpacing_ 10](computerPlayersChoices <$> [0..model ^. playersAmount]),
        spacer,

        button "Start Game" StartGameButtonClick `styleBasic`[textSize 50]
      ] `nodeVisible` not (model ^. startGame),
      spacer,

      filler,
      vgrid_ [childSpacing_ 5] (makeRowState <$> (model ^. displayBoard)) `nodeVisible` (model ^. startGame)--,
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
    | model ^. playersAmount == 2 -> [Model $ model & startGame .~ True
                                                    & displayBoard .~ eraseBoard False twoPlayersSet externalBoard
                                                    & internalStates .~ replicate 2 initialState]
    | model ^. playersAmount == 3 -> [Model $ model & startGame .~ True
                                                    & displayBoard .~ eraseBoard False threePlayersSet externalBoard
                                                    & internalStates .~ replicate 3 initialState]
    | model ^. playersAmount == 4 -> [Model $ model & startGame .~ True
                                                    & displayBoard .~ eraseBoard False fourPlayersSet externalBoard
                                                    & internalStates .~ replicate 4 initialState]
    | otherwise -> [Model $ model & startGame .~ True
                                  & displayBoard .~ externalBoard
                                  & internalStates .~ replicate 6 initialState]
  CancelMove -- modified
    | model ^. ifWin -> []
    | not (ifInitialPiece pf) && not (ifInitialPiece pt) -> [Model $ model & displayBoard .~ newBoard
                                                                           & internalStates .~ insertState
                                                                           & previousFromPiece .~ U(-1, -1)
                                                                           & previousToPiece .~ U(-1, -1)
                                                                           & turnS .~ revertTurnChange model]
    | otherwise -> []
    where
      pf = model ^. previousFromPiece
      pt = model ^. previousToPiece
      newBoard = repaintPath pt pf lastTurnColour (model ^. displayBoard)
      fromProjectPos = projection lastTurnColour (getPos pf)
      toProjectPos = projection lastTurnColour (getPos pt)
      lastTurnColour = turnColour model (revertTurnChange model)
      newState = flipBoardState toProjectPos fromProjectPos ((model ^. internalStates) !! revertTurnChange model)
      insertState = replace (revertTurnChange model) newState (model ^. internalStates)

  RenderMove -- modified
    | model ^. ifWin -> [] -- if already won then do nothing
    | not (ifInitialPiece (model ^. toPiece)) && winStateDetect initialState newState ->
      [Model $ model & ifWin .~ True
                     & displayBoard .~ newBoard
                     & internalStates .~ insertState
                     & movesList .~ []] -- else then first determine the win state
    -- otherwise just update the turn 
    | not (ifInitialPiece (model ^. toPiece)) -> [Model $ model & displayBoard .~ newBoard
                                                                & internalStates .~ insertState
                                                                & turnS .~ turnChange model
                                                                & previousFromPiece .~ f
                                                                & previousToPiece .~ t
                                                                & fromPiece .~ U(-1, -1)
                                                                & toPiece .~ U(-1,-1)
                                                                & movesList .~ []]
    | otherwise -> []
      where
        f = model ^. fromPiece
        t = model ^. toPiece
        newBoard = repaintPath f t currentColour (model ^. displayBoard)
        currentColour = turnColour model (model ^. turnS)
        fromProjectPos = projection currentColour (getPos f)
        toProjectPos = projection currentColour (getPos t)
        newState = flipBoardState fromProjectPos toProjectPos ((model ^. internalStates) !! (model ^. turnS))
        insertState = replace (model ^. turnS) newState (model ^. internalStates)

  EndGameButtonClick -> [Model $ model & turnS .~ 0
                                       & ifWin .~ False
                                       & startGame .~ False
                                       & fromPiece .~ U (-1, -1)
                                       & toPiece .~ U (-1, -1)
                                       & previousFromPiece .~ U(-1, -1)
                                       & previousToPiece .~ U(-1, -1)
                                       & errorMessage .~ ""
                                       & movesList .~ []]

  -- first enter the "from position", and check for the correct input
  -- then enter the "to position", if no error is made then process normal turn change, otherwise, reset and print the error message 
  -- the error message appearing means the entered move will be discarded and need to be entered correctly
  MoveCheck b -> case model ^. ifWin of -- addition check for computer player
                  True -> []
                  False -> case ifInitialPiece $ model ^. fromPiece of
                              True  -> if Just currentColour == getColour b then [Model $ model & fromPiece .~ b
                                                                                                   & errorMessage .~ ""
                                                                                                   & movesList .~ destinationList (model ^. displayBoard) b]
                                       else [Model $ model & errorMessage .~ show currentColour ++ ": invalid start"
                                                           & fromPiece .~ U (-1, -1)]
                              False -> case model ^. fromPiece == b of
                                          True  -> [Model $ model & errorMessage .~ show currentColour ++ ": no move made"
                                                                  & fromPiece .~ U (-1, -1)]
                                          False -> case isOccupied b of
                                                      Just False -> case b `elem` model ^. movesList of
                                                                        True  -> [Model $ model & toPiece .~ b
                                                                                                & errorMessage .~ ""]
                                                                        False -> [Model $ model & errorMessage .~ show currentColour ++ ": destination unreacbable"
                                                                                                & fromPiece .~ U (-1, -1)]
                                                      _ -> [Model $ model & errorMessage .~ show currentColour ++ ": destination occupied"
                                                                          & fromPiece .~ U (-1, -1)]
    where
      currentColour = turnColour model (model ^. turnS)
  ResetChoice v
    | v < c -> [Model $ model & computerPlayersAmount .~ v]
    | otherwise -> []
    where
      c = model ^. computerPlayersAmount

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
      _displayBoard = externalBoard,
      _internalStates = [],
      _playersAmount = 2,
      _computerPlayersAmount = 0,
      _startGame = False,
      _fromPiece = U (-1, -1),
      _toPiece = U (-1, -1),
      _previousFromPiece = U(-1, -1),
      _previousToPiece = U(-1, -1),
      _errorMessage = "",
      _movesList = []
    }
