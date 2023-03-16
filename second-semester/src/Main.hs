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
      EventResponse(Model), nodeInfoFromKey, label, CmbMultiline (multiline), CmbPaddingB (paddingB), CmbOnClick (onClick), CmbBorder (border), black, gray, lightTheme )
import TextShow
import Board
import Control.Monad.State
import qualified Data.Text as T
import qualified Monomer.Lens as L
import Monomer.Widgets
import Monomer.Main
import Monomer.Graphics.ColorTable
import Monomer.Core.Combinators
import Zobrist
import GameTree
import RBTree ( RBTree(RBLeaf) )
import MCTS
import Control.Concurrent
import Configuration (LookupTable, lookupTable)
import Data.List (elemIndex)
import Minimax
import Data.Fixed

-- the container for storing the parameters for MCTS frameworks
-- the configuration of a single computer player
data ComputerPlayerConfig = ComputerPlayerConfig {
  _active :: Bool,
  _uct :: Double,
  _ph :: Double,
  _evaluator :: PlayoutEvaluator,
  _depth :: Int,     -- same as above
  _control :: Int, -- the choice of how the mcts computes, either based on total iterations, expansions count or the time
  _cvalue :: Int
} deriving (Eq, Show)

newtype ConfigList = ConfigList {
  _configList :: [ComputerPlayerConfig]
} deriving (Eq, Show)

-- the GUI for the game platform allowing user to set player and board configuration
-- the model representation indicates the state of the application: the information stored that models the subjects
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing, 0 means the first player in the list
  _turnS :: Int,
  -- a flag that declares the initial state of the game setup
  _startGame :: Bool,
  -- a flag that declares the win state of the game
  _ifWin :: Bool,
  -- the representation of the board
  _displayBoard :: Board,
  -- the hash state of each player to determine the win state, if a player wins the game, the corresponding hash should be equal to certain value
  _internalStates :: [Int],
  -- the players of the game
  _playersAmount :: Int,
  -- _computerIdxList :: [Int], -- the index to determine if the current player an AI or human player
  -- the current player's move: from a position to another new position
  _fromPiece :: BoardPos,
  _toPiece :: BoardPos,
  -- the last movement made by player at last turn, used for movement canceling
  _previousFromPiece:: BoardPos,
  _previousToPiece :: BoardPos,
  -- it will be display when an incorrect state appears
  _errorMessage :: String,
  -- a list of posistions that a player can move from an entered position
  _movesList :: [BoardPos],
  _gameHistory :: HistoryTrace, -- the history trace applied by MCTS that stores movements states

  _pageIndex :: Int,
  _defaultConfig :: ConfigList -- the container that holds the configuration of a list of computer players
} deriving (Eq, Show)

-- the event that the model triggers and handles, the different actions that the handler could react to
data AppEvent
  = AppInit -- the initialisation of the model status
  | MoveCheck BoardPos Int -- determine if a movement input is valid, if it is then render it, otherwise, display an error message
  | StartGameButtonClick -- initialise the game configuration depending on user's settings
  | RenderMove  -- show the change of the movement
  | CancelMove  -- cancel the last played movement
  | EndGameButtonClick -- quit the current game and go back to the menu page
  | ResetChoice Int -- reset the game configuration
  | NextPage
  | LastPage
  deriving (Eq, Show)

makeLenses 'ComputerPlayerConfig
makeLenses 'ConfigList
makeLenses 'AppModel

-- produce the title text 
titleText :: AppModel -> String
titleText model
  | not (model ^. startGame) = "Haskell Chinese Checkers" -- print welcome text at the menu page
  | otherwise = ""
  {-
  | model ^. ifWin = name ++ show turn ++ " wins" -- print winning player's colour when the win state of a player is achieved
  | otherwise = name ++ show turn ++ "'s turn" -- print current player's turn and colour if win state is not satisfied
  where
    turn = model ^. turnS
    name = if turn `elem` model ^. computerIdxList then "Computer Player " else "Player "
    -- colour = playerColour (model ^. turnS) (model ^. playersAmount)
  -}

-- determine if the state of fromPiece, toPiece, previousFromPiece, previousToPiece, is changed
ifInitialPiece :: BoardPos -> Bool
ifInitialPiece (U (-1, -1)) = True
ifInitialPiece _ = False

initialPos :: BoardPos
initialPos = U (-1, -1)

-- check the player index of the a certain piece
getPlayerIndex :: Int -> BoardPos -> Int
getPlayerIndex pn ch = case getColour ch of
                            Nothing -> (-1)
                            Just c  -> fromMaybe (-1) (elemIndex c (playerColourList pn))

-- construct the graphical layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  -- render the color of a piece on the board
  colouredLabel :: BoardPos -> WidgetNode AppModel AppEvent
  colouredLabel ch
    -- place a spacer if the position type is "U"
    | isSpacer ch = spacer
    -- otherwise, place a buttom with colour representing the a board position
    | otherwise = button_ (T.pack tpi) (MoveCheck ch pi) [onClick RenderMove] `styleBasic`
                                                                         [radius 45, bgColor white, border 2 white, textSize 20, textColor white, -- empty positions will be coloured in white
                                                                          styleIf (compareColour ch Red)    (bgColor red),
                                                                          styleIf (compareColour ch Blue)   (bgColor blue),
                                                                          styleIf (compareColour ch Green)  (bgColor green),
                                                                          styleIf (compareColour ch Purple) (bgColor purple),
                                                                          styleIf (compareColour ch Orange) (bgColor darkOrange),
                                                                          styleIf (compareColour ch Black)  (bgColor black),
                                                                          styleIf (ch == piece || ch `elem` model ^. movesList) (bgColor pieceColour),
                                                                          styleIf (ch == piece) (border 2 pieceColour)
                                                                          ]
                                                                          -- ont only the pieces are in colour, but also the avaliable moves
                                                                          -- additionally, the clicked position and movement positions will be in boarder colour while others are in white
    where
      pn = model ^. playersAmount
      pi = getPlayerIndex pn ch
      tpi = if pi == (-1) then "" else show pi
      piece = model ^. fromPiece
      pieceColour
        | compareColour piece Red = red
        | compareColour piece Blue = blue
        | compareColour piece Green = green
        | compareColour piece Purple = purple
        | compareColour piece Orange = darkOrange
        | compareColour piece Black = black
        | otherwise = white

  -- display a row of elements of a board
  makeRowState :: [BoardPos] -> WidgetNode AppModel AppEvent
  makeRowState row = hgrid(colouredLabel <$> row) -- render the rows of button in a grid style

  -- display the player settings, the amount of AI plauers
  computerPlayersChoices :: Int -> WidgetNode AppModel AppEvent
  computerPlayersChoices idx = -- labeledRadio_ (showt amount) amount computerPlayersAmount [textRight]
                                  labeledCheckbox_ (showt idx) (defaultConfig . configList . singular (ix idx) . active) [checkboxSquare, textRight]

  featureLayer =

      vstack [
        label (T.pack ("Exploration Factor: " ++ show (vitem ^. uct))) `styleBasic` [textSize 20],
        spacer,
        hslider_ (mitem.uct) 0 5 [dragRate 0.1] `styleBasic` [fgColor orange],
        spacer,
        label (T.pack ("History Factor: " ++ show (vitem ^. ph))) `styleBasic` [textSize 20],
        spacer,
        hslider_ (mitem.ph) 0 5 [dragRate 0.1] `styleBasic` [fgColor orange],
        spacer,

        hstack [

        box_ [alignTop] $ vgrid_ [childSpacing_ 5] [
            label "Simulation Evaluator" `styleBasic` [textSize 20],
            labeledRadio_ "Random" RandomEvaluator eitem [textRight],
            labeledRadio_ "Move" MoveEvaluator eitem [textRight],
            labeledRadio_ "Board" BoardEvaluator eitem [textRight],

            hstack [
              labeledRadio_ "Paranoid" ShallowParanoid eitem [textRight],
              spacer,
              hgrid_ [childSpacing_ 5] [
                label "(depth)",
                labeledRadio_ "2" 2 ditem [textRight],
                labeledRadio_ "3" 3 ditem [textRight],
                labeledRadio_ "4" 4 ditem [textRight]
              ] `nodeVisible` (vitem ^. evaluator == ShallowParanoid)
            ],

            hstack [
              labeledRadio_ "BRS" ShallowBRS eitem [textRight],
              spacer,
              hgrid_ [childSpacing_ 5] [
                label "(depth)",
                labeledRadio_ "2" 2 ditem [textRight],
                labeledRadio_ "3" 3 ditem [textRight],
                labeledRadio_ "4" 4 ditem [textRight]
              ] `nodeVisible` (vitem ^. evaluator == ShallowBRS)
            ]
          ],

          filler,
          box_ [alignTop] $
            vgrid_ [childSpacing_ 10] [
              label "MCTS Control" `styleBasic` [textSize 20],
              labeledRadio_ "Iterations" 0 citem [textRight],
              labeledRadio_ "Expansion (nodes)" 1 citem [textRight],
              labeledRadio_ "Time (seconds)" 2 citem [textRight]
            ]
        ],
        spacer,

        vstack [
          label (T.pack ("Control Value: " ++ show (vitem ^. cvalue))) `styleBasic` [textSize 20],
          spacer,
          hslider cvitem 1 1000 `styleBasic` [fgColor orange]
        ]
      ] `styleBasic` [maxWidth 600, border 2 white, padding 20, radius 10]

    
  pi = model ^. pageIndex
  vitem = model ^. defaultConfig ^?! configList . ix pi
  aitem = model ^. defaultConfig ^?! configList . ix pi . active
  mitem = defaultConfig . configList . singular (ix pi)
  eitem = defaultConfig . configList . singular (ix pi) . evaluator
  ditem = defaultConfig . configList . singular (ix pi) . depth
  citem = defaultConfig . configList . singular (ix pi) . control
  cvitem = defaultConfig . configList . singular (ix pi) . cvalue


  selectButtonLayer =
      hstack [
        button "Last" LastPage `nodeEnabled` (pi > 0),
        filler,
        label (T.pack ( (if aitem then "Computer Player: " else "Human Player: ")
                        ++ show pi)) `styleBasic` [textSize 30],
        filler,
        button "Next" NextPage `nodeEnabled` (pi < 5)
      ] `styleBasic` [maxWidth 600, padding 20]


  playerLayer =
    hstack[
      vstack [
        label "Total Players",
        spacer_ [width 15],
        label "Computer Players"
      ]`styleBasic` [textSize 20],
      spacer,

      box_ [alignTop] $ vstack [
        box_ [alignLeft] $ hgrid_ [childSpacing_ 10] [
          labeledRadio_ "2" 2 playersAmount [onChange ResetChoice, textRight], -- the change of total players amount will also change the allowed AI players
          labeledRadio_ "3" 3 playersAmount [onChange ResetChoice, textRight],
          labeledRadio_ "4" 4 playersAmount [onChange ResetChoice, textRight],
          labeledRadio_ "6" 6 playersAmount [onChange ResetChoice, textRight]
        ],
        spacer,
        box_ [alignLeft] $ hgrid_ [childSpacing_ 10] (computerPlayersChoices <$> [0.. model ^. playersAmount - 1])
      ]
    ] `styleBasic` [maxWidth 600, border 2 white, padding 20, radius 10]

  -- build strcutute/layout of the application
  widgetTree =
      vstack [
        -- the toppest one is the title
        box $ label_ (T.pack $ titleText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],
        -- box_ [alignCenter, alignMiddle]
        filler,

        box featureLayer `nodeVisible` not (model ^. startGame) `nodeEnabled` aitem,
        spacer,
        box selectButtonLayer,
        {-spacer,
        

        -- then the message area, where the error message will be shown if needed, not visiable until the game is started
        box $ label_ (T.pack $ model ^. errorMessage) [ellipsis] `styleBasic` [textFont "Italic", textSize 20] `nodeVisible` model ^. startGame,
        spacer,
        -- the two buttons that allow user to quit the game or cancel the last move made, not visiable until the game is started
        box $ hgrid[
            filler,
            button "Quit" EndGameButtonClick `styleBasic`[textSize 20],
            filler,
            button "Cancel" CancelMove `styleBasic`[textSize 20],
            filler
        ]`nodeVisible` (model ^. startGame),
        -- filler,
        -}
        -- provide the options the user could choose for game and player configurations
        spacer,
        box playerLayer `nodeVisible` not (model ^. startGame), -- only visitable in the menu page

        filler,
        box $ button "Start Game" StartGameButtonClick `styleBasic`[textSize 30],
        filler
        {-
        spacer,
        -- finally render the pieces row by row
        -- filler,
        vgrid_ [childSpacing_ 5] (makeRowState <$> (model ^. displayBoard)) `nodeVisible` (model ^. startGame)-}
      ] `styleBasic` [padding 20]

-- declare how the events are handled respectively
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [{-Producer computerTurn-}] -- an additional thread for checking computer player's turn
  -- setup the game board according to the options in the menu page: 
  -- declare that the game is started letting certain subjects to be visiable or invisiable
  -- generate the game board based on the chosen players number, and the corresponding length of hash state
  StartGameButtonClick -> [] {-[Model $ model & startGame .~ True
                                         & displayBoard .~ eraseBoard (playerColourList pn) externalBoard
                                         & internalStates .~ replicate pn hashInitial
                                         & computerIdxList .~ idxList]-}
    where
      -- get the computer players indices randomly
      pn = model ^. playersAmount
      -- idxList = take (model ^. computerPlayersAmount) (randomIndices pn)

  NextPage ->  [Model $ model & pageIndex .~ model ^. pageIndex + 1]
  LastPage ->  [Model $ model & pageIndex .~ model ^. pageIndex - 1]

  -- the event of cancel the last piece change made, and revert it to the previous board state   
  -- movement cancel is not allowed to be made over a player, meaning that once another player makes the move, you can no longer cancel yours                             
  CancelMove -> [] {-
    | model ^. ifWin || model ^. computerIdxList /= [] -> [] -- ignore if the win state is confirmed or computer player involves
    | not (ifInitialPiece pf) && not (ifInitialPiece pt) -> [Model $ model & displayBoard .~ newBoard -- reset the board, hash, and turn states
                                                                           & internalStates .~ insertState
                                                                           & previousFromPiece .~ initialPos
                                                                           & previousToPiece .~ initialPos
                                                                           & turnS .~ lastTurn]
    | otherwise -> [] -- ignore if not record is stored for that
    -}
    where
      pf = model ^. previousFromPiece
      pt = model ^. previousToPiece
      newBoard = repaintPath (model ^. displayBoard) (pt, pf) -- revert the board state

      -- get the last turn value and retrieve the information for reverting the hash state
      lastTurn = revertTurnChange model
      lastColour = playerColour lastTurn (model ^. playersAmount)
      pfp = projection lastColour (getPos pf)
      ptp = projection lastColour (getPos pt)

      lastTurnState = (model ^. internalStates) !! lastTurn
      newState = changeHash pfp ptp lastTurnState -- revert the hash and then replace it in the state list
      insertState = replace lastTurn newState (model ^. internalStates)

  -- after a movement is determined valid, it will be rendered
  RenderMove
    | model ^. ifWin -> [] -- if already won then do nothing
    -- otherwise just update the turn 
    | not (ifInitialPiece (model ^. toPiece)) -> [Model $ model & displayBoard .~ newBoard
                                                                & internalStates .~ insertState
                                                                & turnS .~ newTurn
                                                                & previousFromPiece .~ f
                                                                & previousToPiece .~ t
                                                                & fromPiece .~ initialPos
                                                                & toPiece .~ initialPos
                                                                & movesList .~ []
                                                                & ifWin .~ newWinState] -- if a win is reached then update it
    | otherwise -> []
      where
        f = model ^. fromPiece
        t = model ^. toPiece
        newBoard = repaintPath (model ^. displayBoard) (f, t) -- generate the new board state by re-colouring the two board positions
        currentColour = playerColour (model ^. turnS) (model ^. playersAmount)
        currentState = (model ^. internalStates) !! (model ^. turnS)
        fromProjectPos = projection currentColour (getPos f)
        toProjectPos = projection currentColour (getPos t)
        newState = changeHash fromProjectPos toProjectPos currentState
        insertState = replace (model ^. turnS) newState (model ^. internalStates) -- replace the old hash state with the new one
        newWinState = winStateDetectHash newState
        newTurn = if not newWinState then turnChange model else model ^. turnS

  -- quit the game and return back to the menu page
  EndGameButtonClick -> [Model $ model & turnS .~ 0 -- resetting all states
                                       & ifWin .~ False
                                       & startGame .~ False
                                       & fromPiece .~ initialPos
                                       & toPiece .~ initialPos
                                       & previousFromPiece .~ initialPos
                                       & previousToPiece .~ initialPos
                                       & errorMessage .~ ""
                                       & movesList .~ []
                                       & gameHistory .~ RBLeaf]

  -- the movement check, how a movement is passed is done as follows:
  -- first enter the starting point, and check for the correctness
  -- then enter the destination, if no error is made then process, otherwise, discard that and print the error message 
  MoveCheck b pi -> []
                {-case model ^. ifWin || (model ^. turnS `elem` model ^. computerIdxList) of -- if already won or it's computer's turn, then do nothing
                  True ->  []
                  False -> case ifInitialPiece $ model ^. fromPiece of -- if first time for entering starting position
                              True  -> if turn == pi then [Model $ model & fromPiece .~ b     -- check if the entered position is valid for the current player
                                                                         & errorMessage .~ "" -- if valid, then checkout the avaliable movements
                                                                         & movesList .~ newMovesList]
                                       else [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": invalid start"
                                                           & fromPiece .~ U (-1, -1)] -- if not, then discard this, and wait for another valid input

                              False -> case model ^. fromPiece == b of -- otherwise, this position is clicked twice, and is an invalid movement
                                          True  -> [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": no move made"
                                                                  & fromPiece .~ U (-1, -1)]
                                          False -> case isOccupied b of -- if not, then check whether the second clicked position (destination) is avaliable/reachable from the first entered position
                                                      False -> case b `elem` model ^. movesList of
                                                                        True  -> [Model $ model & toPiece .~ b
                                                                                                & errorMessage .~ ""] -- if reachable, then this movement will then be rendered
                                                                        False -> [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": destination unreacbable" -- if the desintation is not in the list, then invalid
                                                                                                & fromPiece .~ U (-1, -1)]
                                                      True -> [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": destination occupied" -- if the destination is occupied, then invalid
                                                                             & fromPiece .~ U (-1, -1)]-}
    where
      turn = model ^. turnS
      -- currentColour = playerColour (model ^. turnS) (model ^. playersAmount)
      newMovesList = evalState (destinationList b) (model ^. displayBoard)

  -- reset the total player amount for computer player options
  ResetChoice v -> [Model $ model & defaultConfig . configList . ix 0 . active .~ head newList
                                  & defaultConfig . configList . ix 1 . active .~ newList !! 1
                                  & defaultConfig . configList . ix 2 . active .~ newList !! 2
                                  & defaultConfig . configList . ix 3 . active .~ newList !! 3
                                  & defaultConfig . configList . ix 4 . active .~ newList !! 4
                                  & defaultConfig . configList . ix 5 . active .~ newList !! 5 ]
    where
      newList = take v remainedList ++ replicate (6 - v) False
      remainedList = map modelList [0..5]
      modelList x = model ^?! defaultConfig . configList . singular (ix x) . active
{-
  -- only react when the during the game and is currently the computer player's turn
  ComputerAction -> case model ^. startGame && (model ^. turnS `elem` model ^. computerIdxList) && not (model ^. ifWin) of
                      True  -> [Model $ model & displayBoard .~ newBoard
                                              & internalStates .~ insertState
                                              & gameHistory .~ newHistoryTree
                                              & turnS .~ newTurn
                                              & previousFromPiece .~ U(-1, -1)
                                              & previousToPiece .~ U(-1, -1)
                                              & ifWin .~ newWinState
                                              ] -- apply the movement retrieved from the MCTS decision, and perform
                      False -> []
    where
      (newBoard, newState, newHistoryTree) = aiDecision model
      insertState = replace (model ^. turnS) newState (model ^. internalStates)
      newWinState = winStateDetectHash newState
      newTurn = if not newWinState then turnChange model else model ^. turnS
-}

-- update the turn based on the order or reverting order
turnChange :: AppModel -> Int
turnChange model
    | model ^. turnS == model ^. playersAmount - 1 = 0
    | otherwise = model ^. turnS + 1

revertTurnChange :: AppModel -> Int
revertTurnChange model
  | model ^. turnS == 0 = model ^. playersAmount - 1
  | otherwise = model ^. turnS - 1

{-
-- keep calling the event for checking the computer player's movement
computerTurn :: (AppEvent -> IO a) -> IO b
computerTurn sendMsg = do sendMsg ComputerAction
                          threadDelay $ 1000 * 800
                          computerTurn sendMsg

-- pass the model information to the MCTS interface and accept the returned board state
aiDecision :: AppModel -> (Board, Int, HistoryTrace)
aiDecision model = let root = GRoot 0 []  -- makeRoot (model ^. playersAmount) (model ^. displayBoard)
                       (newBoard, newState, nht, _) = finalSelection root (model ^. turnS, 1, eboard, ps, pn, ht, (3, 0.9), (BoardEvaluator, 2)) currentState 10
                   in  (newBoard, newState, nht)
  where
    currentState = (model ^. internalStates) !! (model ^. turnS)
    eboard = model ^. displayBoard
    pn = model ^. playersAmount
    ht = model ^. gameHistory
    ps = initialInternalBoard eboard pn
-}

-- load the configuration options as well as define the initial state of the application
main :: IO ()
main = do startApp model handleEvent buildUI config
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

      _startGame = False,
      _fromPiece = initialPos,
      _toPiece = initialPos,
      _previousFromPiece = initialPos,
      _previousToPiece = initialPos,
      _errorMessage = "",
      _movesList = [],
      _gameHistory = RBLeaf,

      _pageIndex = 0
      , _defaultConfig = ConfigList $ replicate 6 (ComputerPlayerConfig False 0 0 MoveEvaluator 0 0 10)
    }