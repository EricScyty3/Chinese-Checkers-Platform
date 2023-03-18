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
import Extension

-- the container for storing the parameters for MCTS frameworks
-- the configuration of a single computer player
data ComputerPlayerConfig = ComputerPlayerConfig {
  _active :: Bool, -- whether a comptuer player is placed at this position
  _uct :: Double,  -- one of the the selection policy's parameters 
  _ph :: Double,   -- same as above
  _evaluator :: PlayoutEvaluator, -- how the board is evaluated when the simulation is taken place
  _depth :: Int,   -- if the minimax search is applied, the related search depth is needed
  _control :: Int, -- the choice of how the MCTS is processed
  _cvalue :: Int   -- and how the control's threshold should be
} deriving (Eq, Show)

newtype ConfigList = ConfigList {
  _configList :: [ComputerPlayerConfig] -- stores a list of configuration for all potential computer players
} deriving (Eq, Show)

-- the GUI for the game platform allowing user to set player and board configuration
-- the model representation indicates the state of the application: the information stored that models the subjects
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing
  _playerIndex :: Int,
  -- a flag that declares the initial state of the game is setup, and can be started
  _startGame :: Bool,
  -- a flag that declares a player won the game, and any other actions should be stopped 
  _ifWin :: Bool,
  -- the representation of the board to be printed on the screen
  _displayBoard :: Board,
  -- the internal pieces' positions for each player (projected from the display board and can be reversed), 
  -- reflecting the personal state for the players, and can be used to determine the win state
  _internalStates :: [[Pos]],
  -- the total players in the game
  _playersAmount :: Int,
  -- the current player's move is defined as moving from a position to another new position
  -- therefore, containing a start and an end
  _startPos :: BoardPos,
  _endPos :: BoardPos,
  -- the move made by the last player previously, used for canceling if needed
  _previousStartPos:: BoardPos,
  _previousEndPos :: BoardPos,
  -- a message that will be display when an incorrect state appears
  _errorMessage :: String,
  -- a list of avaliable posistions that a player can move from an entered position
  _movesList :: [BoardPos],
  -- the history record of the game, which could be made use of by the MCTS
  _gameHistory :: HistoryTrace,
  -- shows at the menu page, indicating the current player to be configured
  _pageIndex :: Int,
  -- the container that holds the configuration of a list of potential computer players
  _defaultConfig :: ConfigList
} deriving (Eq, Show)

-- the event that the model triggers and handles, the different actions that the handler could react to
data AppEvent
  = AppInit -- the initialisation of the model status
  | MoveCheck BoardPos Int -- determine if a movement input is valid, if it is then render it, otherwise, display an error message
  | StartGameButtonClick -- initialise the game configuration depending on user's settings
  | RenderMove  -- show the change of the movement
  | CancelMove  -- cancel the last played movement
  | EndGameButtonClick -- quit the current game and back to the menu page
  | CancelChecked Int -- modify the checkbox's result of computer players when the total players is changed
  | PageUpdate Int -- update the page index for the setting panel
  | ComputerAction -- perform the move made by the computer player
  deriving (Eq, Show)

makeLenses 'ComputerPlayerConfig
makeLenses 'ConfigList
makeLenses 'AppModel

-- produce the title text 
titleText :: AppModel -> String
titleText model
  | not (model ^. startGame) = "Haskell Chinese Checkers" -- print welcome text at the menu page
  | model ^. ifWin = name ++ show turn ++ " wins" -- print winning player's colour when the win state of a player is achieved
  | otherwise = name ++ show turn ++ "'s turn" -- print current player's turn and colour if win state is not satisfied
  where
    turn = model ^. playerIndex
    name = if model ^. defaultConfig ^?! configList . ix turn . active then "Computer Player " else "Player "
    -- determine if the current player is an AI or human player

-- determine if the state of fromPiece, toPiece, previousFromPiece, previousToPiece, is changed
ifInitialPiece :: BoardPos -> Bool
ifInitialPiece pos = pos == initialPos
-- the default position, which just indicate nothing
initialPos :: BoardPos
initialPos = U (-1, -1)

-- construct the graphical layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  -- render the color and index of a piece for certain player on the board
  pieceButton :: BoardPos -> WidgetNode AppModel AppEvent
  pieceButton pos
    -- place a spacer if the position type is "U"
    | isSpacer pos = spacer
    -- otherwise, place a button with related colour and index
    | otherwise = button_ (T.pack textPi) (MoveCheck pos pi) [onClick RenderMove]
      `styleBasic`[radius 45, bgColor white, border 2 white, textSize 20, textColor white, -- empty positions will be coloured in white
                   styleIf (compareColour pos Red)    (bgColor red),
                   styleIf (compareColour pos Blue)   (bgColor blue),
                   styleIf (compareColour pos Green)  (bgColor green),
                   styleIf (compareColour pos Purple) (bgColor purple),
                   styleIf (compareColour pos Orange) (bgColor darkOrange),
                   styleIf (compareColour pos Black)  (bgColor black),
                   styleIf ({-pos == spiece ||-} pos `elem` model ^. movesList) (bgColor pieceColour),
                   styleIf (pos == spiece) (border 2 pieceColour)
                  ]
                  -- not only the pieces are in colour, but also the avaliable destinations
                  -- additionally, the clicked position's boarder will be printed differently
    where
      pi = getPlayerIndex (model ^. playersAmount) pos
      textPi = if pi == (-1) then "" else show pi

      spiece = model ^. startPos
      pieceColour
        | compareColour spiece Red = red
        | compareColour spiece Blue = blue
        | compareColour spiece Green = green
        | compareColour spiece Purple = purple
        | compareColour spiece Orange = darkOrange
        | compareColour spiece Black = black
        | otherwise = white

      getPlayerIndex players piece = case getColour piece of
                                      Nothing -> (-1)
                                      Just c  -> fromMaybe (-1) (elemIndex c (playerColourList players))

  -- display a row of buttons for rendering the board
  rowButton :: [BoardPos] -> WidgetNode AppModel AppEvent
  rowButton ps = hgrid (pieceButton <$> ps) -- render the rows of button in a grid style

  -- display the player settings, the amount of AI plauers
  computerPlayersChoices :: Int -> WidgetNode AppModel AppEvent
  computerPlayersChoices idx = labeledCheckbox_ (showt idx) (defaultConfig . configList . singular (ix idx) . active) [checkboxSquare, textRight]

  -- the access to the element of the config list of the current setting panel index
  pi = model ^. pageIndex
  vitem = model ^. defaultConfig ^?! configList . ix pi
  mitem = defaultConfig . configList . singular (ix pi)
  eitem = defaultConfig . configList . singular (ix pi) . evaluator
  ditem = defaultConfig . configList . singular (ix pi) . depth
  citem = defaultConfig . configList . singular (ix pi) . control
  cvitem = defaultConfig . configList . singular (ix pi) . cvalue

  featureLayer =
      vstack [
        -- slider for controlling the selection parameters: UCT and PH's constants
        label (T.pack ("Exploration Factor: " ++ show (vitem ^. uct))) `styleBasic` [textSize 20],
        spacer,
        hslider_ (mitem.uct) 0 5 [dragRate 0.1] `styleBasic` [fgColor orange],
        spacer,
        label (T.pack ("History Factor: " ++ show (vitem ^. ph))) `styleBasic` [textSize 20],
        spacer,
        hslider_ (mitem.ph) 0 5 [dragRate 0.1] `styleBasic` [fgColor orange],
        spacer,

        hstack [
          -- radio for controlling the playout evaluator and the corresponding search depth
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
            -- the choice of the control of the MCTS procedure and the corresponding value 
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
          hslider cvitem 1 100 `styleBasic` [fgColor orange]
        ]
      ] `styleBasic` [maxWidth 600, border 2 white, padding 20, radius 10]

  selectButtonLayer =
      -- the button for switching the panel's page
      hstack [
        button "Last" (PageUpdate (-1)) `nodeEnabled` (pi > 0),
        filler,
        label (T.pack ( playerType ++ show pi))
        `styleBasic` [textSize 30],
        filler,
        button "Next" (PageUpdate 1) `nodeEnabled` (pi < model ^. playersAmount - 1)
      ] `styleBasic` [maxWidth 600, padding 20]

    where
      playerType = if vitem ^. active then "Computer Player: " else "Human Player: "

  playerLayer =
    hstack[
      vstack [
        label "Total Players",
        spacer_ [width 15],
        label "Computer Players"
      ]`styleBasic` [textSize 20],

      spacer,
      box_ [alignTop] $ vstack [
        -- control the total players
        box_ [alignLeft] $ hgrid_ [childSpacing_ 10] [
          -- the change of total players amount will also change the allowed AI players
          labeledRadio_ "2" 2 playersAmount [onChange CancelChecked, textRight],
          labeledRadio_ "3" 3 playersAmount [onChange CancelChecked, textRight],
          labeledRadio_ "4" 4 playersAmount [onChange CancelChecked, textRight],
          labeledRadio_ "6" 6 playersAmount [onChange CancelChecked, textRight]
        ],

        spacer,
        -- as well as the position of computer players
        box_ [alignLeft] $ hgrid_ [childSpacing_ 10] (computerPlayersChoices <$> [0.. model ^. playersAmount - 1])
      ]
    ] `styleBasic` [maxWidth 600, border 2 white, padding 20, radius 10]

  -- build layout of the application
  widgetTree =
      vstack [
        -- the title text
        box $ label_ (T.pack $ titleText model) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],
        -- the setting panel
        filler,
        vstack [
          box featureLayer  `nodeEnabled` (vitem ^. active),
          spacer,
          box selectButtonLayer,
          spacer,
          box playerLayer
        ] `nodeVisible` not (model ^. startGame),

        -- the game components  
        spacer `nodeVisible` (model ^. startGame),
        box $ label_ (T.pack $ model ^. errorMessage) [ellipsis]
                  `styleBasic` [textFont "Italic", textSize 20]
                  `nodeVisible` (model ^. startGame),
        spacer `nodeVisible` (model ^. startGame),

        vstack [
          -- error message text
          -- the two buttons that allow user to quit the game or cancel the last move made
          box $ hgrid[
              filler,
              button "Quit" EndGameButtonClick,
              filler,
              button "Cancel" CancelMove,
              filler
          ]`styleBasic`[textSize 20],
          -- render the board's positions row by row
          vgrid_ [childSpacing_ 5] (rowButton <$> (model ^. displayBoard))
        ] `nodeVisible` (model ^. startGame),

        -- the button for starting the game
        spacer `nodeVisible` not (model ^. startGame),
        box $ button "Start Game" StartGameButtonClick `styleBasic`[textSize 30] `nodeVisible` not (model ^. startGame),
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
  AppInit -> [{-Producer computerTurn-}] -- an additional thread for checking computer player's turn
  -- setup the game board according to the options in the menu page: 
  -- declare that the game is started letting certain subjects to be visiable or invisiable
  -- generate the game board based on the chosen players number, and the corresponding length of hash state
  StartGameButtonClick -> [Model $ model & startGame .~ True
                                         & displayBoard .~ eraseBoard (playerColourList pn) externalBoard
                                         & internalStates .~ replicate pn startBase]
    where
      pn = model ^. playersAmount

  -- update the page index with given increment
  PageUpdate x -> [Model $ model & pageIndex .~ (model ^. pageIndex) + x]

  -- the event of cancel the last piece change made, and revert it to the previous board state   
  -- movement cancel is not allowed to be made over a player, meaning that once another player makes the move, you can no longer cancel yours                             
  CancelMove
    | model ^. ifWin || ifExistComputerPlayer -> [] -- ignore if the win state is confirmed or computer player involves
    | not (ifInitialPiece ps) && not (ifInitialPiece pe) -> [Model $ model & displayBoard .~ newBoard -- reset the board, hash, and turn states
                                                                           & internalStates .~ insertState
                                                                           & previousStartPos .~ initialPos
                                                                           & previousEndPos .~ initialPos
                                                                           & playerIndex .~ lastTurn]
    | otherwise -> [] -- ignore if not record is stored for that
    where

      ifExistComputerPlayer = all getActive [0..5]
      getActive idx = model ^. defaultConfig ^?! configList . ix idx . active
      ps = model ^. previousStartPos
      pe = model ^. previousEndPos
      pn = model ^. playersAmount
      newBoard = repaintPath (model ^. displayBoard) (pe, ps)
      -- revert the board state

      -- get the last turn value and retrieve the information for reverting the hash state
      lastTurn = turnRevert pn (model ^. playerIndex)
      lastColour = playerColour lastTurn pn
      ips = projection lastColour (getPos ps)
      ipe = projection lastColour (getPos pe)

      lastTurnState = (model ^. internalStates) !! lastTurn
      newState = flipBoard lastTurnState (ipe, ips)
      -- revert the internal positions and then replace it in the state list
      insertState = replace lastTurn newState (model ^. internalStates)

  -- after a movement is determined valid, it will be rendered
  RenderMove
    | model ^. ifWin -> [] -- if already won then do nothing
    -- otherwise perform the change if a movement is constructed 
    | not (ifInitialPiece ep) -> [Model $ model & displayBoard .~ newBoard
                                                & internalStates .~ insertState
                                                & playerIndex .~ newTurn
                                                & previousStartPos .~ sp
                                                & previousEndPos .~ ep
                                                & startPos .~ initialPos
                                                & endPos .~ initialPos
                                                & movesList .~ []
                                                & ifWin .~ newWinState] -- if a win is reached then update it
    | otherwise -> []
      where
        sp = model ^. startPos
        ep = model ^. endPos
        pi = model ^. playerIndex
        newBoard = repaintPath (model ^. displayBoard) (sp, ep) -- generate the new board state by re-colouring the two board positions
        currentColour = playerColour pi (model ^. playersAmount)
        currentState = (model ^. internalStates) !! pi
        isp = projection currentColour (getPos sp)
        iep = projection currentColour (getPos ep)
        newState = flipBoard currentState (isp, iep)
        insertState = replace pi newState (model ^. internalStates) -- replace the old hash state with the new one
        newWinState = winStateDetect newState
        newTurn = if not newWinState then turnBase (model ^. playersAmount) pi else pi

  -- quit the game and return back to the menu page
  EndGameButtonClick -> [Model $ model & playerIndex .~ 0 -- resetting all states
                                       & ifWin .~ False
                                       & startGame .~ False
                                       & startPos .~ initialPos
                                       & endPos .~ initialPos
                                       & previousStartPos .~ initialPos
                                       & previousEndPos .~ initialPos
                                       & errorMessage .~ ""
                                       & movesList .~ []
                                       & gameHistory .~ RBLeaf]

  -- the movement check, how a movement is passed is done as follows:
  -- first enter the starting point, and check for the correctness
  -- then enter the destination, if no error is made then process, otherwise, discard that and print the error message 
  MoveCheck pos pi ->
              if model ^. ifWin || ifComputersTurn then [] -- if already won or it's computer's turn, then do nothing
              else if ifInitialPiece sp  -- if it is the first time for entering the position
                   then if turn == pi then [Model $ model & startPos .~ pos     -- check if the entered position is fitted for the current player
                                                          & errorMessage .~ ""  -- if valid, then checkout the avaliable movements
                                                          & movesList .~ newMovesList]
                                      else [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": invalid start"
                                                          & startPos .~ initialPos] -- if not, then discard this, and wait for another valid input
                   else if sp == pos then -- otherwise, this position is clicked twice, and is an invalid movement
                                          [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": no effective move made"
                                                         & startPos .~ initialPos]
                        else -- check whether the second clicked position (destination) is avaliable/reachable from the first entered position
                             if isOccupied pos then [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": destination occupied" -- if the destination is occupied, then invalid
                                                                   & startPos .~ initialPos]
                                               else if pos `elem` model ^. movesList
                                                    then [Model $ model & endPos .~ pos
                                                                        & errorMessage .~ ""] -- if reachable, then this movement will then be rendered
                                                    else [Model $ model & errorMessage .~ "Player " ++ show turn ++ ": destination unreacbable" -- if the desintation is not in the list, then invalid
                                                                        & startPos .~ initialPos]
                {-case ifInitialPiece $ model ^. fromPiece of 
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
      turn = model ^. playerIndex
      sp = model ^. startPos
      ep = model ^. endPos
      ifComputersTurn = model ^. defaultConfig ^?! configList . ix turn . active
      newMovesList = evalState (destinationList pos) (model ^. displayBoard)

  -- reset the selected computer player's indices when the total players is changed
  CancelChecked v -> [Model $ model & defaultConfig . configList . ix 0 . active .~ head newList
                                    & defaultConfig . configList . ix 1 . active .~ newList !! 1
                                    & defaultConfig . configList . ix 2 . active .~ newList !! 2
                                    & defaultConfig . configList . ix 3 . active .~ newList !! 3
                                    & defaultConfig . configList . ix 4 . active .~ newList !! 4
                                    & defaultConfig . configList . ix 5 . active .~ newList !! 5
                                    & pageIndex .~ newPageId]
    where
      newList = take v remainedList ++ replicate (6 - v) False
      remainedList = map modelList [0..5]
      modelList x = model ^?! defaultConfig . configList . ix x . active
      newPageId = if v <= model ^. pageIndex then v - 1 else model ^. pageIndex

  -- only react when the during the game and is currently the computer player's turn
  ComputerAction -> [] {-[Model $ model & displayBoard .~ newBoard
                                   & internalStates .~ insertState
                                   & gameHistory .~ newHistoryTree
                                   & playerIndex .~ newTurn
                                   & previousStartPos .~ initialPos
                                   & previousEndPos .~ initialPos
                                   & ifWin .~ newWinState | model ^. startGame && ifComputersTurn && not (model ^. ifWin)]
                    {-case model ^. startGame && (model ^. turnS `elem` model ^. computerIdxList) && not (model ^. ifWin) of
                      True  -> [Model $ model & displayBoard .~ newBoard
                                              & internalStates .~ insertState
                                              & gameHistory .~ newHistoryTree
                                              & turnS .~ newTurn
                                              & previousFromPiece .~ U(-1, -1)
                                              & previousToPiece .~ U(-1, -1)
                                              & ifWin .~ newWinState
                                              ] -- apply the movement retrieved from the MCTS decision, and perform
                      False -> []-}
    where
      pi = model ^. playerIndex
      ifComputersTurn = model ^. defaultConfig ^?! configList . ix pi . active
      (newBoard, newState, newHistoryTree) = aiDecision model
      insertState = replace pi newState (model ^. internalStates)
      newWinState = winStateDetect newState
      newTurn = if not newWinState then turnBase (model ^. playersAmount) pi else pi-}

{-
-- keep calling the event for checking the computer player's movement
computerTurn :: (AppEvent -> IO a) -> IO b
computerTurn sendMsg = do sendMsg ComputerAction
                          threadDelay $ 1000 * 800 -- check every 800 microseconds
                          computerTurn sendMsg

-- pass the model information to the MCTS interface and accept the returned board state
aiDecision :: AppModel -> (Board, [Pos], HistoryTrace)
aiDecision model = let root = GRoot 0 []
                       (newBoard, newState, nht, _) = finalSelection root
                                                      (pi, 1, eboard, iboard, pn, ht, (uctCons, phCons), (eval, dep))
                                                      (mctsControl mctsCon ctVal)
                   in  (newBoard, newState, nht)
  where
    pi = model ^. playerIndex
    eboard = model ^. displayBoard
    iboard = model ^. internalStates
    pn = model ^. playersAmount
    ht = model ^. gameHistory

    uctCons = model ^. defaultConfig ^?! configList . ix pi . uct
    phCons = model ^. defaultConfig ^?! configList . ix pi . ph
    eval = model ^. defaultConfig ^?! configList . ix pi . evaluator
    dep = model ^. defaultConfig ^?! configList . ix pi . depth

    mctsCon = model ^. defaultConfig ^?! configList . ix pi . control
    ctVal = model ^. defaultConfig ^?! configList . ix pi . cvalue

    mctsControl 0 x = (Just x, Nothing, Nothing)
    mctsControl 1 x = (Nothing, Just x, Nothing)
    mctsControl _ x = (Nothing, Nothing, Just (fromIntegral x))
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
      _playerIndex = 0,
      _ifWin = False,
      _displayBoard = externalBoard,
      _internalStates = [],
      _playersAmount = 2,
      _startGame = False,
      _startPos = initialPos,
      _endPos = initialPos,
      _previousStartPos = initialPos,
      _previousEndPos = initialPos,
      _errorMessage = "",
      _movesList = [],
      _gameHistory = RBLeaf,
      _pageIndex = 0,
      _defaultConfig = ConfigList $ replicate 6 (ComputerPlayerConfig False 3 1 MoveEvaluator 0 0 10)
    }