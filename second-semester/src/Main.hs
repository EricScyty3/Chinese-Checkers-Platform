{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Monomer
  ( nodeEnabled,
      nodeVisible,
      darkTheme,
      black,
      blue,
      darkOrange,
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
      appWindowResizable,
      appWindowState,
      appWindowTitle,
      styleIf,
      box,
      box_,
      hgrid,
      hgrid_,
      vgrid_,
      hstack,
      vstack,
      button,
      label,
      label_,
      labeledCheckbox_,
      labeledRadio_,
      hslider,
      hslider_,
      filler,
      spacer,
      spacer_,
      CmbAlignLeft(alignLeft),
      CmbAlignTop(alignTop),
      CmbBgColor(bgColor),
      CmbBorder(border),
      CmbChildSpacing(childSpacing_),
      CmbDragRate(dragRate),
      CmbEllipsis(ellipsis),
      CmbFgColor(fgColor),
      CmbMaxWidth(maxWidth),
      CmbOnChange(onChange),
      CmbPadding(padding),
      CmbRadius(radius),
      CmbStyleBasic(styleBasic),
      CmbStyleHover(styleHover),
      CmbTextColor(textColor),
      CmbTextFont(textFont),
      CmbTextRight(textRight),
      CmbTextSize(textSize),
      CmbWidth(width),
      WidgetEnv,
      WidgetNode,
      AppEventResponse,
      MainWindowState(MainWindowNormal),
      EventResponse(Task, Model),
      CmbCheckboxMark(checkboxSquare), toggleButton, optionButton, appExitEvent )
import Control.Lens ( (&), (^?!), (^.), (+~), (.~), makeLenses, singular, Ixed(ix) )
import Data.Maybe ( fromMaybe )
import Data.Text (Text)
import TextShow ( TextShow(showt) )
import Board
    ( Pos,
      compareColour,
      destinationList,
      erase,
      eraseBoard,
      externalBoard,
      getColour,
      getPos,
      isOccupied,
      isSpacer,
      playerColourList,
      projection,
      repaint,
      repaintPath,
      replace,
      startBase,
      Board,
      BoardPos(U),
      Colour(Black, Red, Blue, Green, Purple, Orange), colourIndex, initialPos, ifInitialPiece, removeByIdx )
import Control.Monad.State ( evalState )
import qualified Data.Text as T
import Zobrist ( flipBoard, winStateDetect )
import GameTree
    ( playerColour,
      turnBase,
      GameTree(GRoot),
      HistoryTrace,
      KillerMoves,
      PlayoutEvaluator(..) )
import RBTree ( RBTree(RBLeaf) )
import Configuration (LookupTable, lookupTable)
import Data.List (elemIndex)
import Extension ( finalSelection )
import System.Random ( newStdGen )
import Control.Concurrent (MVar, ThreadId, newMVar, newEmptyMVar, takeMVar, putMVar, forkFinally, killThread, myThreadId)
import GHC.IO (unsafePerformIO)

threadPool :: MVar [ThreadId]
{-# NOINLINE threadPool #-}
threadPool = unsafePerformIO (newMVar [])

discardThread :: ThreadId -> IO ()
discardThread id = do
    existingThreads <- takeMVar threadPool
    case id `elemIndex` existingThreads of
      Nothing -> error "Error Thread"
      Just idx -> -- update the record of running threads
                  let newThreadPool = removeByIdx idx existingThreads
                  in  do putMVar threadPool []

appendThread :: ThreadId -> IO ()
appendThread id = do
    existingThreads <- takeMVar threadPool
    putMVar threadPool (id:existingThreads) -- add the new generated thread'id to the record

endAllThread :: IO ()
endAllThread = do
  -- get all thread ids and terminate them
  existingThreads <- takeMVar threadPool
  mapM_ killThread existingThreads
  putMVar threadPool [] -- reset the thread pool

-- the container for storing the parameters for a selected computer player
data ComputerPlayerConfig = ComputerPlayerConfig {
  _active :: Bool, -- whether a computer player is active at this position
  _uct :: Double,  -- the constant of UCT formula in MCTS selection 
  _ph :: Double,   -- the constant of Progressive History in MCTS selection
  _evaluator :: PlayoutEvaluator, -- the board evaluator used during the MCTS playouts
  _depth :: Int,   -- if the embedded minimax search is applied during the playouts, the related search depth is needed to be defined
  _percentage :: Int, -- if the embedded minimax search is applied during the playouts, the search could be triggered based on certain possibility 
  _control :: Bool, -- the choice of how the MCTS is processed, including iteration counts, time limits and tree expansions
  _cvalue :: Int   -- as well as the exact control's value
} deriving (Eq, Show)

-- a list of configuration for each computer player
newtype ConfigList = ConfigList {
  _configList :: [ComputerPlayerConfig]
  -- since it is a list, if wanting to check whether a computer player is active, just check the boolean flag at that index
} deriving (Eq, Show)

-- the GUI for the game platform allowing user to set player and board configuration
-- the model representation indicates the state of the application: the information stored that models the subjects
data AppModel = AppModel {
  -- the integer that identifies which player is currently playing
  _playerIndex :: Int,
  -- a flag that declares the game is started
  _startGame :: Bool,
  -- a flag that declares a player has win the game, and any action after that should be ignored except the "Quit" button clicked 
  _ifWin :: Bool,
  -- the representation of the board to be printed on the screen
  _displayBoard :: Board,
  -- the internal pieces' positions for each player (projected from the display board and can be reversed), 
  -- reflecting the personal state for the players, and can be used to determine the win state through certain hash function
  _internalStates :: [[Pos]],
  -- the total players in the game
  _playersAmount :: Int,
  -- the current player's move is defined as moving from a position to another, therefore, should contain a start and an end
  _startPos :: BoardPos,
  _endPos :: BoardPos,
  -- hint/error message will be displayed when needed
  _errorMessage :: String,
  -- a list of available position that a player can move from a position that was chosen
  _movesList :: [BoardPos],
  -- the history record of the game, which could be made use of by each MCTS player
  _gameHistory :: [HistoryTrace],
  -- indicates the current player to be configured at the setting dialog
  _pageIndex :: Int,
  -- a list of potential computer players' configurations
  _playerConfigs :: ConfigList,
  -- indicate whether a public memory is allowed
  _ifMemoryShared :: Bool
} deriving (Eq, Show)

-- the event that the model triggers and handles as well as the responses
data AppEvent
  = -- the initialisation of the model status
    AppInit ()
    -- determine if a movement input by the player is valid
  | MoveCheck BoardPos Int
    -- initialise the board game based on user's input
  | StartGameButtonClick
    -- if the movement passes the validity check, then display the change onto the board
  | RenderMove
    -- quit the current game and navigate back to the menu
  | EndGameButtonClick
    -- modify the checkbox's results when the total players is changed
  | CancelChecked Int
    -- update the page index for the setting panel
  | PageUpdate Int
    -- display the move made by the computer player onto the board
  | RenderComputerAction (Board, [Pos], HistoryTrace)
    -- generate the corresponding movement based on the current game state and pass the result to the "RenderComputerAction" event
  | GenerateComputerAction
    -- jump to the next player's turn, as well as check the next player's state
  | TurnSwitch
  deriving (Eq, Show)

makeLenses 'ComputerPlayerConfig
makeLenses 'ConfigList
makeLenses 'AppModel

-- construct the graphical layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  -- produce the title text 
  titleText :: String
  titleText
      -- print welcome text at the menu page when the game is not started yet
    | not (model ^. startGame) = "Haskell Chinese Checkers"
      -- print a message when the win state of a player is achieved
    | model ^. ifWin = playerType ++ show pi ++ " wins"
      -- print the current player's turn to remind the user
    | otherwise = playerType ++ show pi ++ "'s turn"
    where
      pi = model ^. playerIndex
      -- determine if the current player is an AI or human player
      playerType = if model ^. playerConfigs ^?! configList . ix pi . active then "Computer Player " else "Player "

  -- display a row of buttons for colouring and indexing them
  renderRowButtons :: [BoardPos] -> WidgetNode AppModel AppEvent
  renderRowButtons ps = hgrid (pieceButton <$> ps) -- order in a grid style
    where
      -- render the colour and index of a piece for certain player on the board
      pieceButton pos
        -- place a spacer if the position type is "U", which is just for separating the buttons
        | isSpacer pos = spacer
        -- otherwise, place a button with related color and index
        | otherwise = button (T.pack textBi) (MoveCheck pos ci)
          `styleBasic`[radius 45, bgColor (pieceColour pos), border 2 white, textSize 20, textColor white,
                      -- besides, once a button is clicked, the available destinations will be rendered with corresponding colour 
                      styleIf (pos `elem` model ^. movesList) (bgColor (pieceColour sp)),
                      -- and the clicked button's border will be repainted as well
                      styleIf (pos == sp) (border 2 (pieceColour sp))
                      ]
          `styleHover` [styleIf (pos `elem` model ^. movesList) (border 2 yellow)] -- allow the button border to change when the mouse hovers
        where
          -- get the player index based on the clicked button's colour, could be used to determine if the clicked button fit the current turn
          -- the ones has no colour or is not in the current colour list due to the number of the players are assigned with (-1)
          ci = case getColour pos of
                Nothing -> (-1)
                Just c  -> fromMaybe (-1) $ colourIndex c (model ^. playersAmount)
          -- the empty button shows no colour, therefore, will display nothing, while others will show their indices
          textBi = if ci == (-1) then "" else show ci

          sp = model ^. startPos
          -- "E" position will be coloured in white
          -- other buttons with different type "R, B, G, P, O, K" will be coloured correspondingly
          pieceColour inputPos
            | compareColour inputPos Red = red
            | compareColour inputPos Blue = blue
            | compareColour inputPos Green = green
            | compareColour inputPos Purple = purple
            | compareColour inputPos Orange = darkOrange
            | compareColour inputPos Black = black
            | otherwise = white

  -- display the checkbox for choosing the number of AI players
  computerPlayersChoices :: Int -> WidgetNode AppModel AppEvent
  computerPlayersChoices idx = labeledCheckbox_ (showt idx) (playerConfigs . configList . singular (ix idx) . active) [checkboxSquare, textRight]

  -- the access to the element of the player configuration based on the current panel page
  -- current page
  pageIdx = model ^. pageIndex
  -- retrieve an (read-only) instance of the configuration list
  vitem = model ^. playerConfigs ^?! configList . ix pageIdx
  -- retrieve an (changeable) instance of the configuration list
  mitem = playerConfigs . configList . singular (ix pageIdx)
  -- the detailed instance's information from the above instance
  eitem = playerConfigs . configList . singular (ix pageIdx) . evaluator
  ditem = playerConfigs . configList . singular (ix pageIdx) . depth
  citem = playerConfigs . configList . singular (ix pageIdx) . control
  cvitem = playerConfigs . configList . singular (ix pageIdx) . cvalue
  pitem = playerConfigs . configList . singular (ix pageIdx) . percentage

  -- the layer that integrates the options of player's settings
  featureLayer =
      vstack [
        -- slider for controlling the selection parameters: UCT and PH constants
        label (T.pack ("Exploration Factor: " ++ show (vitem ^. uct))) `styleBasic` [textSize 20],
        spacer,
        hslider_ (mitem.uct) 0 5 [dragRate 0.1] `styleBasic` [fgColor orange],
        spacer,
        label (T.pack ("History Factor: " ++ show (vitem ^. ph))) `styleBasic` [textSize 20],
        spacer,
        hslider_ (mitem.ph) 0 5 [dragRate 0.1] `styleBasic` [fgColor orange],
        spacer,

        hstack [
          -- radio for controlling the playout evaluator
          box_ [alignTop] $ vgrid_ [childSpacing_ 5] [
            label "Simulation Evaluator" `styleBasic` [textSize 20],
            labeledRadio_ "Random Choice" Random eitem [textRight],
            labeledRadio_ "Move Distance" Move eitem [textRight],
            labeledRadio_ "Lookup Table" Board eitem [textRight],
            -- and the corresponding search depth if minimax search is chosen
            hstack [
              labeledRadio_ "Midgame Paranoid" MParanoid eitem [textRight],
              spacer,
              hgrid_ [childSpacing_ 5] [
                label "(depth)",
                labeledRadio_ "2" 2 ditem [textRight],
                labeledRadio_ "3" 3 ditem [textRight],
                labeledRadio_ "4" 4 ditem [textRight]
              ] `nodeVisible` (vitem ^. evaluator == MParanoid)
            ],

            hstack [
              labeledRadio_ "Midgame BRS" MBRS eitem [textRight],
              spacer,
              hgrid_ [childSpacing_ 5] [
                label "(depth)",
                labeledRadio_ "2" 2 ditem [textRight],
                labeledRadio_ "3" 3 ditem [textRight],
                labeledRadio_ "4" 4 ditem [textRight]
              ] `nodeVisible` (vitem ^. evaluator == MBRS)
            ]
          ],

          filler,
          box_ [alignTop] $
            -- the choice of control the progress of MCTS, either iterations, time, and expansion
            vgrid_ [childSpacing_ 10] [
              label "MCTS Control" `styleBasic` [textSize 20],
              labeledRadio_ "Iterations" True citem [textRight],
              labeledRadio_ "Time (seconds)" False citem [textRight]
            ]
        ],

        spacer,
        -- the corresponding value of the control threshold
        vstack [
          label (T.pack ("Control Value: " ++ show (vitem ^. cvalue) ++ (if vitem ^. control then " rounds" else " seconds"))) `styleBasic` [textSize 20],
          spacer,
          hslider cvitem 1 100 `styleBasic` [fgColor orange],

          spacer,
          label (T.pack ("Percentage Value (Minimax-only): " ++ show (vitem ^. percentage) ++ "%")) `styleBasic` [textSize 20],
          spacer,
          hslider pitem 1 100 `styleBasic` [fgColor orange] `nodeEnabled` (vitem ^. evaluator == MParanoid || vitem ^. evaluator == MBRS)
        ]
      ] `styleBasic` [maxWidth 650, border 2 white, padding 20, radius 10]

  -- the layer that allow the user to flip the page of the setting panel
  selectButtonLayer =
      -- the button for switching the page, next or last page
      hstack [
        button "Last" (PageUpdate (-1)) `nodeEnabled` (pageIdx > 0),
        filler,
        label (T.pack ( playerType ++ show pageIdx))
        `styleBasic` [textSize 30],
        filler,
        -- the maximum page index is restricted when the total players number is changed
        button "Next" (PageUpdate 1) `nodeEnabled` (pageIdx < model ^. playersAmount - 1)
      ] `styleBasic` [maxWidth 650, padding 20]

    where
      -- besides, the description of the player is updated based on the user's choice
      playerType = if vitem ^. active then "Computer Player: " else "Human Player: "

  -- the layer for user to set the number of human players and computer players, as well as order them
  playerLayer =
    hstack[
      vstack [
        label "Total Players",
        spacer_ [width 15],
        label "Computer Players"
      ]`styleBasic` [textSize 20],

      spacer,
      box_ [alignTop] $ vstack [
        -- the radio buttons for controlling the total players
        box_ [alignLeft] $ hgrid_ [childSpacing_ 10] [
          -- the change of total players amount will also change the allowed AI players
          labeledRadio_ "2" 2 playersAmount [onChange CancelChecked, textRight],
          labeledRadio_ "3" 3 playersAmount [onChange CancelChecked, textRight],
          labeledRadio_ "4" 4 playersAmount [onChange CancelChecked, textRight],
          labeledRadio_ "6" 6 playersAmount [onChange CancelChecked, textRight]
        ],

        spacer,
        -- as well as the position of computer players (checkbox)
        box_ [alignLeft] $ hgrid_ [childSpacing_ 10] (computerPlayersChoices <$> [0.. model ^. playersAmount - 1])
      ],
      filler,
      -- the flag of activing the public memory
      toggleButton "Shared Memory" ifMemoryShared
    ] `styleBasic` [maxWidth 650, border 2 white, padding 20, radius 10]

  -- combine all layers and other components together onto the screen
  widgetTree =
      vstack [
        -- the title text
        box $ label_ (T.pack titleText) [ellipsis] `styleBasic` [textFont "Bold", textSize 50],

        filler,
        -- the setting panel
        vstack [
          box featureLayer  `nodeEnabled` (vitem ^. active),
          spacer,
          box selectButtonLayer,
          spacer,
          box playerLayer
        ] `nodeVisible` not (model ^. startGame), -- not visible when the game is started

        -- the hint/error message, only visible when the game is started
        spacer `nodeVisible` (model ^. startGame),
        box $ label_ (T.pack $ model ^. errorMessage) [ellipsis]
                  `styleBasic` [textFont "Italic", textSize 20]
                  `nodeVisible` (model ^. startGame),
        spacer `nodeVisible` (model ^. startGame),

        vstack [
          -- the buttons that allow user to quit the game
          box $ button "End Game" EndGameButtonClick `styleBasic` [textSize 20],
          spacer,
          -- render the board row by row
          vgrid_ [childSpacing_ 5] (renderRowButtons <$> (model ^. displayBoard))
        ] `nodeVisible` (model ^. startGame), -- only visible when the game is started

        -- the button for starting the game, and will not be seen once clicked
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
  AppInit () -> []

  -- setup the game board according to the options in the menu page
  -- declare that the game is started letting certain subjects to be visible or invisible
  -- initialise the game board based on the chosen players number, and other corresponding states
  StartGameButtonClick -> [Model $ model & startGame .~ True
                                         & displayBoard .~ eraseBoard (playerColourList pn) externalBoard
                                         & internalStates .~ replicate pn startBase
                                         & movesList .~ []
                                         & gameHistory .~ replicate pn RBLeaf
                                         & startPos .~ initialPos
                                         & endPos .~ initialPos
                                         & playerIndex .~ 0
                                         & ifWin .~ False
                                         & errorMessage .~ "Please perform a move",
                          Task $ return GenerateComputerAction -- also check if the first turn is played by the computer player
                          ]

  -- quit the game and return back to the menu page
  EndGameButtonClick -> [Model $ model & startGame .~ False, 
                         Task $ AppInit <$> endAllThread] 
                         -- also terminate the running threads, for avoiding program stall, 
                         -- but could cause exception (which can be ignored)

  -- update the page index with given increment or decrement
  PageUpdate x -> [Model $ model & pageIndex +~ x]

  -- update the turn index when a movement is complete
  TurnSwitch -> [Model $ model & playerIndex .~ newTurn -- update the game turn and internal state, check the win state, as well as clean the buffer
                               & ifWin .~ newWinState
                               & startPos .~ initialPos
                               & endPos .~ initialPos
                               & internalStates .~ insertState
                               & errorMessage .~ if newWinState then "Congratulations" else "Please perform a move",
                    Task $ return GenerateComputerAction] -- also determine the next turn's player 
    where
      currentInternalState = iboards !! pi
      isp = projection currentColour (getPos sp)
      iep = projection currentColour (getPos ep)

      newInternalState = flipBoard currentInternalState (isp, iep) -- update the move change from the external board to the internal form
      newWinState = winStateDetect newInternalState -- check if this movement lead to a win state for a player

      insertState = replace pi newInternalState iboards -- update the new internal state to the list of states
      newTurn = if not newWinState then turnBase pn pi else pi -- based on the win state determination, decide if it is needed to update the game turn 

  -- after a movement is determined valid, it will be rendered onto the board
  RenderMove -> [Model $ model & displayBoard .~ newBoard
                               & movesList .~ [],
                 Task $ return TurnSwitch]
      where
        newBoard = repaintPath (model ^. displayBoard) (sp, ep) -- generate the new board state by recolouring the two board positions

  -- the movement check, how a movement is validated is done as follows:
  -- first check the validity of the start position, then the end position, if no error is made then process, otherwise, discard that and print the error message 
  MoveCheck pos ci
    -- if win state is confirmed, or it is not the turn for human player then ignore the event
    | model ^. ifWin || ifComputersTurn pi -> []
    -- if entering the start position
    | ifInitialPiece sp -> case () of
                               -- check if the entered position is fitted for the current player
                            () | pi == ci -> [Model $ model & startPos .~ pos
                                                            & errorMessage .~ "Please perform a move"
                                                            & movesList .~ newMovesList]
                               -- if not, then discard this, and wait for another valid input
                               | pi /= ci -> [Model $ model & errorMessage .~ "Player " ++ show pi ++ ": invalid start"
                                                            & startPos .~ initialPos]
                               | otherwise -> []
    -- if entering the end position
    | not (ifInitialPiece sp) -> case () of
                                      -- if a position is clicked twice, and is an invalid movement
                                  ()  | sp == pos -> [Model $ model & errorMessage .~ "Player " ++ show pi ++ ": no effective move made"
                                                                    & startPos .~ initialPos]
                                      -- if the end position is occupied, then invalid
                                      | isOccupied pos -> [Model $ model & errorMessage .~ "Player " ++ show pi ++ ": destination occupied"
                                                                         & startPos .~ initialPos]
                                      -- if reachable, then this movement will then be rendered
                                      | pos `elem` availableMoves -> [Model $ model & endPos .~ pos
                                                                                    & errorMessage .~ "Please perform a move",
                                                                      Task $ return RenderMove]
                                      -- if the destination is not in the list, then invalid
                                      | pos `notElem` availableMoves -> [Model $ model & errorMessage .~ "Player " ++ show pi ++ ": destination unreacbable"
                                                                                       & startPos .~ initialPos]
                                      | otherwise -> []
    | otherwise -> []
    where
      newMovesList = evalState (destinationList pos) (model ^. displayBoard)

  -- reset the selected computer player's indices when the total players is changed
  CancelChecked v -> [Model $ model & playerConfigs . configList . ix 0 . active .~ head newActiveList
                                    & playerConfigs . configList . ix 1 . active .~ newActiveList !! 1
                                    & playerConfigs . configList . ix 2 . active .~ newActiveList !! 2
                                    & playerConfigs . configList . ix 3 . active .~ newActiveList !! 3
                                    & playerConfigs . configList . ix 4 . active .~ newActiveList !! 4
                                    & playerConfigs . configList . ix 5 . active .~ newActiveList !! 5
                                    & pageIndex .~ newPageId]
    where
      -- set the computer players with larger index inactive, and keep the smaller ones unchanged
      newActiveList = take v activeList ++ replicate (6 - v) False
      -- access the active state for each potential computer player
      activeList = map (\idx -> model ^?! playerConfigs . configList . ix idx . active) [0..5]
      -- modify the current panel page index
      newPageId = if v <= model ^. pageIndex then v - 1 else model ^. pageIndex

  -- called by other events to determine if needed to trigger AI decision function
  GenerateComputerAction
   | not (ifComputersTurn pi) || model ^. ifWin || not (model ^. startGame) -> [] -- ignore if already win or not the turn for computer players, or the game is ended

   | otherwise -> [ Model $ model & errorMessage .~ "Computer is now thinking ...",
                    Task $ RenderComputerAction <$> aiDecision model] -- call the decision function with necessary IO actions

  -- only react when the during the game that is currently played by the computer player
  RenderComputerAction (newBoard, newInternalState, newHistory)
    | not (model ^. startGame) -> [] -- quiting the game will avoid the decision function's result being rendered
    | otherwise -> [Model $ model & displayBoard .~ newBoard
                                  & internalStates .~ insertState
                                  & gameHistory .~ (if ifExistPublicMemory then replicate pn newHistory else replace pi newHistory ht)
                                  & playerIndex .~ newTurn
                                  & ifWin .~ newWinState
                                  & errorMessage .~ (if newWinState then "Congratulations"
                                                     else if not (ifComputersTurn newTurn) then "Please perform a move"
                                                          else model ^. errorMessage),
                   Task $ return GenerateComputerAction]
    where
      -- update the internal state resulted from the decision function, as well as check the win state
      insertState = replace pi newInternalState iboards
      newWinState = winStateDetect newInternalState
      newTurn = if not newWinState then turnBase (model ^. playersAmount) pi else pi
      ifExistPublicMemory = model ^. ifMemoryShared

  where
    -- boolean flag for checking whether the current turn is played by the computer player 
    ifComputersTurn id = model ^. playerConfigs ^?! configList . ix id . active
    pn = model ^. playersAmount
    sp = model ^. startPos
    ep = model ^. endPos
    pi = model ^. playerIndex
    iboards = model ^. internalStates
    eboard = model ^. displayBoard
    ht = model ^. gameHistory
    currentColour = playerColour pi pn -- retrieve the current turn's colour
    availableMoves = model ^. movesList

    -- pass the model' state to the MCTS framework and run it, and finally return the new state
    aiDecision :: AppModel -> IO (Board, [Pos], HistoryTrace)
    aiDecision model = do threadId <- myThreadId
                          appendThread threadId

                          gen <- newStdGen -- first get the random number generator for the later usage
                          let root = GRoot 0 [] -- initialise the search tree
                          (newBoard, newInternalState, newHistory)
                            <- finalSelection root (gen, pi, 1, eboard, iboards, pn, ht !! pi, (uctCons, phCons), (eval, dep, per))
                                                   (mctsControl mctsCon ctVal)
                          discardThread threadId
                          return (newBoard, newInternalState, newHistory)
      where
        -- access to detailed configuration for the computer player
        uctCons = model ^. playerConfigs ^?! configList . ix pi . uct
        phCons = model ^. playerConfigs ^?! configList . ix pi . ph
        eval = model ^. playerConfigs ^?! configList . ix pi . evaluator
        dep = model ^. playerConfigs ^?! configList . ix pi . depth
        per = model ^. playerConfigs ^?! configList . ix pi . percentage
        -- the control of the search progress
        mctsCon = model ^. playerConfigs ^?! configList . ix pi . control
        ctVal = model ^. playerConfigs ^?! configList . ix pi . cvalue

        mctsControl True x = (Just x, Nothing)
        mctsControl False x = (Nothing, Just (fromIntegral x))

-- load the configuration options as well as define the initial state of the application
main :: IO ()
main = do lookupTable `seq` startApp model handleEvent buildUI config
          endAllThread -- terminate all thread when closing the application
          existingThreads <- takeMVar threadPool
          if null existingThreads then putStrLn "All remaining threads were cleaned"
          else putStrLn "Unable to clean all generated threads"

  where
    config = [
      appWindowTitle "Fun Haskell - Chinese Checkers",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
      appWindowResizable False, -- disable resizing windows
      appWindowState $ MainWindowNormal (800, 750),
      appInitEvent (AppInit ())
      ]

    -- provide an initial model state
    model = AppModel {
      _playerIndex = 0,
      _ifWin = False,
      _displayBoard = externalBoard,
      _internalStates = [],
      _playersAmount = 2,
      _startGame = False,
      _startPos = initialPos,
      _endPos = initialPos,
      _errorMessage = "",
      _movesList = [],
      _gameHistory = [],
      _pageIndex = 0,
      _playerConfigs = ConfigList $ replicate 6 (ComputerPlayerConfig False 2.5 2.5 Move 2 100 True 10),
      _ifMemoryShared = False
    }
