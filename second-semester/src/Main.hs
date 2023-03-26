{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Lens
    ( (&), (^?!), (^.), (+~), (.~), makeLenses, singular, Ixed(ix) )
import Data.Maybe ( fromMaybe )
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
      Colour(Black, Red, Blue, Green, Purple, Orange), colourIndex, initialPos, ifInitialPiece )
import Control.Monad.State
import qualified Data.Text as T
import qualified Monomer.Lens as L
import Monomer.Widgets
    ( nodeEnabled,
      nodeVisible,
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
      WidgetEnv,
      WidgetNode,
      EventResponse(Task, Model),
      CmbCheckboxMark(checkboxSquare) )
import Monomer.Main
    ( startApp,
      appFontDef,
      appInitEvent,
      appTheme,
      appWindowIcon,
      appWindowResizable,
      appWindowState,
      appWindowTitle,
      styleIf,
      AppEventResponse,
      MainWindowState(MainWindowNormal) )
import Monomer.Graphics.ColorTable
    ( black, blue, darkOrange, green, orange, purple, red, white )
import Monomer.Core.Combinators
    ( CmbAlignLeft(alignLeft),
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
      CmbTextColor(textColor),
      CmbTextFont(textFont),
      CmbTextRight(textRight),
      CmbTextSize(textSize),
      CmbWidth(width) )
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

-- the container for storing the parameters for a selected computer player
data ComputerPlayerConfig = ComputerPlayerConfig {
  _active :: Bool, -- whether a computer player is active at this position
  _uct :: Double,  -- the constant of UCT formula in MCTS selection 
  _ph :: Double,   -- the constant of Progressive History in MCTS selection
  _evaluator :: PlayoutEvaluator, -- the board evaluator used during the MCTS playouts
  _depth :: Int,   -- if the embedded minimax search is applied during the playouts, the related search depth is needed to be defined
  _control :: Int, -- the choice of how the MCTS is processed, including iteration counts, time limits and tree expansions
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
  -- the history record of the game, which could be made use of by the MCTS
  _gameHistory :: HistoryTrace,
  -- the history record of moves that cause a alpha-beta pruning, which could be made use of by the minimax search
  _killerMoves :: [KillerMoves],
  -- indicates the current player to be configured at the setting dialog
  _pageIndex :: Int,
  -- a list of potential computer players' configurations
  _playerConfigs :: ConfigList
} deriving (Eq, Show)

-- the event that the model triggers and handles as well as the responses
data AppEvent
  = -- the initialisation of the model status
    AppInit 
    -- determine if a movement input by the player is valid
  | MoveCheck BoardPos Int 
    -- initialise the board game based on user's input
  | StartGameButtonClick 
    -- if the movement passes the validity check, then display the change onto the board
  | RenderMove  
    -- allow the user to cancel the his move before the "Confirm" button is clicked
  | CancelMove  
    -- quit the current game and navigate back to the menu
  | EndGameButtonClick 
    -- modify the checkbox's results when the total players is changed
  | CancelChecked Int 
    -- update the page index for the setting panel
  | PageUpdate Int 
    -- display the move made by the computer player onto the board
  | RenderComputerAction (Board, [Pos], HistoryTrace, [KillerMoves]) 
    -- generate the corresponding movement based on the current game state and pass the result to the "RenderComputerAction" event
  | GenerateComputerAction 
    -- jump to the next player's turn, as well as check the next player's state
  | TurnSwitch 
  deriving (Eq, Show)

makeLenses 'ComputerPlayerConfig
makeLenses 'ConfigList
makeLenses 'AppModel

-- produce the title text 
titleText :: AppModel -> String
titleText model
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
    
-- construct the graphical layout of the application
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

  -- render the colour and index of a piece for certain player on the board
  pieceButton :: BoardPos -> WidgetNode AppModel AppEvent
  pieceButton pos
    -- place a spacer if the position type is "U", which is just for separating the buttons
    | isSpacer pos = spacer
    -- otherwise, place a button with related color and index
    | otherwise = button (T.pack textBi) (MoveCheck pos bi)
      `styleBasic`[radius 45, bgColor (pieceColour pos), border 2 white, textSize 20, textColor white,
                   -- besides, once a button is clicked, the available destinations will be rendered with corresponding colour 
                   styleIf (pos `elem` model ^. movesList) (bgColor (pieceColour sp)),
                   -- and the clicked button's border will be repainted as well
                   styleIf (pos == sp) (border 2 (pieceColour sp))
                  ]
    where
      -- get the player index based on the clicked button's colour, could be used to determine if the clicked button fit the current turn
      -- the ones has no colour or is not in the current colour list due to the number of the players are assigned with (-1)
      bi = case getColour pos of
            Nothing -> (-1) 
            Just c  -> fromMaybe (-1) $ colourIndex c (model ^. playersAmount)
      -- the empty button shows no colour, therefore, will display nothing, while others will show their indices
      textBi = if bi == (-1) then "" else show bi

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

  -- display a row of buttons for colouring and indexing them
  rowButton :: [BoardPos] -> WidgetNode AppModel AppEvent
  rowButton ps = hgrid (pieceButton <$> ps) -- order in a grid style

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

  -- the layer that integrates the options of player's settings
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
              labeledRadio_ "Embedded Paranoid" MixedParanoid eitem [textRight],
              spacer,
              hgrid_ [childSpacing_ 5] [
                label "(depth)",
                labeledRadio_ "2" 2 ditem [textRight],
                labeledRadio_ "3" 3 ditem [textRight],
                labeledRadio_ "4" 4 ditem [textRight]
              ] `nodeVisible` (vitem ^. evaluator == MixedParanoid)
            ],

            hstack [
              labeledRadio_ "Embedded BRS" MixedBRS eitem [textRight],
              spacer,
              hgrid_ [childSpacing_ 5] [
                label "(depth)",
                labeledRadio_ "2" 2 ditem [textRight],
                labeledRadio_ "3" 3 ditem [textRight],
                labeledRadio_ "4" 4 ditem [textRight]
              ] `nodeVisible` (vitem ^. evaluator == MixedBRS)
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

  -- the layer that
  selectButtonLayer =
      -- the button for switching the panel's page
      hstack [
        button "Last" (PageUpdate (-1)) `nodeEnabled` (pi > 0),
        filler,
        label (T.pack ( playerType ++ show pi))
        `styleBasic` [textSize 30],
        filler,
        button "Next" (PageUpdate 1) `nodeEnabled` (pageIdx < model ^. playersAmount - 1)
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
              button "Confirm" TurnSwitch,
              filler,
              button "Cancel" CancelMove,
              filler
          ]`styleBasic`[textSize 20],
          spacer,
          -- render the board's positions row by row
          vgrid_ [childSpacing_ 5] (rowButton <$> (model ^. displayBoard))
        ] `nodeVisible` (model ^. startGame),

        -- the button for starting the game
        spacer `nodeVisible` not (model ^. startGame),
        box $ button "Start Game" StartGameButtonClick `styleBasic`[textSize 30] `nodeVisible` not (model ^. startGame),
        filler
      ] `styleBasic` [padding 20]

ifComputersTurn :: AppModel -> Bool
ifComputersTurn model = model ^. playerConfigs ^?! configList . ix (model ^. playerIndex) . active

-- declare how the events are handled respectively
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [] -- [Producer computerTurn] -- an additional thread for checking computer player's turn
  -- setup the game board according to the options in the menu page: 
  -- declare that the game is started letting certain subjects to be visiable or invisiable
  -- generate the game board based on the chosen players number, and the corresponding length of hash state
  StartGameButtonClick -> [Model $ model & startGame .~ True
                                         & displayBoard .~ eraseBoard (playerColourList pn) externalBoard
                                         & internalStates .~ replicate pn startBase
                                         & killerMoves .~ replicate pn []
                                         & movesList .~ []
                                         & gameHistory .~ RBLeaf
                                         & startPos .~ initialPos
                                         & endPos .~ initialPos
                                         & playerIndex .~ 0 -- resetting all states
                                         & ifWin .~ False,
                          Task $ return GenerateComputerAction
                          ]
    where
      pn = model ^. playersAmount

  -- quit the game and return back to the menu page
  EndGameButtonClick -> [Model $ model & startGame .~ False
                                       & errorMessage .~ ""
                                       ]

  -- update the page index with given increment
  PageUpdate x -> [Model $ model & pageIndex +~ x]

  -- the event of cancel the last piece change made, and revert it to the previous board state   
  -- movement cancel is not allowed to be made over a player, meaning that once another player makes the move, you can no longer cancel yours                             
  CancelMove
    | model ^. ifWin || ifComputersTurn model -> [] -- ignore if the win state is confirmed
    | not (ifInitialPiece sp) && not (ifInitialPiece ep) -> [Model $ model & displayBoard .~ newBoard -- reset the board, hash, and turn states
                                                                           -- & internalStates .~ insertState
                                                                           --  & previousStartPos .~ initialPos
                                                                           --  & previousEndPos .~ initialPos
                                                                           -- & playerIndex .~ lastTurn
                                                                           & startPos .~ initialPos
                                                                           & endPos .~ initialPos
                                                                           & errorMessage .~ ""]
    | otherwise -> [Model $ model & errorMessage .~ "the comfirmed move cannot be cancelled"] -- ignore if not record is stored for that
    where

      -- ifExistComputerPlayer = all getActive [0..5]
      -- getActive idx = model ^. playerConfigs ^?! configList . ix idx . active

      sp = model ^. startPos
      ep = model ^. endPos
      pn = model ^. playersAmount
      currentColour = playerColour (model ^. playerIndex) pn

      newBoard = repaintPath (model ^. displayBoard) (repaint currentColour ep, erase sp)
      -- revert the board state

      -- get the last turn value and retrieve the information for reverting the hash state
      -- lastTurn = turnRevert pn (model ^. playerIndex)
      -- lastColour = playerColour lastTurn pn
      -- ips = projection lastColour (getPos ps)
      -- ipe = projection lastColour (getPos pe)

      -- lastTurnState = (model ^. internalStates) !! lastTurn
      -- newState = flipBoard lastTurnState (ipe, ips)
      -- -- revert the internal positions and then replace it in the state list
      -- insertState = replace lastTurn newState (model ^. internalStates)

  -- update the turn when a movement for a player is complete
  TurnSwitch
    | model ^. ifWin || ifComputersTurn model -> []
    | otherwise -> [Model $ model & playerIndex .~ newTurn
                                  & ifWin .~ newWinState
                                  & startPos .~ initialPos
                                  & endPos .~ initialPos
                                  & internalStates .~ insertState
                                  & errorMessage .~ "",
                    Task $ return GenerateComputerAction]
    where
      sp = model ^. startPos
      ep = model ^. endPos
      pi = model ^. playerIndex
      pn = model ^. playersAmount

      currentColour = playerColour pi pn
      currentState = (model ^. internalStates) !! pi
      isp = projection currentColour (getPos sp)
      iep = projection currentColour (getPos ep)

      newState = flipBoard currentState (isp, iep)
      newWinState = winStateDetect newState

      insertState = replace pi newState (model ^. internalStates)
      newTurn = if not newWinState then turnBase pn pi else pi

  -- after a movement is determined valid, it will be rendered
  RenderMove -> [Model $ model & displayBoard .~ newBoard
                               & movesList .~ []
                               ]
      where
        sp = model ^. startPos
        ep = model ^. endPos
        pi = model ^. playerIndex
        newBoard = repaintPath (model ^. displayBoard) (sp, ep) -- generate the new board state by re-colouring the two board positions

  -- the movement check, how a movement is passed is done as follows:
  -- first enter the starting point, and check for the correctness
  -- then enter the destination, if no error is made then process, otherwise, discard that and print the error message 
  MoveCheck pos pi ->
              if model ^. ifWin || ifComputersTurn model || (not (ifInitialPiece sp) && not (ifInitialPiece ep)) then [] -- if already won or it's computer's turn, then do nothing
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
                                                                        & errorMessage .~ "Please press comfirm to complete the move",
                                                          Task $ return RenderMove] -- if reachable, then this movement will then be rendered
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
      -- ifComputersTurn = model ^. playerConfigs ^?! configList . ix turn . active
      newMovesList = evalState (destinationList pos) (model ^. displayBoard)

  -- reset the selected computer player's indices when the total players is changed
  CancelChecked v -> [Model $ model & playerConfigs . configList . ix 0 . active .~ head newList
                                    & playerConfigs . configList . ix 1 . active .~ newList !! 1
                                    & playerConfigs . configList . ix 2 . active .~ newList !! 2
                                    & playerConfigs . configList . ix 3 . active .~ newList !! 3
                                    & playerConfigs . configList . ix 4 . active .~ newList !! 4
                                    & playerConfigs . configList . ix 5 . active .~ newList !! 5
                                    & pageIndex .~ newPageId]
    where
      newList = take v remainedList ++ replicate (6 - v) False
      remainedList = map modelList [0..5]
      modelList x = model ^?! playerConfigs . configList . ix x . active
      newPageId = if v <= model ^. pageIndex then v - 1 else model ^. pageIndex

  GenerateComputerAction
   | not (ifComputersTurn model) || model ^. ifWin -> []
   | otherwise -> [Task $ RenderComputerAction <$> aiDecision model]
   where
    pi = model ^. playerIndex
    -- ifComputersTurn = model ^. playerConfigs ^?! configList . ix pi . active

  -- only react when the during the game and is currently the computer player's turn
  RenderComputerAction (nboard, nps, nht, nkms) -> [Model $ model & displayBoard .~ nboard
                                                            & internalStates .~ insertState
                                                            & gameHistory .~ nht
                                                            & killerMoves .~ nkms
                                                            & playerIndex .~ newTurn
                                                            & ifWin .~ newWinState,
                                              Task $ return GenerateComputerAction]
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
      insertState = replace pi nps (model ^. internalStates)
      newWinState = winStateDetect nps
      newTurn = if not newWinState then turnBase (model ^. playersAmount) pi else pi

-- keep calling the event for checking the computer player's movement
-- computerTurn :: (AppEvent -> IO a) -> IO b
-- computerTurn sendMsg = do sendMsg ComputerAction
--                           threadDelay $ 1000 * 500 -- check every 1 second
--                           computerTurn sendMsg

-- pass the model information to the MCTS interface and accept the returned board state
aiDecision :: AppModel -> IO (Board, [Pos], HistoryTrace, [KillerMoves])
aiDecision model = do gen <- newStdGen
                      let root = GRoot 0 []
                      (newBoard, newState, nht, _, nkms) <- finalSelection root
                                                            (gen, pi, 1, eboard, iboard, pn, ht, (uctCons, phCons), (eval, dep, kms))
                                                            (mctsControl mctsCon ctVal)
                      return (newBoard, newState, nht, nkms)
  where
    pi = model ^. playerIndex
    eboard = model ^. displayBoard
    iboard = model ^. internalStates
    pn = model ^. playersAmount
    ht = model ^. gameHistory
    kms = model ^. killerMoves

    uctCons = model ^. playerConfigs ^?! configList . ix pi . uct
    phCons = model ^. playerConfigs ^?! configList . ix pi . ph
    eval = model ^. playerConfigs ^?! configList . ix pi . evaluator
    dep = model ^. playerConfigs ^?! configList . ix pi . depth

    mctsCon = model ^. playerConfigs ^?! configList . ix pi . control
    ctVal = model ^. playerConfigs ^?! configList . ix pi . cvalue

    mctsControl 0 x = (Just x, Nothing, Nothing)
    mctsControl 1 x = (Nothing, Just x, Nothing)
    mctsControl _ x = (Nothing, Nothing, Just (fromIntegral x))

-- load the configuration options as well as define the initial state of the application
main :: IO ()
main = do lookupTable `seq` startApp model handleEvent buildUI config
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
      _errorMessage = "",
      _movesList = [],
      _gameHistory = RBLeaf,
      _killerMoves = [],
      _pageIndex = 0,
      _playerConfigs = ConfigList $ replicate 6 (ComputerPlayerConfig False 3 1 MoveEvaluator 0 0 10)
    }