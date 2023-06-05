{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad ( void )
import           Data.Text                      (Text, pack, unpack )
import qualified GI.GObject                    as GI
import qualified GI.Gtk.Objects.Widget as Gtk
import           GI.Gtk                         ( Button(..)
                                                , Grid(..)
                                                , Label(..)
                                                , Window(..)
                                                , Box (..), textBufferNew

                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid
import           GI.Gtk.Objects.Image as GtkImage
import Data.String (fromString)
import Lib
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.Vector (Vector)
import Parser
import GHC.Int (Int32)
import Control.Concurrent
import GI.Gtk.Declarative.EventSource (Subscription, fromCancellation)

data State =
  State { board :: Board, code :: String, gobstonesProgram :: GobstonesProgram }

data GobstonesProgram =
  NotLoaded { filePath :: Maybe String } |
  Ready { sourceCode :: String } |
  Running { boardStates :: [Board], executionResult :: ExecutionResult } |
  ParseFailed

data ExecutionResult = Success | Failure { errorMessage :: String }

data Event =
  Closed |
  FileSelectionChanged (Maybe FilePath) |
  RunProgram |
  GobstonesProgramLoaded String |
  RunStep |
  CodeChanged String |
  BackToNotLoaded |
  ResetBoard

view' :: State -> AppView Window Event
view' state =
  bin
      Window
      [ #title := "Gobstones"
      , on #deleteEvent (const (True, Closed))
      ]
    $ paned [#wideHandle := True]
            (pane defaultPaneProperties { resize = True, shrink = False} $ loadProgramWidget state)
            (pane defaultPaneProperties { resize = True, shrink = False} rightPane)
        where rightPane = case gobstonesProgram state of
                Running [] (Failure errorMessage) -> programFailed errorMessage (board state)
                _ -> boardGrid (board state)

programFailed :: String -> Board -> Widget event
programFailed errorMessage board =
  container Box [#orientation := Gtk.OrientationVertical]
          [ BoxChild defaultBoxChildProperties
            $ widget Label [#label := pack errorMessage, classes ["failure", "title"]],
            BoxChild defaultBoxChildProperties { expand = True, fill = True } $ boardGrid board
          ]

boardGrid :: Board -> Widget event
boardGrid board = toGrid (fromIntegral . Lib.height $ board)
                          [#columnHomogeneous := True, #rowHomogeneous := True]
                          (mapWithIndex (viewCell board) . cells $ board)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

isParseFailed state = case gobstonesProgram state of
  ParseFailed -> True
  _ -> False

loadProgramWidget :: State -> Widget Event
loadProgramWidget state = container Box [#orientation := Gtk.OrientationVertical]
          [ BoxChild defaultBoxChildProperties { padding = 10 } $ widget
              Gtk.FileChooserButton
              [ onM #selectionChanged
                    (fmap FileSelectionChanged . Gtk.fileChooserGetFilename)
              ]
          ,
            BoxChild defaultBoxChildProperties { expand = True, fill = True }
            $ textArea [#monospace := True, classes $ "sourceCode" : ["parseError" | isParseFailed state ]] (TextViewProperties (pack $ code state))
          ,  BoxChild defaultBoxChildProperties { padding = 10 } $ widget
            Button
            [ #label := "Correr"
            , #tooltipText := "Correr programa"
            , on #clicked RunProgram
            ]
          ,  BoxChild defaultBoxChildProperties { padding = 10 } $ widget
            Button
            [ #label := "Resetear"
            , #tooltipText := "Resetear tablero"
            , on #clicked ResetBoard
            ]
          ]

leftPanelText :: State -> Text
leftPanelText state = case gobstonesProgram state of
  NotLoaded filePath -> maybe "Cargá un programa de gobstones." pack filePath
  Ready sourceCode -> pack sourceCode
  Running _ _ -> ""
  ParseFailed -> ""

toGrid :: GHC.Int.Int32 -> Vector (Attribute Grid event) -> [Widget event] -> Widget event
toGrid columns gridProperties widgets =
  let (rows, lastRowColumns) = quotRem (fromIntegral . length $ widgets) columns
      positions  :: Vector (Int32, Int32)
      positions = do
        row <- [0..rows-1]
        column <- [0..columns-1]
        pure (row, column)
        <> do
        column <- [0..lastRowColumns-1]
        pure (rows, column)
    in
  container Grid gridProperties $
    (\(row, column) -> GridChild {
        properties = defaultGridChildProperties {
          leftAttach = column,
          topAttach = row
        }, child = reverse widgets !! fromIntegral (row * columns + column)
      }) <$> positions

viewCell :: Board -> Int -> Cell -> Widget event
viewCell board idx cell =
  toGrid 2 [classes $ "ballsCell" : ["currentCell" | currentCellIdx board == idx]] .
  map (\(name, amount) -> ballWithText name (show . amount $ cell)) .
  filter (\(_, amount) -> amount cell > 0) $ [
    ("black", black),
    ("red", red),
    ("green", green),
    ("blue", blue)
  ]

setFilePath :: Maybe String -> State -> State
setFilePath maybeFilePath state = state {
  gobstonesProgram = NotLoaded { filePath = maybeFilePath }
}

update' :: State -> Event -> Transition State Event
update' state Closed = Exit
update' state (FileSelectionChanged fileName) =
  Transition (setFilePath fileName state) $ do
    case fileName of
      Nothing -> pure Nothing
      Just filePath -> Just <$> do
        programSourceCode <- readFile filePath
        pure $ GobstonesProgramLoaded programSourceCode
update' state (GobstonesProgramLoaded programSourceCode) =
  Transition (state {
      gobstonesProgram = Ready programSourceCode
    }) (pure . Just . CodeChanged $ programSourceCode)
update' state RunProgram = let maybeProgram = parsearPrograma (code state)
                               (boardStates, finalResult) = case maybeProgram of
                                      Just program -> case program ((board state) { previous = Nothing }) of
                                        Left (errorMessage, failedBoard) ->
                                          (allBoardStates failedBoard, Failure errorMessage)
                                        Right newBoard ->
                                          (allBoardStates newBoard, Success)
                                      Nothing -> ([], Success)
                            in
  case maybeProgram of
    Just _ -> Transition (state { gobstonesProgram = Running boardStates finalResult }) (pure . Just $ RunStep)
    Nothing -> Transition (state { gobstonesProgram = ParseFailed}) (threadDelay 500000 >> pure (Just BackToNotLoaded))
update' state RunStep = Transition (case gobstonesProgram state of
                          Running (nextBoard : rest) finalResult -> state {
                            board = nextBoard,
                            gobstonesProgram = Running rest finalResult
                          }
                          _ -> state) (
                            case gobstonesProgram state of
                              Running (_:_) _ -> threadDelay 300000 >> pure (Just RunStep)
                              _ -> pure Nothing
                          )
update' state (CodeChanged newCode) =
  Transition (state { code = newCode }) (pure Nothing)
update' state BackToNotLoaded =
  Transition (state { gobstonesProgram = NotLoaded Nothing }) (pure Nothing)
update' state ResetBoard =
  Transition (state { board = initialBoard }) (pure Nothing)

-- update' state ButtonClicked = _wd
styles = mconcat [
  ".ballsCell {",
  "background-size: 100% 100%; ",
  "padding: 30px; ",
  "font-size: 30px; ",
  "border: 5px inset black; ",
  "background-color: white; ",
  "transition-property: background-color; ",
  "transition-duration: 0.2s; ",
  "}",
  "\n",
  ".currentCell {",
  "background-color: rgb(173,216,230); ",
  " }",
  "\n",
  ".failure { ",
  "color: red; ",
  "font-size: 30px; ",
  "}",
  "\n",
  ".sourceCode { ",
  "font-size: 30px; ",
  "}",
  "\n",
  "textview.view.parseError {",
  "padding-left: 0px; ",
  "animation-name: horizontal-shaking; ",
  "animation-duration: 0.5s;", 
  "animation-timing-function: linear;",
  "animation-iteration-count: 1;",
  "}",
  "\n",
  "@keyframes horizontal-shaking {",
  "\n",
  "0% { padding-left: 0px }","\n",
  "15% { padding-left: 30px }","\n",
  "30% { padding-left: 0px }","\n",
  "45% { padding-left: 20px }","\n",
  "60% { padding-left: 0px }","\n",
  "75% { padding-left: 10px }","\n",
  "100% { padding-left: 0px }","\n",
 "}","\n",
 "textview.view.parseError text {",
  "animation-name: horizontal-shaking-text; ",
  "animation-duration: 0.5s;", 
  "animation-timing-function: linear;",
  "animation-iteration-count: 1;",
  "}",
  "\n",
  "@keyframes horizontal-shaking-text {",
  "\n",
  "0% { color: black }","\n",
  "15% { color: red }","\n",
  "85% { color: red }","\n",
  "100% { color: black }","\n",
 "}","\n"
  ]

ballWithText :: String -> String -> Widget event
ballWithText color quantity = container Box [#homogeneous := True] [
    BoxChild defaultBoxChildProperties { expand = True, fill = True } $
      widget GtkImage.Image [
        #file := fromString ("assets/" <> color <> ".png")
      ],
    BoxChild defaultBoxChildProperties { expand = True, fill = True } $ widget Gtk.Label [
      #label := pack quantity
    ]
  ]

main :: IO ()
main = do
  void $ Gtk.init Nothing
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  cssProvider      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData cssProvider styles
  Gtk.styleContextAddProviderForScreen
    screen
    cssProvider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
  void $ run App { view         = view'
                      , update       = update'
                      , inputs       = []
                      , initialState = State {
                          code = "",
                          board = initialBoard,
                          gobstonesProgram = NotLoaded { filePath = Nothing }
                        }
                      }

initialBoard :: Board
initialBoard = newBoard 3

data TextViewProperties = TextViewProperties { content :: Text } deriving Eq

data TextViewState = TextViewState { textBuffer :: Gtk.TextBuffer }

textArea :: Vector (Attribute Gtk.TextView Event) -> TextViewProperties -> Widget Event
textArea customAttributes customParams = Widget
  (CustomWidget { customWidget
                , customCreate
                , customPatch
                , customSubscribe
                , customAttributes
                , customParams
                }
  )
 where
  -- The constructor for the underlying GTK widget.
  customWidget = Gtk.TextView
  -- A function that creates a widget (of the same type as
  -- customWidget), used on first render and on 'CustomReplace'. It's
  -- also returning our internal state, a reference to the spin button
  -- widget.
  customCreate :: TextViewProperties -> IO (Gtk.TextView, TextViewState)
  customCreate (TextViewProperties text) = do
    buffer <- Gtk.new Gtk.TextBuffer [#text Gtk.:= text]
    view <- Gtk.new Gtk.TextView [#buffer Gtk.:= buffer]

    return (view, TextViewState buffer)

  customPatch :: TextViewProperties -> TextViewProperties -> TextViewState -> CustomPatch Gtk.TextView TextViewState
  customPatch old new state
    | old == new = CustomKeep
    | otherwise = CustomModify $ \_textView -> do
                      let buffer = textBuffer state
                      cursorOffset <- Gtk.get buffer #cursorPosition
                      Gtk.setTextBufferText (textBuffer state) (content new)
                      iterator <- Gtk.textBufferGetStartIter buffer 
                      Gtk.textIterSetOffset iterator cursorOffset
                      Gtk.textBufferPlaceCursor buffer iterator

                      pure state

  customSubscribe :: TextViewProperties -> TextViewState -> Gtk.TextView -> (Event -> IO ()) -> IO Subscription
  customSubscribe _params state _box callback = do
    let buffer = textBuffer state
    handler <- Gtk.on buffer #changed $ do
      newText <- Gtk.getTextBufferText buffer
      case newText of
        Just text -> callback $ CodeChanged (unpack text)
        Nothing -> pure ()

    pure $ fromCancellation (GI.signalHandlerDisconnect buffer handler)

