{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hello where

import           Control.Monad                  ( void )
import           Data.Text                      ( pack )
import           GI.Gtk                         ( Button(..)
                                                , Grid(..)
                                                , Label(..)
                                                , Window(..)
                                                , Overlay(..), ListBox (..), Box (..)

                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Grid
import qualified Data.Vector as Vector
import           GI.Gtk.Objects.Image as GtkImage
import GHC.IO.Unsafe (unsafePerformIO)
import qualified GHC.Int
import Data.String (fromString)
import Lib
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import GHC.Exts (Item)
import Data.Vector (Vector)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Parser
import qualified GI.Gtk.Declarative.Container.Grid as Grid
import Debug.Trace
import GHC.Int (Int32)
import Control.Monad (forM_)
import qualified Pipes.Extras as Pipes
import Pipes ((>->))
import Control.Concurrent

fileNameLabel :: State -> Widget event
fileNameLabel state = case gobstonesProgram state of
  NotLoaded filePath -> widget
                Label
                [#label := maybe "Cargá un programa de gobstones." pack filePath]
  Ready sourceCode -> widget Label [#label := pack sourceCode]
  Running _ -> widget Label []

data State =
  State { board :: Board, gobstonesProgram :: GobstonesProgram }

data GobstonesProgram =
  NotLoaded { filePath :: Maybe String } |
  Ready { sourceCode :: String } |
  Running { boardStates :: [Board] }

data Event =
  Closed |
  FileSelectionChanged (Maybe FilePath) |
  RunProgram |
  GobstonesProgramLoaded String |
  RunStep

view' :: State -> AppView Window Event
view' state =
  bin
      Window
      [ #title := "Gobstones"
      , on #deleteEvent (const (True, Closed))
      ]
    $ paned [#wideHandle := True]
            (pane defaultPaneProperties { resize = True, shrink = False} $ loadProgramWidget state)
            (pane defaultPaneProperties { resize = True, shrink = False} boardGrid)
        where boardGrid = toGrid (fromIntegral . Lib.height . board $ state)
                                            [#columnHomogeneous := True, #rowHomogeneous := True]
                                            (mapWithIndex (viewCell $ board state) . cells . board $ state)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = zipWith f [0..]

loadProgramWidget :: State -> Widget Event
loadProgramWidget state = container Box [#orientation := Gtk.OrientationVertical]
          [ BoxChild defaultBoxChildProperties { expand = True, fill = True }
            $ fileNameLabel state
          , BoxChild defaultBoxChildProperties { padding = 10 } $ widget
              Gtk.FileChooserButton
              [ onM #selectionChanged
                    (fmap FileSelectionChanged . Gtk.fileChooserGetFilename)
              ]
          , BoxChild defaultBoxChildProperties { padding = 10 } $ widget
            Button
            [ #label := "Correr"
            , #tooltipText := "Correr programa"
            , on #clicked RunProgram
            ]
          ]

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
  toGrid 2 [classes $ "ballsCell" : ["currentCell" | currentCell board == idx]] .
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
  Transition (state { gobstonesProgram = Ready programSourceCode })
             (pure Nothing)
update' state RunProgram = let maybeProgram = case gobstonesProgram state of
                                      NotLoaded _ -> Nothing
                                      Ready sourceCode -> parsearPrograma sourceCode
                               boardStates = case maybeProgram of
                                      Just program -> case program (board state) of
                                        Left _ -> []
                                        Right newBoard -> allBoardStates newBoard
                                      Nothing -> []
                            in
  Transition (state { gobstonesProgram = Running boardStates }) (pure . Just $ RunStep)
update' state RunStep = Transition (case gobstonesProgram state of
                          Running (nextBoard : rest) -> state {
                            board = nextBoard,
                            gobstonesProgram = Running rest
                          }
                          _ -> state) (
                            case gobstonesProgram state of
                              Running (_:_) -> threadDelay 1000000 >> pure (Just RunStep)
                              _ -> pure Nothing
                          )


-- update' state ButtonClicked = _wd
styles = mconcat [
  ".ballsCell {",
  "background-size: 100% 100%; ",
  "padding: 30px; ",
  "font-size: 30px; ",
  "border: 5px inset black; ",
  "background-color: white; ",
  "}",
  "\n",
  ".currentCell {",
  "background-color: rgb(173,216,230); ",
  " }"
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
                          board = newBoard 3,
                          gobstonesProgram = NotLoaded { filePath = Nothing }
                        }
                      }