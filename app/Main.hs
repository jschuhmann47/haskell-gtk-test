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

fileNameLabel :: State -> Widget event
fileNameLabel state = case gobstonesProgram state of
  NotLoaded filePath -> widget
                Label
                [#label := maybe "Cargá un programa de gobstones." pack filePath]
  Ready sourceCode -> widget Label [#label := pack sourceCode]

data State =
  State { board :: Board, gobstonesProgram :: GobstonesProgram }

data GobstonesProgram =
  NotLoaded { filePath :: Maybe String } |
  Ready { sourceCode :: String }

data Event =
  Closed |
  FileSelectionChanged (Maybe FilePath) |
  RunProgram |
  GobstonesProgramLoaded String

view' :: State -> AppView Window Event
view' state =
  bin
      Window
      [ #title := "Gobstones"
      , on #deleteEvent (const (True, Closed))
      ]
    $ paned [#wideHandle := True]
            (pane defaultPaneProperties { resize = True, shrink = False} $ loadProgramWidget state)
            (pane defaultPaneProperties { resize = False, shrink = False} boardGrid)
        where boardGrid = toGrid (fromIntegral . Lib.height . board $ state)
                                            [#rowSpacing := 75, #columnSpacing := 75, classes ["window"]]
                                            (map ballsCount . cells . board $ state)

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
toGrid columns gridProperties widgets = let rows = div (fromIntegral . length $ widgets) columns in
  container Grid gridProperties    
    (
        Vector.fromList [GridChild {
        properties = defaultGridChildProperties {
          leftAttach = column,
          topAttach = row
        }, child = reverse widgets !! fromIntegral (row * columns + column)
      } | column <- [0..columns-1], row <- [0..rows-1]]
    )

ballsCount :: Cell -> Widget event
ballsCount cell =
  toGrid 2 [] . map (\(name, amount) -> ballWithText name (show . amount $ cell)) $ [
    ("black", black),
    ("blue", blue),
    ("red", red),
    ("green", green)
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
update' state RunProgram =
  Transition (state { board = resultingBoard }) (pure Nothing)
  where maybeProgram = case gobstonesProgram state of
          NotLoaded _ -> Nothing
          Ready sourceCode -> parsearPrograma sourceCode
        resultingBoard = case maybeProgram of
          Just program -> case program (board state) of
            Left _ -> board state
            Right newBoard -> newBoard
          Nothing -> board state

-- update' state ButtonClicked = _wd

styles = mconcat [
  ".window {",
  "background-size: 145px 140px; ",
  "background-image: url('assets/cell.png');",
  "}",
  "\n",
  ".ball {",
  "background-image: url('assets/red.png'); ",
  "background-size: 32px; ",
  " }"
  ]

ballWithText :: String -> String -> Widget event
ballWithText color quantity = container Box [] [
    BoxChild defaultBoxChildProperties $
      widget GtkImage.Image [
        #file := fromString ("assets/" <> color <> ".png")
      ],
    BoxChild defaultBoxChildProperties $ widget Gtk.Label [
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