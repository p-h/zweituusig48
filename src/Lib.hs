{-# LANGUAGE OverloadedStrings, OverloadedLabels, MultiWayIf #-}

module Lib
    ( someFunc
    , updateGrid
    , Direction(..)
    , Grid
    , Row
    , rotateGridLeft
    , rotateGridRight
    , reverseColumns
    ) where

import Control.Monad (forever)
import Data.GI.Base
import Data.List (genericIndex)
import qualified Data.Text as T
import Data.Tuple.Select
import Data.Word (Word32)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B
import qualified System.Random as R

data Direction
    = North
    | East
    | South
    | West
    deriving (Eq, Show, Read)

reverseColumns :: Grid a -> Grid a
reverseColumns = map reverse

type Row a = [a]

type Grid a = [Row a]

initialGrid :: Grid Int
initialGrid = [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]

gridLookup :: Integral i => i -> i -> Grid a -> a
gridLookup x y g = g `genericIndex` y `genericIndex` x

bindings =
    [ (Gdk.KEY_Up, North)
    , (Gdk.KEY_W, North)
    , (Gdk.KEY_w, North)
    , (Gdk.KEY_Right, East)
    , (Gdk.KEY_D, East)
    , (Gdk.KEY_d, East)
    , (Gdk.KEY_Down, South)
    , (Gdk.KEY_S, South)
    , (Gdk.KEY_s, South)
    , (Gdk.KEY_Left, West)
    , (Gdk.KEY_A, West)
    , (Gdk.KEY_a, West)
    ]

keyCodeToDirection :: Word32 -> Maybe Direction
keyCodeToDirection keycode = lookup keycode bindings

someFunc :: IO ()
someFunc = do
    Gtk.init Nothing
    window <-
        new
            Gtk.ApplicationWindow
            [ #title := "Reactive banana and gtk test"
            , #windowPosition := Gtk.WindowPositionCenter
            ]
    on window #destroy Gtk.mainQuit
    labelGrid <-
        new
            Gtk.Grid
            [ #expand := True
            , #halign := Gtk.AlignFill
            , #valign := Gtk.AlignFill
            ]
    #add window labelGrid
    labels <-
        sequence
            [ let coord = (x, y)
                  t = T.pack $ show coord
               in (,) coord <$> new Gtk.Label [#label := t, #expand := True]
            | x <- [0 .. 3]
            , y <- [0 .. 3]
            ]
    mapM_ (\((x, y), label) -> #attach labelGrid label x y 1 1) labels
    (keyEvent, fireKey) <- B.newAddHandler
    on window #keyPressEvent $ \eventKey -> do
        kv <- get eventKey #keyval
        fireKey kv
        pure True
    #showAll window
    let networkDescription :: B.MomentIO ()
        networkDescription = do
            eKey <- B.fromAddHandler keyEvent
            let eDir = B.filterJust $ fmap keyCodeToDirection eKey
            let eGridChange = fmap updateGrid eDir
            eGrid <- B.accumE initialGrid eGridChange
            B.reactimate $ (`displayGrid` labels) <$> eGrid
    network <- B.compile networkDescription
    B.actuate network
    Gtk.main

displayGrid ::
       (Integral i, Num a, Show a) => Grid a -> [((i, i), Gtk.Label)] -> IO ()
displayGrid grid =
    mapM_
        (\((x, y), label) ->
             let val = T.pack $ show $ gridLookup x y grid
              in set label [#label := val])

updateGrid :: (Eq a, Num a) => Direction -> Grid a -> Grid a
updateGrid dir grid =
    case dir of
        East -> map updateEast grid
        South -> rotateGridRight $ updateGrid East $ rotateGridLeft grid
        West -> reverseColumns $ updateGrid East $ reverseColumns grid
        North -> reverse $ updateGrid South $ reverse grid
  where
    updateEast :: (Eq a, Num a) => Row a -> Row a
    updateEast (a:b:c:d:_) =
        if | a /= 0 && c /= 0 && a == b && c == d -> [0, 0, a + 1, c + 1]
           | c /= 0 && c == d -> [0, a, b, c + 1]
           | b /= 0 && b == c -> [0, a, b + 1, d]
           | a /= 0 && a == b -> [0, a + 1, c, d]
           | a == 0 && b == 0 && d == 0 -> [0, 0, 0, c]
           | a == 0 && c == 0 && d == 0 -> [0, 0, 0, b]
           | b == 0 && c == 0 && d == 0 -> [0, 0, 0, a]
           | a == 0 && b == 0 -> [0, 0, c, d]
           | a == 0 && c == 0 -> [0, 0, b, d]
           | a == 0 && d == 0 -> [0, 0, b, c]
           | b == 0 && c == 0 -> [0, 0, a, d]
           | b == 0 && d == 0 -> [0, 0, a, c]
           | c == 0 && d == 0 -> [0, 0, a, b]
           | b == 0 -> [0, a, c, d]
           | c == 0 -> [0, a, b, d]
           | d == 0 -> [0, a, b, c]
           | otherwise -> [a, b, c, d]
    --n <- (\r -> if r == 0 then 2 else 1) <$> R.randomRIO(0, 9)
    --let choices = [(x, y) | x <- [0..3], y <- [0..3], gridLookup x y == 0]
    --i <- R.randomRIO (0, length choices - 1)
    --let (x, y) = choices !! i
    --let newGrid = (modifyGrid x y) n grid
    --pure newGrid

--extendGrid :: Num a => Grid a -> IO (Grid a)
--extendGrid grid = do
rotateGridRight :: Grid a -> Grid a
rotateGridRight g =
    let ((a1:a2:a3:a4:_):(b1:b2:b3:b4:_):(c1:c2:c3:c4:_):(d1:d2:d3:d4:_):_) = g
     in [[d1, c1, b1, a1], [d2, c2, b2, a2], [d3, c3, b3, a3], [d4, c4, b4, a4]]

rotateGridLeft :: Grid a -> Grid a
rotateGridLeft g =
    let ((a1:a2:a3:a4:_):(b1:b2:b3:b4:_):(c1:c2:c3:c4:_):(d1:d2:d3:d4:_):_) = g
     in [[a4, b4, c4, d4], [a3, b3, c3, d3], [a2, b2, c2, d2], [a1, b1, c1, d1]]
