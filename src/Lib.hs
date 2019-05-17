{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Lib ( someFunc
           , updateGrid
           , Direction (..)
           , rotateGridLeft
           , rotateGridRight
           , Row
           , Grid
           , reverse'
           , reverseColumns
           ) where

import Data.GI.Base
import Data.List(genericIndex)
import Data.Tuple.Select
import Data.Word(Word32)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified Reactive.Banana as B
import qualified Reactive.Banana.Frameworks as B

data Direction = North | East | South | West
    deriving (Eq, Show, Read)

reverse' :: (a, b, c, d) -> (d, c, b, a)
reverse' (e1, e2, e3, e4) = (e4, e3, e2, e1)

reverseColumns :: Grid a -> Grid a
reverseColumns (r1, r2, r3, r4) = (reverse' r1, reverse' r2, reverse' r3, reverse' r4)

type Row a = (a, a, a, a)
type Grid a = (Row a, Row a, Row a, Row a)

initialGrid :: Grid Int
initialGrid = ((1, 2, 3, 4), (1, 2, 3, 4), (1, 2, 3, 4), (1, 2, 3, 4))

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
    window <- new Gtk.ApplicationWindow [ #title := "Reactive banana and gtk test"
                                        , #windowPosition := Gtk.WindowPositionCenter ]
    on window #destroy Gtk.mainQuit

    labelGrid <- new Gtk.Grid [ #expand := True
                              , #halign := Gtk.AlignFill
                              , #valign := Gtk.AlignFill ]
    #add window labelGrid

    labels <- sequence
        [
            let coord = (x, y)
                t = T.pack $ show coord
            in (,) coord <$> new Gtk.Label [ #label := t, #expand := True ]
        | x <- [0..3], y <- [0..3]
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

displayGrid :: (Integral i, Num a, Show a) => Grid a -> [((i, i), Gtk.Label)] -> IO ()
displayGrid grid = mapM_ (\((x, y), label) ->
    let val = T.pack $ show $ gridLookup x y grid
    in set label [#label := val])
    where
        selectors :: [Row a -> a]
        selectors = [sel1, sel2, sel3, sel4]
        gridLookup :: Integral i => i -> i -> Grid a -> a
        gridLookup x y g = (selectors `genericIndex` x) $ (selectors `genericIndex` y) g


updateGrid :: (Eq a, Num a) => Direction -> Grid a -> Grid a
updateGrid dir grid@(r1, r2, r3, r4) = case dir of
    East -> (updateEast r1, updateEast r2, updateEast r3, updateEast r4)
    South -> rotateGridRight $ updateGrid East $ rotateGridLeft grid
    West -> reverseColumns $ updateGrid East $ reverseColumns grid
    North -> reverse' $ updateGrid South $ reverse' grid
    where
        updateEast :: (Eq a, Num a) => Row a -> Row a
        updateEast row@(a, b, c, d)
            | a /= 0 && c /= 0 && a == b && c == d = (0, 0, a + 1, c + 1)
            | c /= 0 && c == d = (0, a, b, c + 1)
            | b /= 0 && b == c = (0, a, b + 1, d)
            | a /= 0 && a == b = (0, a + 1, c, d)
            | a == 0 && b == 0 && d == 0 = (0, 0, 0, c)
            | a == 0 && c == 0 && d == 0 = (0, 0, 0, b)
            | b == 0 && c == 0 && d == 0 = (0, 0, 0, a)
            | a == 0 && b == 0 = (0, 0, c, d)
            | a == 0 && c == 0 = (0, 0, b, d)
            | a == 0 && d == 0 = (0, 0, b, c)
            | b == 0 && c == 0 = (0, 0, a, d)
            | b == 0 && d == 0 = (0, 0, a, c)
            | c == 0 && d == 0 = (0, 0, a, b)
            | b == 0 = (0, a, c, d)
            | c == 0 = (0, a, b, d)
            | d == 0 = (0, a, b, c)
            | otherwise = row

rotateGridRight :: Grid a -> Grid a
rotateGridRight ((a1, a2, a3, a4), (b1, b2, b3, b4), (c1, c2, c3, c4), (d1, d2, d3, d4)) =
    ((d1, c1, b1, a1)
    ,(d2, c2, b2, a2)
    ,(d3, c3, b3, a3)
    ,(d4, c4, b4, a4)
    )

rotateGridLeft :: Grid a -> Grid a
rotateGridLeft ((a1, a2, a3, a4), (b1, b2, b3, b4), (c1, c2, c3, c4), (d1, d2, d3, d4)) =
    ((a4, b4, c4, d4)
    ,(a3, b3, c3, d3)
    ,(a2, b2, c2, d2)
    ,(a1, b1, c1, d1)
    )
