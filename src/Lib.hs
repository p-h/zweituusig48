{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Lib
    ( someFunc
    , updateWest
    , updateGrid
    , Direction(..)
    , Grid
    , Row
    , rotateGridLeft
    , rotateGridRight
    , reverseColumns
    )
where

import           Control.Lens            hiding ( set )
import           Control.Monad                  ( forever )
import           Data.GI.Base
import           Data.List                      ( genericIndex )
import qualified Data.Text                     as T
import           Data.Word                      ( Word32 )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import qualified Reactive.Banana               as B
import qualified Reactive.Banana.Frameworks    as B
import qualified System.Random                 as R

data Direction = North
               | East
               | South
               | West
    deriving (Eq, Show, Read)

reverseColumns :: Grid a -> Grid a
reverseColumns = map reverse

type Row a = [a]

type Grid a = [Row a]

initialGrid :: Grid Int
initialGrid = [[0, 0, 0, 0], [2, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 0]]

gridLookup :: Integral i => Grid a -> i -> i -> a
gridLookup g x y = g `genericIndex` y `genericIndex` x

bindings =
    [ (Gdk.KEY_Up   , North)
    , (Gdk.KEY_W    , North)
    , (Gdk.KEY_w    , North)
    , (Gdk.KEY_Right, East)
    , (Gdk.KEY_D    , East)
    , (Gdk.KEY_d    , East)
    , (Gdk.KEY_Down , South)
    , (Gdk.KEY_S    , South)
    , (Gdk.KEY_s    , South)
    , (Gdk.KEY_Left , West)
    , (Gdk.KEY_A    , West)
    , (Gdk.KEY_a    , West)
    ]

keyCodeToDirection :: Word32 -> Maybe Direction
keyCodeToDirection keycode = lookup keycode bindings

someFunc :: IO ()
someFunc = do
    random <- R.getStdGen
    Gtk.init Nothing
    window <- new Gtk.ApplicationWindow
        [ #title := "Reactive banana and gtk test"
        , #windowPosition := Gtk.WindowPositionCenter
        ]
    on window #destroy Gtk.mainQuit
    labelGrid <- new Gtk.Grid
        [ #expand := True
        , #halign := Gtk.AlignFill
        , #valign := Gtk.AlignFill
        ]
    #add window labelGrid
    labels <- sequence
        [ let coord = (x, y)
              t     = T.pack $ show coord
          in  (,) coord <$> new Gtk.Label [#label := t, #expand := True]
        | x <- [0 .. 3]
        , y <- [0 .. 3]
        ]
    mapM_ (\((x, y), label) -> #attach labelGrid label x y 1 1) labels
    displayGrid initialGrid labels
    (keyEvent, fireKey) <- B.newAddHandler
    on window #keyPressEvent $ \eventKey -> do
        kv <- get eventKey #keyval
        fireKey kv
        pure True
    #showAll window
    let networkDescription :: B.MomentIO ()
        networkDescription = do
            eKey <- B.fromAddHandler keyEvent
            let eDir        = B.filterJust $ fmap keyCodeToDirection eKey
            let eGridChange = fmap updateGrid eDir
            let eGridChangeAndUpdate = (extendGrid random .) <$> eGridChange
            eGrid <- B.accumE initialGrid eGridChangeAndUpdate
            B.reactimate $ (`displayGrid` labels) <$> eGrid
    network <- B.compile networkDescription
    B.actuate network
    Gtk.main

extendGrid :: (Num a, Eq a) => R.StdGen -> Grid a -> Grid a
extendGrid r grid = modifyGrid grid x y n
  where
    (foo, r') = R.randomR (0, 9 :: Int) r
    n         = if foo == 0 then 2 else 1
    choices =
        [ (x, y) | x <- [0 .. 3], y <- [0 .. 3], gridLookup grid x y == 0 ]
    (i, _) = R.randomR (0, length choices - 1) r'
    (x, y) = choices !! i

displayGrid
    :: (Integral i, Integral a, Show a)
    => Grid a
    -> [((i, i), Gtk.Label)]
    -> IO ()
displayGrid grid = mapM_
    (\((x, y), label) ->
        let val    = 2 ^ gridLookup grid x y
            label' = if val == 1 then "" else T.pack $ show val
        in  set label [#label := label']
    )

updateGrid :: (Eq a, Num a) => Direction -> Grid a -> Grid a
updateGrid dir grid = case dir of
    West  -> map updateWest grid
    South -> rotateGridRight $ updateGrid East $ rotateGridLeft grid
    East  -> reverseColumns $ updateGrid West $ reverseColumns grid
    North -> reverse $ updateGrid South $ reverse grid

updateWest :: (Eq a, Num a) => Row a -> Row a
updateWest xs = take 4 $ ue (filter (/= 0) xs) ++ repeat 0
  where
    ue []  = []
    ue [a] = [a]
    ue (a : b : xs) | a == b    = (a + 1) : ue xs
                    | otherwise = a : ue (b : xs)

modifyGrid :: Grid a -> Int -> Int -> a -> Grid a
modifyGrid grid x y n = grid & (element y . element x) .~ n

rotateGridRight :: Grid a -> Grid a
rotateGridRight g =
    let ((a1:a2:a3:a4:_):(b1:b2:b3:b4:_):(c1:c2:c3:c4:_):(d1:d2:d3:d4:_):_) = g
    in  [[d1, c1, b1, a1], [d2, c2, b2, a2], [d3, c3, b3, a3], [d4, c4, b4, a4]]

rotateGridLeft :: Grid a -> Grid a
rotateGridLeft g =
    let ((a1:a2:a3:a4:_):(b1:b2:b3:b4:_):(c1:c2:c3:c4:_):(d1:d2:d3:d4:_):_) = g
    in  [[a4, b4, c4, d4], [a3, b3, c3, d3], [a2, b2, c2, d2], [a1, b1, c1, d1]]
