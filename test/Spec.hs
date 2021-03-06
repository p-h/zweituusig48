import           Control.Monad                  ( forM_ )
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Lib.updateWest" $ it "correctly merges a row westwards" $ do
        updateWest [2, 0, 2, 2] `shouldBe` [3, 2, 0, 0]
        updateWest [2, 2, 0, 2] `shouldBe` [3, 2, 0, 0]
        updateWest [4, 4, 8, 16] `shouldBe` [5, 8, 16, 0]
        updateWest [8, 8, 16, 0] `shouldBe` [9, 16, 0, 0]
        updateWest [16, 16, 0, 0] `shouldBe` [17, 0, 0, 0]
    describe "Lib.updateGrid" $ do
        it "updates eastwards and returns the updated grid" $ forM_
            [ ( [ [1, 1, 1, 1] :: Row Int
                , [1, 1, 1, 1]
                , [1, 1, 1, 1]
                , [1, 1, 1, 1]
                ]
              , [[0, 0, 2, 2], [0, 0, 2, 2], [0, 0, 2, 2], [0, 0, 2, 2]]
              )
            , ( [[1, 2, 3, 4], [1, 1, 2, 2], [1, 2, 2, 1], [1, 1, 1, 1]]
              , [[1, 2, 3, 4], [0, 0, 2, 3], [0, 1, 3, 1], [0, 0, 2, 2]]
              )
            , ( [[1, 2, 3, 4], [1, 1, 2, 2], [1, 2, 2, 1], [1, 1, 1, 1]]
              , [[1, 2, 3, 4], [0, 0, 2, 3], [0, 1, 3, 1], [0, 0, 2, 2]]
              )
            , ( [[0, 0, 1, 0], [0, 0, 1, 0], [0, 0, 1, 0], [0, 0, 1, 0]]
              , [[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1]]
              )
            , ( [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
              , [[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 1]]
              )
            , ( [[0, 1, 2, 0], [1, 2, 0, 0], [1, 2, 3, 0], [1, 2, 3, 4]]
              , [[0, 0, 1, 2], [0, 0, 1, 2], [0, 1, 2, 3], [1, 2, 3, 4]]
              )
            ]
            (\(i, e) -> updateGrid East i `shouldBe` e)
        it "updates westwards and returns the updated grid"
            $ let
                  i =
                      [ [1, 2, 3, 4 :: Int]
                      , [1, 1, 2, 2]
                      , [1, 2, 2, 1]
                      , [1, 1, 1, 1]
                      ]
                  e = [[1, 2, 3, 4], [2, 3, 0, 0], [1, 3, 1, 0], [2, 2, 0, 0]]
              in
                  updateGrid West i `shouldBe` e
        it "updates southwards and returns the updated grid" $ forM_
            [ ( [[1, 2, 3, 4], [1, 1, 2, 2], [1, 2, 2, 1], [1, 1, 1, 1]]
              , [[0, 2, 0, 0], [0, 1, 3, 4], [2, 2, 3, 2], [2, 1, 1, 2]]
              )
            , ( [[1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1]]
              , [[0, 0, 0, 0], [0, 0, 0, 0], [2, 2, 2, 2], [2, 2, 2, 2]]
              )
            , ( [[0, 0, 0, 1], [0, 0, 0, 1], [0, 0, 0, 0], [0, 0, 1, 3]]
              , [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 2], [0, 0, 1, 3]]
              )
            ]
            (\(i, e) -> updateGrid South i `shouldBe` e)
        it "updates northwards and returns the updated grid" $ forM_
            [ ( [[1, 1, 1, 1 :: Int], [1, 1, 1, 1], [1, 1, 1, 1], [1, 1, 1, 1]]
              , [[2, 2, 2, 2], [2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0]]
              )
            , ( [[2, 2, 2, 2], [2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0]]
              , [[3, 3, 3, 3], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
              )
            , ( [[1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4], [1, 2, 3, 4]]
              , [[2, 3, 4, 5], [2, 3, 4, 5], [0, 0, 0, 0], [0, 0, 0, 0]]
              )
            , ( [[1, 2, 3, 4], [2, 1, 2, 3], [3, 2, 1, 2], [4, 3, 2, 1]]
              , [[1, 2, 3, 4], [2, 1, 2, 3], [3, 2, 1, 2], [4, 3, 2, 1]]
              )
            , ( [[1, 2, 3, 1], [1, 1, 1, 1], [1, 1, 1, 1], [1, 2, 2, 1]]
              , [[2, 2, 3, 2], [2, 2, 2, 2], [0, 2, 2, 0], [0, 0, 0, 0]]
              )
            , ( [[1, 2, 3, 4 :: Int], [1, 1, 2, 2], [1, 2, 2, 1], [1, 1, 1, 1]]
              , [[2, 2, 3, 4], [2, 1, 3, 2], [0, 2, 1, 2], [0, 1, 0, 0]]
              )
            , ( [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
              , [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
              )
            ]
            (\(i, e) -> updateGrid North i `shouldBe` e)
    describe "Lib.rotateGridRight" $ it "rotates a grid to the right" $ forM_
        [ ( [[1, 1, 1, 1], [2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0]]
          , [[0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 2, 1], [0, 0, 2, 1]]
          )
        , ( [[1, 2, 3, 4], [1, 1, 2, 2], [1, 2, 2, 1], [1, 1, 1, 1]]
          , [[1, 1, 1, 1], [1, 2, 1, 2], [1, 2, 2, 3], [1, 1, 2, 4]]
          )
        ]
        (\(i, e) -> rotateGridRight i `shouldBe` e)
    describe "Lib.rotateGridLeft" $ do
        it "rotates a grid to the left" $ forM_
            [ ( [[1, 1, 1, 1], [2, 2, 2, 2], [0, 0, 0, 0], [0, 0, 0, 0]]
              , [[1, 2, 0, 0], [1, 2, 0, 0], [1, 2, 0, 0], [1, 2, 0, 0]]
              )
            , ( [[1, 2, 3, 4], [1, 1, 2, 2], [1, 2, 2, 1], [1, 1, 1, 1]]
              , [[4, 2, 1, 1], [3, 2, 2, 1], [2, 1, 2, 1], [1, 1, 1, 1]]
              )
            ]
            (\(i, e) -> rotateGridLeft i `shouldBe` e)
        it "is the inverse of rotateGridRight"
            $ forAll (vectorOf 4 $ vector 4)
            $ \x -> (x :: Grid Int) == rotateGridLeft (rotateGridRight x)
    describe "Lib.reverseColumns'" $ do
        it "reverses the columns in a Grid"
            $          reverseColumns
                           [ [1, 2, 3, 4]
                           , [5, 6, 7, 8]
                           , [9, 10, 11, 12]
                           , [13, 14, 15, 16]
                           ]
            `shouldBe` [ [4, 3, 2, 1]
                       , [8, 7, 6, 5]
                       , [12, 11, 10, 9]
                       , [16, 15, 14, 13]
                       ]
        it "is its own inverse function" $ property $ \x ->
            (x :: Grid Int) == reverseColumns (reverseColumns x)
