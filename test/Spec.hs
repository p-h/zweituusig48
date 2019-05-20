import Lib
import Test.Hspec
import Test.QuickCheck
import Control.Monad(forM_)

instance Arbitrary a => Arbitrary (Row a) where
    arbitrary = do
        r <- arbitrary
        pure $ Row r

instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = do
        g <- arbitrary
        pure $ Grid g

main :: IO ()
main = hspec $ do
    describe "Lib.updateGrid" $ do
        it "updates eastwards and returns the updated grid" $ do
            forM_ 
                [
                    (((1, 1, 1, 1) :: (Int, Int, Int, Int)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1))
                    ,((0, 0, 2, 2)
                    ,(0, 0, 2, 2)
                    ,(0, 0, 2, 2)
                    ,(0, 0, 2, 2))
                    ),
                    (((1, 2, 3, 4)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                    ,((1, 2, 3, 4)
                    ,(0, 0, 2, 3)
                    ,(0, 1, 3, 1)
                    ,(0, 0, 2, 2))
                    ),
                    (((1, 2, 3, 4)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                    ,((1, 2, 3, 4)
                    ,(0, 0, 2, 3)
                    ,(0, 1, 3, 1)
                    ,(0, 0, 2, 2))
                    ),
                    (((0, 0, 1, 0)
                    ,(0, 0, 1, 0)
                    ,(0, 0, 1, 0)
                    ,(0, 0, 1, 0))
                    ,((0, 0, 0, 1)
                    ,(0, 0, 0, 1)
                    ,(0, 0, 0, 1)
                    ,(0, 0, 0, 1))
                    ),
                    (((1, 0, 0, 0)
                    ,(0, 1, 0, 0)
                    ,(0, 0, 1, 0)
                    ,(0, 0, 0, 1))
                    ,((0, 0, 0, 1)
                    ,(0, 0, 0, 1)
                    ,(0, 0, 0, 1)
                    ,(0, 0, 0, 1))
                    ),
                    (((0, 1, 2, 0)
                    ,(1, 2, 0, 0)
                    ,(1, 2, 3, 0)
                    ,(1, 2, 3, 4))
                    ,((0, 0, 1, 2)
                    ,(0, 0, 1, 2)
                    ,(0, 1, 2, 3)
                    ,(1, 2, 3, 4))
                    )
                ]
                (\(i, e) -> (updateGrid East (grid i)) `shouldBe` (grid e))

        it "updates westwards and returns the updated grid" $ do
            let i = ((1, 2, 3, 4 :: Int)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                e = ((1, 2, 3, 4)
                    ,(2, 3, 0, 0)
                    ,(1, 3, 1, 0)
                    ,(2, 2, 0, 0))
                    in (updateGrid West (grid i)) `shouldBe` (grid e)

        it "updates southwards and returns the updated grid" $ do
            forM_ 
                [
                    (((1, 2, 3, 4)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                    ,((0, 2, 0, 0)
                    ,(0, 1, 3, 4)
                    ,(2, 2, 3, 2)
                    ,(2, 1, 1, 2))
                    ),
                    (((1, 1, 1, 1)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1))
                    ,((0, 0, 0, 0)
                    ,(0, 0, 0, 0)
                    ,(2, 2, 2, 2)
                    ,(2, 2, 2, 2)))
                ]
                (\(i, e) -> (updateGrid South (grid i)) `shouldBe` (grid e))

        it "updates northwards and returns the updated grid" $ do
            forM_ 
                [
                    (((1, 1, 1, 1 :: Int)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1))
                    ,((2, 2, 2, 2)
                    ,(2, 2, 2, 2)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ),
                    (((2, 2, 2, 2)
                    ,(2, 2, 2, 2)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ,((3, 3, 3, 3)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ),
                    (((1, 2, 3, 4)
                    ,(1, 2, 3, 4)
                    ,(1, 2, 3, 4)
                    ,(1, 2, 3, 4))
                    ,((2, 3, 4, 5)
                    ,(2, 3, 4, 5)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ),
                    (((1, 2, 3, 4)
                    ,(2, 1, 2, 3)
                    ,(3, 2, 1, 2)
                    ,(4, 3, 2, 1))
                    ,((1, 2, 3, 4)
                    ,(2, 1, 2, 3)
                    ,(3, 2, 1, 2)
                    ,(4, 3, 2, 1))
                    ),
                    (((1, 2, 3, 1)
                    ,(1, 1, 1, 1)
                    ,(1, 1, 1, 1)
                    ,(1, 2, 2, 1))
                    ,((2, 2, 3, 2)
                    ,(2, 2, 2, 2)
                    ,(0, 2, 2, 0)
                    ,(0, 0, 0, 0))
                    ),
                    (((1, 2, 3, 4) :: (Int, Int, Int, Int)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                    ,((2, 2, 3, 4)
                    ,(2, 1, 3, 2)
                    ,(0, 2, 1, 2)
                    ,(0, 1, 0, 0))
                    ),
                    (((0, 0, 0, 0)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ,((0, 0, 0, 0)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    )
                ]
                (\(i, e) -> (updateGrid North (grid i)) `shouldBe` (grid e))

    describe "Lib.rotateGridRight" $ do
        it "rotates a grid to the right" $ do
            forM_
                [
                    (((1, 1, 1, 1)
                    ,(2, 2, 2, 2)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ,((0, 0, 2, 1)
                    ,(0, 0, 2, 1)
                    ,(0, 0, 2, 1)
                    ,(0, 0, 2, 1))
                    ),
                    (((1, 2, 3, 4)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                    ,((1, 1, 1, 1)
                    ,(1, 2, 1, 2)
                    ,(1, 2, 2, 3)
                    ,(1, 1, 2, 4)))
                ]
                (\(i, e) -> rotateGridRight (grid i) `shouldBe`(grid e))

    describe "Lib.rotateGridLeft" $ do
        it "rotates a grid to the left" $ do
            forM_
                [
                    (((1, 1, 1, 1)
                    ,(2, 2, 2, 2)
                    ,(0, 0, 0, 0)
                    ,(0, 0, 0, 0))
                    ,((1, 2, 0, 0)
                    ,(1, 2, 0, 0)
                    ,(1, 2, 0, 0)
                    ,(1, 2, 0, 0))
                    ),
                    (((1, 2, 3, 4)
                    ,(1, 1, 2, 2)
                    ,(1, 2, 2, 1)
                    ,(1, 1, 1, 1))
                    ,((4, 2, 1, 1)
                    ,(3, 2, 2, 1)
                    ,(2, 1, 2, 1)
                    ,(1, 1, 1, 1)))
                ]
                (\(i, e) -> rotateGridLeft (grid i) `shouldBe`(grid e))

        it "is the inverse of rotateGridRight" $ property $
             \x -> (x :: Grid Int) == rotateGridLeft (rotateGridRight x)

    describe "Lib.reverse'" $ do
        it "reverses Grids" $ do
            reverse' (grid ((1, 1, 1, 1), (2, 2, 2, 2), (3, 3, 3, 3), (4, 4, 4, 4)))
                `shouldBe`
                (grid ((4, 4, 4, 4), (3, 3, 3, 3), (2, 2, 2, 2), (1, 1, 1, 1)))
        it "is its own inverse function" $ property $ 
             \x -> (x :: Grid Int) == reverse' (reverse' x)

    describe "Lib.reverse''" $ do
        it "reverses rows" $ do
            reverse'' (Row (1, 2, 3, 4)) `shouldBe` (Row (4, 3, 2, 1))
        it "is its own inverse function" $ property $ 
             \x -> (x :: Row Int) == reverse'' (reverse'' x)

    describe "Lib.reverseColumns'" $ do
        it "reverses the columns in a Grid" $ do
            reverseColumns (grid ((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12), (13, 14, 15, 16)))
                `shouldBe` 
                grid ((4, 3, 2, 1), (8, 7, 6, 5), (12, 11, 10, 9), (16, 15, 14, 13))
        it "is its own inverse function" $ property $ 
             \x -> (x :: Grid Int) == reverseColumns (reverseColumns x)
