import Test.Hspec

import Lib
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Lib.updateGrid" $ do
        it "updates eastwards and returns the updated grid" $ do
            updateGrid East
                ((1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((0, 0, 2, 2)
                ,(0, 0, 2, 2)
                ,(0, 0, 2, 2)
                ,(0, 0, 2, 2))

            updateGrid East
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((1, 2, 3, 4)
                ,(0, 0, 2, 3)
                ,(0, 1, 3, 1)
                ,(0, 0, 2, 2))

            updateGrid East
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((1, 2, 3, 4)
                ,(0, 0, 2, 3)
                ,(0, 1, 3, 1)
                ,(0, 0, 2, 2))

            updateGrid East
                ((0, 0, 1, 0)
                ,(0, 0, 1, 0)
                ,(0, 0, 1, 0)
                ,(0, 0, 1, 0))
                `shouldBe`
                ((0, 0, 0, 1)
                ,(0, 0, 0, 1)
                ,(0, 0, 0, 1)
                ,(0, 0, 0, 1))

            updateGrid East
                ((1, 0, 0, 0)
                ,(0, 1, 0, 0)
                ,(0, 0, 1, 0)
                ,(0, 0, 0, 1))
                `shouldBe`
                ((0, 0, 0, 1)
                ,(0, 0, 0, 1)
                ,(0, 0, 0, 1)
                ,(0, 0, 0, 1))

            updateGrid East
                ((0, 1, 2, 0)
                ,(1, 2, 0, 0)
                ,(1, 2, 3, 0)
                ,(1, 2, 3, 4))
                `shouldBe`
                ((0, 0, 1, 2)
                ,(0, 0, 1, 2)
                ,(0, 1, 2, 3)
                ,(1, 2, 3, 4))

        it "updates westwards and returns the updated grid" $ do
            updateGrid West
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((1, 2, 3, 4)
                ,(2, 3, 0, 0)
                ,(1, 3, 1, 0)
                ,(2, 2, 0, 0))

        it "updates southwards and returns the updated grid" $ do
            updateGrid South
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((0, 2, 0, 0)
                ,(0, 1, 3, 4)
                ,(2, 2, 3, 2)
                ,(2, 1, 1, 2))

            updateGrid South
                ((1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((0, 0, 0, 0)
                ,(0, 0, 0, 0)
                ,(2, 2, 2, 2)
                ,(2, 2, 2, 2))

        it "updates northwards and returns the updated grid" $ do
            updateGrid North
                ((1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((2, 2, 2, 2)
                ,(2, 2, 2, 2)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
            updateGrid North
                ((2, 2, 2, 2)
                ,(2, 2, 2, 2)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
                `shouldBe`
                ((3, 3, 3, 3)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
            updateGrid North
                ((1, 2, 3, 4)
                ,(1, 2, 3, 4)
                ,(1, 2, 3, 4)
                ,(1, 2, 3, 4))
                `shouldBe`
                ((2, 3, 4, 5)
                ,(2, 3, 4, 5)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
            updateGrid North
                ((1, 2, 3, 4)
                ,(2, 1, 2, 3)
                ,(3, 2, 1, 2)
                ,(4, 3, 2, 1))
                `shouldBe`
                ((1, 2, 3, 4)
                ,(2, 1, 2, 3)
                ,(3, 2, 1, 2)
                ,(4, 3, 2, 1))
            updateGrid North
                ((1, 2, 3, 1)
                ,(1, 1, 1, 1)
                ,(1, 1, 1, 1)
                ,(1, 2, 2, 1))
                `shouldBe`
                ((2, 2, 3, 2)
                ,(2, 2, 2, 2)
                ,(0, 2, 2, 0)
                ,(0, 0, 0, 0))
            updateGrid North
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((2, 2, 3, 4)
                ,(2, 1, 3, 2)
                ,(0, 2, 1, 2)
                ,(0, 1, 0, 0))
            updateGrid North
                ((0, 0, 0, 0)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
                `shouldBe`
                ((0, 0, 0, 0)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))


    describe "Lib.rotateGridRight" $ do
        it "rotates a grid to the right" $ do
            rotateGridRight
                ((1, 1, 1, 1)
                ,(2, 2, 2, 2)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
                `shouldBe`
                ((0, 0, 2, 1)
                ,(0, 0, 2, 1)
                ,(0, 0, 2, 1)
                ,(0, 0, 2, 1))
            rotateGridRight
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((1, 1, 1, 1)
                ,(1, 2, 1, 2)
                ,(1, 2, 2, 3)
                ,(1, 1, 2, 4)
                )

    describe "Lib.rotateGridLeft" $ do
        it "rotates a grid to the left" $ do
            rotateGridLeft
                ((1, 1, 1, 1)
                ,(2, 2, 2, 2)
                ,(0, 0, 0, 0)
                ,(0, 0, 0, 0))
                `shouldBe`
                ((1, 2, 0, 0)
                ,(1, 2, 0, 0)
                ,(1, 2, 0, 0)
                ,(1, 2, 0, 0))
            rotateGridLeft
                ((1, 2, 3, 4)
                ,(1, 1, 2, 2)
                ,(1, 2, 2, 1)
                ,(1, 1, 1, 1))
                `shouldBe`
                ((4, 2, 1, 1)
                ,(3, 2, 2, 1)
                ,(2, 1, 2, 1)
                ,(1, 1, 1, 1))

        it "is the inverse of rotateGridRight" $ property $
            \x -> (x :: Grid Int) == rotateGridLeft (rotateGridRight x)

    describe "Lib.reverse'" $ do
        it "reverses 4-tuples" $ do
            reverse' (1, 2, 3, 4) `shouldBe` (4, 3, 2, 1)
        it "is its own inverse function" $ property $ 
            \x -> (x :: Row Int) == reverse' (reverse' x)

    describe "Lib.reverseColumns'" $ do
        it "reverses the columns in a Grid" $ do
            reverseColumns ((1, 2, 3, 4), (5, 6, 7, 8), (9, 10, 11, 12), (13, 14, 15, 16))
                `shouldBe` 
                ((4, 3, 2, 1), (8, 7, 6, 5), (12, 11, 10, 9), (16, 15, 14, 13))
        it "is its own inverse function" $ property $ 
            \x -> (x :: Row Int) == reverse' (reverse' x)
