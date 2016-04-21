import Data.Maybe
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

data Matrix = M
  { mDat :: Seq Integer
  , mSize :: Int
  , mIx :: (Int, Int) -> Maybe Int
  }

newMatrix :: Int -> Matrix
newMatrix size = M {
  mDat = Seq.replicate (size * size) 0,
  mSize = size,
  mIx = \(x,y) -> do
    if x < 1 || y < 1 || x > size || y > size
      then Nothing
      else Just $ ((x-1) * size + y) - 1
}

setMatrix :: (Int, Int) -> Integer -> Matrix -> Matrix
setMatrix (x, y) value matrix@(M mDat _ mIx) = do
  let poz = mIx (x,y)
  if isJust poz
    then matrix { mDat = Seq.update (fromJust poz) value mDat }
    else matrix

--getMatrix :: (Int, Int) -> Matrix -> Maybe Integer
--addMatrix :: Matrix -> Matrix -> Maybe Matrix
--mulMatrix :: Matrix -> Matrix -> Maybe Matrix

-- TESTS

test_newMatrix :: [Bool]
test_newMatrix =
  let m3 = newMatrix 3 in
  [ mDat m3 == Seq.replicate 9 0
  , mSize m3 == 3
  , mIx m3 (0, 0) == Nothing
  , mIx m3 (1, 1) == Just 0
  , mIx m3 (2, 3) == Just 5
  , mIx m3 (3, 2) == Just 7
  ]

test_setMatrix :: [Bool]
test_setMatrix =
  let m2 = newMatrix 2 in
  [ mDat (setMatrix (0, 0) 1 m2) == mDat m2
  , mDat (setMatrix (1, 1) 1 m2) == Seq.fromList [1, 0, 0, 0]
  , mDat (setMatrix (1, 2) 1 m2) == Seq.fromList [0, 1, 0, 0]
  ]

{-
test_getMatrix :: [Bool]
test_getMatrix =
  let m2 = setMatrix (1, 2) 1 $ newMatrix 2 in
  [ getMatrix (0, 0) m2 == Nothing
  , getMatrix (1, 1) m2 == Just 0
  , getMatrix (1, 2) m2 == Just 1
  ]
-}
{-
test_addMatrix :: [Bool]
test_addMatrix =
  let ma = newMatrix 3
      mb = setMatrix (1,2) 1 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 1 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ addMatrix ma mb
      , (mDat <$> addMatrix mb mc) == Just (Seq.fromList [2, 1, 1, 0])
      ]
-}
{-
test_mulMatrix :: [Bool]
test_mulMatrix =
  let ma = newMatrix 3
      mb = setMatrix (2,2) 2 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 4 $ setMatrix (2,2) 3 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ mulMatrix ma mb
      , (mDat <$> mulMatrix mb mc) == Just (Seq.fromList [1, 0, 8, 6])
      , (mDat <$> mulMatrix mc mb) == Just (Seq.fromList [1, 0, 4, 6])
      ]
-}