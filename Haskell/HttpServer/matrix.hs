import Control.Parallel.Strategies
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

getMatrix :: (Int, Int) -> Matrix -> Maybe Integer
getMatrix (x, y) matrix@(M mDat _ mIx) = do
  let poz = mIx (x,y)
  if (isJust poz)
    then Just $ Seq.index mDat (fromJust poz)
    else Nothing

addMatrix :: Matrix -> Matrix -> Maybe Matrix
addMatrix (M a_mDat a_mSize _) (M b_mDat b_mSize _) = do
  if (a_mSize /= b_mSize)
    then Nothing
    else do
      let matrix = fmap (\(x,y) -> x+y) (Seq.zip a_mDat b_mDat) `using` parTraversable rseq
      Just $ (newMatrix a_mSize) { mDat = matrix }

mulMatrix :: Matrix -> Matrix -> Maybe Matrix
mulMatrix mA@(M a_mDat a_mSize _) mB@(M b_mDat b_mSize _)
  | a_mSize /= b_mSize = Nothing
  | otherwise = do
    let rows = getRows mA
    let columns = getColumns mB
    let paired = pairSeq rows columns
    let matrix = fmap (\x -> foldl (+) 0 (fmap (\(a, b) -> a*b) x)) paired `using` parTraversable rseq
    Just $ (newMatrix a_mSize) { mDat = Seq.fromList matrix }

pairSeq :: [Seq Integer] -> [Seq Integer] -> [Seq (Integer, Integer)]
pairSeq rows columns = acc rows columns [] where
  acc :: [Seq Integer] -> [Seq Integer] -> [Seq (Integer, Integer)] -> [Seq (Integer, Integer)]
  acc [] _ v = v
  acc (x:xs) ys v = do
    let rX = fmap (\i -> Seq.zip x i) ys
    acc xs ys (v ++ rX)

getRows :: Matrix -> [Seq Integer]
getRows (M mDat mSize mIx) = acc 1 [] where
  acc :: Int -> [Seq Integer] -> [Seq Integer]
  acc i v
    | i == (mSize + 1) = v
    | otherwise = acc (i+1) (v ++ [Seq.take mSize (Seq.drop ((i-1)*mSize) mDat)])

getColumns :: Matrix -> [Seq Integer]
getColumns (M mDat mSize mIx) = acc 1 [] where
  acc :: Int -> [Seq Integer] -> [Seq Integer]
  acc i v
    | i == (mSize + 1) = v
    | otherwise = acc (i+1) (v ++ [(Seq.fromList (accCol i 1 []))])
  accCol :: Int -> Int -> [Integer] -> [Integer]
  accCol column row vv
    | row == (mSize + 1) =  vv
    | otherwise = accCol column (row+1) (vv ++ [Seq.index mDat ((((row-1)*mSize)+column)-1)])

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

test_getMatrix :: [Bool]
test_getMatrix =
  let m2 = setMatrix (1, 2) 1 $ newMatrix 2 in
  [ getMatrix (0, 0) m2 == Nothing
  , getMatrix (1, 1) m2 == Just 0
  , getMatrix (1, 2) m2 == Just 1
  ]

test_addMatrix :: [Bool]
test_addMatrix =
  let ma = newMatrix 3
      mb = setMatrix (1,2) 1 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 1 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ addMatrix ma mb
      , (mDat <$> addMatrix mb mc) == Just (Seq.fromList [2, 1, 1, 0])
      ]

test_mulMatrix :: [Bool]
test_mulMatrix =
  let ma = newMatrix 3
      mb = setMatrix (2,2) 2 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 4 $ setMatrix (2,2) 3 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ mulMatrix ma mb
      , (mDat <$> mulMatrix mb mc) == Just (Seq.fromList [1, 0, 8, 6])
      , (mDat <$> mulMatrix mc mb) == Just (Seq.fromList [1, 0, 4, 6])
      ]
