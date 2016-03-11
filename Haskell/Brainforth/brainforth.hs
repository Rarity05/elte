import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M

class BFMem m where
  incVal    :: m -> m
  decVal    :: m -> m
  isNull    :: m -> Bool
  getVal    :: m -> Int
  putVal    :: m -> Int -> m
  memLeft   :: m -> m
  memRight  :: m -> m

data Tape = T
  { tVec :: Vector Int
  , tIx  :: Int
  } deriving (Show, Eq)

data BFSymbol
  = Inc | Dec | MemLeft | MemRight | BrktOpen | BrktClose | In | Out
  | StartSeq | EndSeq | SeqId Char
  deriving (Show, Eq)

data BFState = S
  { sCallStk :: [(Int, Char)]
  , sMem     :: Tape
  , sIn      :: [Int]
  , sOut     :: [Int]
  } deriving (Show, Eq)

type BFSequence = Vector BFSymbol
type BFEnv = Map Char BFSequence
sq0 :: Char
sq0 = '*'

-- FUNCTIONS

instance BFMem Tape where
  incVal (T vec idx) = T { tVec = (vec V.// [(idx, ((vec V.! idx)+1))]), tIx = idx }
  decVal (T vec idx) = T { tVec = (vec V.// [(idx, ((vec V.! idx)-1))]), tIx = idx }
  isNull (T vec idx) = (vec V.! idx) == 0
  getVal (T vec idx) = (vec V.! idx)
  putVal (T vec idx) val = T { tVec = (vec V.// [(idx, val)]), tIx = idx }
  memLeft (T vec idx)
    | 0 == idx = T { tVec = vec, tIx = ((V.length vec) - 1) }
    | otherwise = T { tVec = vec, tIx = idx - 1 }
  memRight (T vec idx)
    | ((V.length vec) - 1) == idx = T { tVec = vec, tIx = 0 }
    | otherwise = T { tVec = vec, tIx = idx + 1 }

newTape :: Int -> Tape
newTape n = T { tVec = V.replicate n 0, tIx = 0 }

parseProgram :: String -> BFEnv
parseProgram input = M.fromList (map listToTuple (wordsWhen (==';') input)) where
  wordsWhen :: (Char -> Bool) -> String -> [String]
  wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'
  listToTuple :: [Char] -> (Char, BFSequence)
  listToTuple (x:xs)
    | x == ':' = (head xs, V.fromList (charToSymbol (tail xs)))
    | otherwise = (sq0, V.fromList (charToSymbol (x:xs)))
  charToSymbol :: [Char] -> [BFSymbol]
  charToSymbol [] = []
  charToSymbol (x:xs)
    | x == '+' = [Inc] ++ charToSymbol xs
    | x == '-' = [Dec] ++ charToSymbol xs
    | x == '<' = [MemLeft] ++ charToSymbol xs
    | x == '>' = [MemRight] ++ charToSymbol xs
    | x == '[' = [BrktOpen] ++ charToSymbol xs
    | x == ']' = [BrktClose] ++ charToSymbol xs
    | x == ',' = [In] ++ charToSymbol xs
    | x == '.' = [Out] ++ charToSymbol xs
    | otherwise = [SeqId x] ++ charToSymbol xs

matchingBracket :: BFSequence -> Int -> Int
matchingBracket sequance index = search sequance index where
  search :: BFSequence -> Int -> Int
  search seq idx
    | seq V.! idx == BrktOpen = searchRight seq (idx + 1) 1
    | seq V.! idx == BrktClose = searchLeft seq (idx - 1) 1
    | otherwise = 0
  searchRight :: BFSequence -> Int -> Int -> Int
  searchRight rightSeq rightIdx bracketCount
    | bracketCount == 0 = rightIdx - 1
    | rightIdx == V.length rightSeq = 0
    | rightSeq V.! rightIdx == BrktClose = searchRight rightSeq (rightIdx + 1) (bracketCount - 1)
    | rightSeq V.! rightIdx == BrktOpen = searchRight rightSeq (rightIdx + 1) (bracketCount + 1)
    | otherwise = searchRight rightSeq (rightIdx + 1) bracketCount
  searchLeft :: BFSequence -> Int -> Int -> Int
  searchLeft leftSeq leftIdx bracketCount
    | bracketCount == 0 = leftIdx + 1
    | leftIdx == -1 = 0
    | leftSeq V.! leftIdx == BrktClose = searchLeft leftSeq (leftIdx - 1) (bracketCount + 1)
    | leftSeq V.! leftIdx == BrktOpen = searchLeft leftSeq (leftIdx - 1) (bracketCount - 1)
    | otherwise = searchLeft leftSeq (leftIdx - 1) bracketCount

--step :: ReaderT BFEnv (State BFState) ()
--runProgram :: String -> [Int] -> [Int]

-- TESTS


test_BFMem_Tape =
  [ incVal   t                  ==  t { tVec = V.fromList [ 1, 1, 2, 3] }
  , decVal   t                  ==  t { tVec = V.fromList [-1, 1, 2, 3] }
  , isNull   t                  ==  True
  , isNull   t { tIx = 1 }      ==  False
  , getVal   t                  ==  0
  , getVal   t { tIx = 3 }      ==  3
  , putVal   t             451  ==  t { tVec = V.fromList [451, 1, 2, 3] }
  , putVal   t { tIx = 3 } 451  ==  T { tVec = V.fromList [0, 1, 2, 451], tIx = 3 }
  , memLeft  t                  ==  t { tIx = 1 }
  , memLeft  t { tIx = 3 }      ==  t { tIx = 0 }
  , memRight t { tIx = 3 }      ==  t { tIx = 2 }
  , memRight t                  ==  t { tIx = 3 }
  ]
  where t = T { tVec = V.fromList [0, 1, 2, 3], tIx = 0 }



test_parseProgram =
  [ parseProgram "+-<>[],."    == M.fromList [(sq0, V.fromList [Inc, Dec, MemLeft, MemRight, BrktOpen, BrktClose, In, Out])]
  , parseProgram ":A-;A+"      == M.fromList [(sq0, V.fromList [SeqId 'A', Inc]), ('A', V.fromList [Dec])]
  , parseProgram ":A-;:B+;AB+" == M.fromList [(sq0, V.fromList [SeqId 'A', SeqId 'B', Inc]), ('A', V.fromList [Dec]), ('B', V.fromList [Inc])]
  ]



test_matchingBracket = testBrkt sq1 pairs1 ++ testBrkt sq2 pairs2
  where
    testBrkt sq pairs = map (\(s, e) -> matchingBracket (mkSq sq) s == e) pairs
    mkSq   = V.fromList . map (\c -> case c of '(' -> BrktOpen; ')' -> BrktClose; _ -> Inc)
    sq1    = "(a)(b)"
    pairs1 = [(0, 2), (3, 5)]
    sq2    = "((())()())"
    pairs2 = zip [0..9] [9, 4, 3, 2, 1, 6, 5, 8, 7, 0]


{-
test_step =
  [ exec env1 st1{sCallStk = [(0, sq0)]} == st1{sCallStk = [(1, sq0)], sMem = incVal $ newTape 32}
  , exec env1 st1{sCallStk = [(1, sq0)]} == st1{sCallStk = [(2, sq0)], sMem = memRight $ newTape 32}
  , exec env1 st1{sCallStk = [(2, sq0)]} == st1{sCallStk = [(5, sq0)]}
  , exec env1 st1{sCallStk = [(2, sq0)], sMem = incVal $ newTape 32} == st1{sCallStk = [(3, sq0)], sMem = incVal $ newTape 32}
  , exec env1 st1{sCallStk = [(4, sq0)]} == st1{sCallStk = [(2, sq0)]}
  , exec env1 st1{sCallStk = [(5, sq0)]} == st1{sCallStk = [(6, sq0)], sMem = putVal (newTape 32) 43, sIn = []}
  , exec env1 st1{sCallStk = [(6, sq0)]} == st1{sCallStk = [(7, sq0)], sOut = [0]}

  , exec env2 st2{sCallStk = [(1, sq0)]} == st2{sCallStk = [(0, 'A'), (2, sq0)]}
  , exec env2 st2{sCallStk = [(0, 'A'), (2, sq0)]} == st2{sCallStk = [(1, 'A'), (2, sq0)], sMem = incVal $ newTape 32}
  , exec env2 st2{sCallStk = [(1, 'A'), (2, sq0)]} == st2{sCallStk = [(2, sq0)]}
  ]
  where
    exec env st = execState (runReaderT step env) st

    env1 = M.fromList [(sq0, V.fromList [Inc, MemRight, BrktOpen, Inc, BrktClose, In, Out])]
    st1  = S {sCallStk = [], sMem = newTape 32, sIn = [43], sOut = []}

    env2 = M.fromList [(sq0, V.fromList [Dec, SeqId 'A']), ('A', V.fromList [Inc])]
    st2  = S {sCallStk = [], sMem = newTape 32, sIn = [], sOut = []}
-}

{-
test_runProgram =
  [ runProgram sqSimple    []           == [3, 4, 5, 4, 3]
  , runProgram sqLoop      []           == [4, 3, 2, 1, 0]
  , runProgram sqInput     [69, 418]    == [69, 420]
  , runProgram sqMovePtr   [1, 2, 3]    == [3, 2, 1]
  , runProgram sqSimpleSq  []           == [0, 2]
  , runProgram sqAddThree  [1, 10, 100] == [111]
  ]
  where
    sqSimple    = "+++.+.+.-.-."
    sqLoop      = "++++[.-]."
    sqInput     = ",.,++."
    sqMovePtr   = ",>,>,.<.<."
    sqSimpleSq  = ":A++;.A."
    sqAddThree  = ":A>[-<+>]<;,>,>,<A<A."
-}