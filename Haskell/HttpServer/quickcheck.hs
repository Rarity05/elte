import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

removeNegative :: [Int] -> [Int]
removeNegative = filter (>= 0)

drop2 :: [a] -> [a]
drop2 []        = []
drop2 [_]       = []
drop2 (_:_:xs)  = xs

newtype ZippList a = ZPL ([a], [a])
  deriving (Show)

left :: ZippList a -> ZippList a
left (ZPL (x : xs, ys)) = ZPL (xs, x : ys)
left zl = zl

prop_removeNegative :: [Int] -> Bool
prop_removeNegative x = foldl (&&) True $ map (>=0) $ removeNegative x

prop_drop2 :: [a] -> Bool
prop_drop2 x = ((length x) - (length (drop2 x))) <= 2

prop_left :: ZippList a -> Bool
prop_left list@(ZPL ([], oRight)) = do
  let (ZPL (nLeft, nRight)) = left list
  length nLeft == 0 && length nRight == length oRight
prop_left list@(ZPL (oLeft, oRight)) = do
  let (ZPL (nLeft, nRight)) = left list
  length oLeft - length nLeft == 1 && length nRight - length oRight == 1

instance Arbitrary a => Arbitrary (ZippList a) where
  arbitrary = do
    left <- arbitrary
    right <- arbitrary
    return $ ZPL (left, right)
