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

--prop_removeNegative :: [Int] -> Bool
--prop_drop2 :: [a] -> Bool
--prop_left :: ZippList a -> Bool
--instance Arbitrary a => Arbitrary (ZippList a) where