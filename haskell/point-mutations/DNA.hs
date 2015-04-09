module DNA (hammingDistance) where


hammingDistance :: Eq a => [a] -> [a] -> Int
hammingDistance = ((length . filter not) .) . zipWith (==)
