import System.Random
import Data.List
import Data.Char
 
str n = do
    seed  <- newStdGen
    let rs = randomlist n seed
    print $ map ( chr . (\ x -> (mod x 27)+97) . abs )rs
 
randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)