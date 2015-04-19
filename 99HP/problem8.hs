import Criterion.Main
import Data.List

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y		= compress (x:xs)
  | otherwise = x : compress (y:xs)

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group

randomData :: RandomGen g, Eq a => g -> [a]
randomData g = take 5000 $ randoms g

main = defaultMain [
	bgroup "compress" [ bench "
