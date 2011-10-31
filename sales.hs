import Data.List (minimumBy, permutations)
import Data.Ord (comparing)

type City = Int
type Distance = Int

--            札幌　東京　名古屋　大阪　福岡
distance :: City -> City -> Distance
distance a b = [[  0, 230, 260, 260, 280]
               ,[230,   0, 120, 200, 240]
               ,[260, 120,   0,  80, 230]
               ,[260, 200,  80,   0, 220]
               ,[280, 240, 230, 220,   0]] !! a !! b

total :: [City] -> Distance
total xs = sum $ zipWith distance (init xs) (tail xs)

-- Finds the shortest itinery start from city x and passing through a
-- list of cities.
salesman :: City -> [City] -> [City]
salesman x = minimumBy (comparing (total . (x:))) . permutations

-- Solves the Salesman problem, with starting city Tokyo (1), visiting
-- in any order Sapporo, Nagoya, Osaka and Fukuoka ([0,2,3,4]).
main :: IO ()
main = putStrLn "Shortest path: " >>
       mapM_ (putStrLn . cityName) (salesman 1 [0,2,3,4])

cityName :: City -> String
cityName = (!!) ["Sapporo", "Tokyo", "Nagoya", "Osaka", "Fukuoka"]