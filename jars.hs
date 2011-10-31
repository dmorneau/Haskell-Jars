module Main where
import Ruleng
import Control.Applicative
import System.Environment
import Text.Printf

type JarState = (Int, Int)
type JarRule  = Rule JarState String

rules :: Int -> Int -> [JarRule]
rules maxX maxY = map (uncurry Rule) [
 ("R1 Fill jar A.", \(x,y) -> when (x < maxX) (maxX, y) ),
 ("R2 Fill jar B.", \(x,y) -> when (y < maxY) (x, maxY) ),
 ("R3 Empty jar A.", \(x,y) -> when (x > 0) (0, y) ),
 ("R4 Empty jar B.", \(x,y) -> when (y > 0) (x, 0) ),
 ("R5 Pour B into A until A is full.", \(x,y) -> 
      when (x + y >= maxX && x < maxX && y > 0) (maxX, y - (maxX - x)) ),
 ("R6 Pour A into B until B is full.", \(x,y) -> 
      when (x + y >= maxY && x > 0 && y < maxY) (x - (maxY - y), maxY) ),
 ("R7 Pour B into A until B is empty.", \(x,y) -> 
      when (x + y < maxX && y > 0) (x + y, 0) ),
 ("R8 Pour A into B until A is empty.", \(x,y) -> 
      when (x + y < maxY && x > 0) (0, x + y) )]
    where when c v = if c then Just v else Nothing

solveJars maxX maxY initial final = solve (==final) (rules maxX maxY) initial

showSolution mx my i f = case solveJars mx my i f of
                           Nothing -> putStrLn "No solution"
                           Just (v,h) -> do
                             putStrLn $ "Solution found: "
                             mapM_ (\(x,y) -> putStr x >> putStrLn y) desc
                                 where desc = zip (map show $ replay i history) 
                                                  (map name history)
                                       history = reverse h

countSteps mx my i f = length . snd <$> solveJars mx my i f

main = do
  args <- getArgs
  let out = head args
  let [mx, my, fx, fy] = map read $ tail args
  case out of
    "s" -> showSolution mx my (0,0) (fx, fy)
    "n" -> do putStrLn $ "Number of steps: "
              case countSteps mx my (0,0) (fx, fy) of
                Nothing -> putStrLn "No solution"
                Just n  -> print n
    "c" -> case solveCount (==(fx,fy)) (rules mx my) (0,0) of
             Left n -> printf "No solution, %d states tried" n
             Right n -> printf "Solution found, %d states tried" n
    "t" -> print . map value . concat $ expandTree (0,0) (rules mx my)
