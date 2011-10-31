-- | Rule engine. This module can be used to express a problem as a
--   set of states and transformation rules. Breadth-first search is
--   then used to try all valid transformations, until a state that
--   satisfies all constraints is found.
module Ruleng where
import Data.Maybe (mapMaybe)
import Data.Set (Set, union, notMember, fromList, singleton)
import Data.List (find, findIndex)
import Control.Arrow ((&&&))
import Control.Applicative

-- | Production rule.
data Rule a b = Rule {
      -- | This can be used to hold a name, number or identifier.
      name :: b,
      -- | Applies the rule if possible, transforming the state.
      apply :: a -> Maybe a
    }

-- | Intermediary state. A state, plus its transformation history.
data IState a b = IState { 
      -- | Rules applied so far. The head of the list is the rule that
      -- was applied /last/.
      history :: [Rule a b],
      -- | Current state.
      value :: a 
    }

-- | Applies a transformation rule to a state.  If the rule was
-- successfully applied, the rule is added to the state's
-- transformation history. Otherwise Nothing is returned.
applyRule :: IState a b -> Rule a b -> Maybe (IState a b)
applyRule x r = apply r (value x) >>= (Just . IState (r:history x))

-- | Takes a tree level (a list of states at a given depth), and a
-- list of unique nodes so far. Generates the next level of depth (a
-- new list of states), eliminating states that have been reached
-- before.
expandLevel :: (Ord a) => [Rule a b] -> (Set a, [IState a b])
            -> (Set a, [IState a b])
expandLevel rs (s, x) = (set, states)
    where states = filter (flip notMember s . value) (x >>= applyRules)
          set = s `union` fromList (map value states)
          applyRules y = mapMaybe (applyRule y) rs

-- | Generates the whole tree of transformations from an initial
-- state. Returns a list of levels; element 0 is the list of elements
-- at depth 0, and so on.
expandTree :: (Ord a) => a -> [Rule a b] -> [[IState a b]]
expandTree x rs = takeWhile (not . null) $ map snd $
                  iterate (expandLevel rs) (singleton x, [IState [] x])

-- | Solves predicate p, under rules r, deriving from initial state i.
-- Generates the tree of states by recursively applying the rules
-- until there is no more possible transformation or a state that
-- satisfies predicate p has been found. The history is in reverse
-- order (the last rule to have been applied is first in the list).
solve :: (Ord a) => (a -> Bool) -> [Rule a b] -> a -> Maybe (a, [Rule a b])
solve p r i = (value &&& history) <$> (find (p . value) . concat $ expandTree i r)

-- | Same as solve, but with a limit on the tree depth.
solveDownTo :: (Ord a) => (a -> Bool) -> [Rule a b] -> a -> Int 
            -> Maybe (a, [Rule a b])
solveDownTo p r i l = (value &&& history) <$> 
                      (find (p . value) . concat . take l $ expandTree i r)

-- | Counts the number of states necessary to solve a problem
solveCount :: (Ord a) => (a -> Bool) -> [Rule a b] -> a -> Either Int Int
solveCount p r i = case findIndex (p . value) tree of
                     Nothing -> Left $ length tree
                     Just n  -> Right n
    where tree = concat $ expandTree i r

-- | Takes an initial state, a list of rules, and rebuilds the list of
-- intermediate states.
replay :: a -> [Rule a b] -> [a]
replay x [] = []
replay x (r:rs) = case apply r x of Nothing -> []
                                    Just x' -> x : replay x' rs