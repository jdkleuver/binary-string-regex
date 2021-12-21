module BinaryRegex (regexDivisibleBy) where
import Data.List
import Data.Bits
import qualified Data.Map as M

type State = Int
-- This type definition allows a different starting and accepting state, although all the functions
-- assume that 0 will be both the initial state and the only accepting state
-- There is no "alphabet" of accepted Chars, any string with an unexpected Char is rejected 
type GNFA = ([State], M.Map (State, State) [Char], State, [State])

-- A singleton doesn't need parentheses to group it, wrap anything more than one character in parens
wrap :: [Char] -> [Char]
wrap [] = []
wrap (x:[]) = [x]
wrap xs = '(':xs++")"

-- Remove a state from the GNFA, create new transitions where necessary
removeState :: State -> GNFA -> GNFA
removeState s (states, transitions, i, fs) = (delete s states, allNewTransitions, i, fs)
  where
    allNewTransitions = M.unionWith (\a b -> (wrap a)++"|"++(wrap b)) others (M.fromList updated)
    from = M.toList $ M.filterWithKey (\(x,y) _ -> y == s && x /= y) transitions
    to = M.toList $ M.filterWithKey (\(x,y) _ -> x == s && x /= y) transitions
    self = case M.lookup (s,s) transitions of
      Nothing -> ""
      Just reg -> (wrap reg)++"*"
    others = M.filterWithKey (\(x,y) _ -> x /=s && y /= s) transitions
    updated = newTransitions from to
    newTransitions [] _ = []
    newTransitions (x:xs) [] = newTransitions xs to
    newTransitions (((fromState,_),fromReg):xs) (((_,toState),toReg):ys) = ((fromState,toState),(wrap fromReg)++self++(wrap toReg)):newTransitions (((fromState,s),fromReg):xs) ys

-- Assumes that there is only one final state, which must be "0" (All other states must be a higher number than 0)
removeAllStates :: GNFA -> GNFA
removeAllStates (x:[], transitions, i, fs) = (x:[], transitions, i, fs)
removeAllStates (xs, transitions, i, fs) = removeAllStates $ removeState (maximum xs) (xs, transitions, i, fs)

-- Assumes 0 is the final state
gnfaToRegex :: GNFA -> [Char]
gnfaToRegex g = case M.lookup (0,0) transitions of
  Nothing -> ""
  Just x -> x
  where (_, transitions, _, _) = removeAllStates g

-- This will return a complete DFA, but instead of a delta function it has a list of state transitions
-- Each state transition is a regular expression.
createGNFA :: Int -> GNFA
createGNFA n = (reverse states, M.fromList $ zip (zip (concat (transpose $ take 2 $ repeat states)) (concat $ take 2 $ repeat states)) (concat $ repeat ["0", "1"]), 0, [0]) 
    where states = [0..((fromIntegral n)-1)]

isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 n = n .&. (n-1) == 0

-- For powers of two we can skip the DFA and just directly construct the regex
regexDivisibleBy :: Int -> [Char]
regexDivisibleBy n
    | isPower2 n = "^(0|([01]+" ++ (take (round $ logBase 2 (fromIntegral n)) $ repeat '0') ++ "))+$"
    | otherwise = '^':'(':(gnfaToRegex $ createGNFA n)++")+$"

