module DFAMinimization (dfaMinimization) where

import AutomatonDefinitions (Automaton (..), DTransition, Partition, State, Symbol, TransitionTable)
import Data.List (elemIndex, groupBy, intersect, nub, partition, sort, (\\))
import Data.Maybe (fromJust, fromMaybe)

-- dfaMinimization minimizes a DFA
-- It follows the steps:
-- 1. Remove states not reachable from the start state
-- 2. Remove states from which no accepting state can be reached
-- 3. Create the initial partition of the DFA's states
-- 4. Create the transition table of the DFA's transitions
-- 5. Refine the partition until it can't be refined any further
-- 6. Rename the states of the DFA
dfaMinimization :: Automaton -> Automaton
dfaMinimization dfa =
  let dfa' = filterLiveStates $ filterReachableStates dfa -- Remove states not reachable from the start state and states from which no accepting state can be reached
      initPartition = getInitialPartition dfa' -- Create the initial partition of the DFA's states
      transitionTable = buildTransitionTable (dtransitions dfa') -- Create the transition table of the DFA's transitions
      finalPartition = refinePartitionDFA initPartition transitionTable (dalphabet dfa') -- Refine the partition until it can't be refined any further
      newStates = zip [0 ..] $ filter (not . null) finalPartition
      newStartState = mapState (dinitialState dfa') newStates
      newAcceptStates = nub $ map (\state -> mapState state newStates) (dfinalStates dfa')
      newTransitions =
        [ (mapState state newStates, sym, mapState dstate newStates)
          | (state, sym, dstate) <- dtransitions dfa'
        ]
   in DFA (map fst newStates) (dalphabet dfa') (nub newTransitions) newStartState newAcceptStates

-- filterReachableStates removes states not reachable from the start state
-- It follows the steps:
-- 1. Find the reachable states of the DFA
-- 2. Remove states not reachable from the start state
-- 3. Remove transitions to states not reachable from the start state
-- 4. Remove accepting states not reachable from the start state
-- 5. Return the DFA
filterReachableStates :: Automaton -> Automaton
filterReachableStates dfa =
  let reachables = getReachableStates dfa
      newStates = intersect reachables (dstates dfa)
      newTransitions = [(state, sym, dstate) | (state, sym, dstate) <- dtransitions dfa, state `elem` newStates, dstate `elem` newStates]
      newAcceptStates = intersect reachables (dfinalStates dfa)
   in dfa {dstates = newStates, dtransitions = newTransitions, dfinalStates = newAcceptStates}

-- getLiveStates finds the states reachable from an accepting state
-- It follows the steps:
-- 1. Find the states reachable from an accepting state
-- 2. Return the states
-- Note: This function is not used in the minimization algorithm
getLiveStates :: Automaton -> [State]
getLiveStates dfa = dfs (dfinalStates dfa) []
  where
    dfs [] visited = visited
    -- dfs
    dfs (x : xs) visited
      | x `elem` visited = dfs xs visited
      | otherwise = dfs (predecessors x ++ xs) (x : visited)

    predecessors state = [ss | (ss, _, state') <- dtransitions dfa, state' == state]

-- filterLiveStates removes states from which no accepting state can be reached
-- It follows the steps:
-- 1. Find the live states of the DFA
-- 2. Remove states from which no accepting state can be reached
-- 3. Remove transitions from states from which no accepting state can be reached
-- 4. Remove accepting states from which no accepting state can be reached
-- 5. Return the DFA
filterLiveStates :: Automaton -> Automaton
filterLiveStates dfa =
  -- lives equals the result of applying getliveStates to dfa
  let lives = getLiveStates dfa
      -- newStates equals the result of applying intersect to lives and dstates dfa
      newStates = intersect lives (dstates dfa)
      -- newTransitions equals the result of applying filter to dtransitions dfa
      newTransitions = [(state, sym, dstate) | (state, sym, dstate) <- dtransitions dfa, state `elem` newStates, dstate `elem` newStates]
      -- newAcceptStates equals the result of applying intersect to lives and dfinalStates dfa
      newAcceptStates = intersect lives (dfinalStates dfa)
   in -- this returns the DFA with the new states, transitions, and accepting states
      dfa {dstates = newStates, dtransitions = newTransitions, dfinalStates = newAcceptStates}

-- getInitialPartition creates the initial partition of the DFA's states
-- It follows the steps:
-- 1. Create a group of accepting states
-- 2. Create a group of non-accepting states
getInitialPartition :: Automaton -> Partition
getInitialPartition dfa = [dfinalStates dfa, dstates dfa \\ dfinalStates dfa]

-- buildTransitionTable creates the transition table of the DFA's transitions
-- It follows the steps:
-- 1. Create a list of all possible transitions
-- 2. For each transition, add it to the transition table
buildTransitionTable :: [(State, Symbol, State)] -> TransitionTable
buildTransitionTable transition = [((state, sym), dstate) | (state, sym, dstate) <- transition]

-- refinePartitionDFA refines the partition until it can't be refined any further
-- It follows the steps:
-- 1. Refine the partition
-- 2. If the refined partition is the same as the original partition, return the partition
-- 3. Otherwise, refine the refined partition
refinePartitionDFA :: Partition -> TransitionTable -> [Symbol] -> Partition
refinePartitionDFA partition transitiontable sym =
  let partition' = splitPartition partition transitiontable sym
   in if partition' == partition
        then partition
        else refinePartitionDFA partition' transitiontable sym

-- splitPartition splits the partition
-- It follows the steps:
-- 1. For each group in the partition, split the group into subgroups based on the transition function
-- 2. Concatenate the subgroups into a new partition
splitPartition :: Partition -> TransitionTable -> [Symbol] -> Partition
splitPartition partition transitiontable sym = concat [splitGroupByTransition state transitiontable partition sym | state <- partition]

-- splitGroupByTransition splits a group into subgroups based on the transition function
-- It follows the steps:
-- 1. For each symbol in the alphabet, split the group into subgroups based on the transition function
-- 2. Return the subgroups
splitGroupByTransition :: [State] -> TransitionTable -> Partition -> [Symbol] -> Partition
splitGroupByTransition state transitiontable partition symbols = groupBy (\s1 s2 -> all (\sym -> getGroupIndex (getTransitionState s1 sym transitiontable) partition == getGroupIndex (getTransitionState s2 sym transitiontable) partition) symbols) state

-- getTransitionState finds the state reachable from a state with a symbol
-- It follows the steps:
-- 1. Find the state reachable from the state with the symbol
-- 2. If no state is reachable, return the state
-- 3. Otherwise, return the state reachable from the state with the symbol
getTransitionState :: State -> Symbol -> TransitionTable -> State
getTransitionState state sym transitiontable = fromMaybe state $ lookup (state, sym) transitiontable

-- getGroupIndex finds the group a state belongs to
-- It follows the steps:
-- 1. Find the index of the group the state belongs to
-- 2. Return the index
getGroupIndex :: State -> Partition -> Int
getGroupIndex state partition = fromJust $ elemIndex True $ map (elem state) partition

-- mapState renames a state
-- It follows the steps:
-- 1. Find the group the state belongs to
-- 2. Return the index of the group
mapState :: State -> [(State, [State])] -> State
mapState state states = fromMaybe (-1) $ lookup True [(state `elem` a, s) | (s, a) <- states]

-- getReachableStates finds the states reachable from the start state
-- It follows the steps:
-- 1. Find the states reachable from the start state
-- 2. Return the states
getReachableStates :: Automaton -> [State]
getReachableStates dfa = dfs (dinitialState dfa) []
  where
    dfs current visited
      | current `elem` visited = visited
      | otherwise = foldl (flip dfs) (current : visited) (successors current)
    successors state = [state' | (s, _, state') <- dtransitions dfa, s == state]
