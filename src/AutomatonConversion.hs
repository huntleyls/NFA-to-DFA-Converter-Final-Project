module AutomatonConversion where

import AutomatonDefinitions (Automaton (..), State, Symbol, Transition)
import Data.List (elemIndex, intersect, nub, sort, (\\))
import Data.Maybe (fromJust)

-- nfa2dfa converts an NFA to a DFA
-- It follows the steps:
-- 1. Create a list of all subsets of the NFA's states
-- 2. For each state in the DFA, for each symbol in the NFA's alphabet,
--    find the list of states reachable from the state with the symbol
--    and the list of states reachable from the state with the symbol and lambda transitions
--    it adds a transition from the state to the target state with the symbol
-- 3. Find the initial state of the DFA
-- 4. Find the accepting states of the DFA
nfa2dfa :: Automaton -> Automaton
nfa2dfa nfa =
  DFA
    { dstates = [0 .. (length subset - 1)], -- List of states for the DFA
      dalphabet = alphabet nfa, -- List of symbols for the DFA
      dtransitions = buildTransitions (transitions nfa) subset, -- List of transitions for the DFA
      dinitialState = fromJust $ elemIndex (lambdaTransitionClosure nfa (initialState nfa)) subset, -- Initial state for the DFA
      dfinalStates = findAcceptingStates subset (finalStates nfa) -- List of accepting states for the DFA
    }
  where
    subset = allSubsets (nub $ concatMap (\s -> lambdaTransitionClosure nfa s) (states nfa)) -- List of all subsets of the NFA's states
    buildTransitions :: [(State, Symbol, [State])] -> [[State]] -> [(State, Symbol, State)] -- List of transitions for the DFA
    buildTransitions transitions subset =
      -- List of transitions for the DFA
      [ (i, a, targetStateIndex) -- Transition from state i to state targetStateIndex with symbol a
        | (i, states) <- zip [0 ..] subset, -- For each state i in the DFA
          a <- alphabet nfa, -- For each symbol a in the NFA's alphabet
          let targetStates = nub $ concat [transition | state <- states, (s, sym, transition) <- transitions, s == state, sym == a], -- List of states reachable from state i with symbol a
          let lambdaTargetStates = nub $ concatMap (\s -> lambdaTransitionClosure nfa s) targetStates, -- List of states reachable from state i with symbol a and lambda transitions
          let targetStateIndex = fromJust $ elemIndex (sort lambdaTargetStates) (map sort subset) -- Index of the target state in the DFA
      ]
    findAcceptingStates :: [[State]] -> [State] -> [State] -- List of accepting states for the DFA
    findAcceptingStates subset acceptingStates = [i | (i, states) <- zip [0 ..] subset, not (null (states `intersect` acceptingStates))] -- List of accepting states for the DFA

-- allSubsets returns a list of all subsets of a list
-- It follows the steps:
-- 1. If the list is empty, return a list containing the empty list
-- 2. Otherwise, return the list concatenated with the list of each element of the list
-- It takes a list and returns a list of lists
allSubsets :: [State] -> [[State]]
allSubsets [] = [[]]
allSubsets (x : xs) = allSubsets xs ++ map (x :) (allSubsets xs)

-- lambdaTransitionClosure returns the lambda transition closure of a state
-- It follows the steps:
-- 1. If the state has no lambda transitions, return the state
-- 2. Otherwise, return the state concatenated with the lambda transition closure
--    of each state reachable from the state with a lambda transition
-- It takes an Automaton and a State and returns a list of States
lambdaTransitionClosure :: Automaton -> State -> [State]
lambdaTransitionClosure automaton state = lambdaClosureHelper automaton [state]
  where
    -- lambdaClosureHelper returns the lambda transition closure of a list of states
    -- It follows the steps:
    -- 1. If the list is empty, return the empty list
    -- 2. Otherwise, return the head of the list concatenated with the lambda transition closure
    --    of each state reachable from the head of the list with a lambda transition
    --    concatenated with the lambda transition closure of the tail of the list
    lambdaClosureHelper :: Automaton -> [State] -> [State]
    lambdaClosureHelper _ [] = []
    lambdaClosureHelper automaton (x : xs) =
      case lookup x (lambdaTransitions automaton) of
        Nothing -> x : lambdaClosureHelper automaton xs
        Just reachableStates -> x : lambdaClosureHelper automaton ((reachableStates ++ xs) \\ [x])
