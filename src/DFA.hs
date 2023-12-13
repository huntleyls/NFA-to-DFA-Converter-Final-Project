module DFA (writeDFAToFile) where

import AutomatonDefinitions (Automaton (..))
import System.IO (writeFile)

-- convertDFA converts a DFA to a string
-- It takes an Automaton and returns a string
-- It follows the steps:
-- 1. Convert the states to a string
-- 2. Convert the alphabet to a string
-- 3. Convert the transitions to a string
-- 4. Convert the initial state to a string
-- 5. Convert the accepting states to a string
-- 6. Return the string
convertDFA :: Automaton -> String
convertDFA (DFA states alphabet transitions startState acceptStates) =
  "States: "
    ++ unwords (map show states)
    ++ "\n"
    ++ "Alphabet: "
    ++ unwords (map show alphabet)
    ++ "\n"
    ++ "Transitions:\n"
    ++ unlines (map showTransition transitions)
    ++ "Initial State: "
    ++ show startState
    ++ "\n"
    ++ "Final States: "
    ++ unwords (map show acceptStates)
  where
    showTransition (from, sym, to) = "  " ++ show from ++ " " ++ [sym] ++ " -> " ++ show to

-- writeDFAToFile writes a DFA to a file
-- it follows the steps:
-- 1. Convert the DFA to a string
-- 2. Write the string to a file
writeDFAToFile :: Automaton -> FilePath -> IO ()
writeDFAToFile dfa filePath = do
  let dfaString = convertDFA dfa
  writeFile filePath dfaString