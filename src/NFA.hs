module NFA (lexer, constructNFA) where

-- Module: NFA
import AutomatonDefinitions (Automaton (..), LexedAutomatonParts (..), State, Symbol, Transition)
-- isSpace is used to check if a character is a whitespace character
import Data.Char (isSpace)
-- dropWhileEnd is used to drop all trailing whitespace characters and isPrefixOf is used to check if a string is a prefix of another string
import Data.List (dropWhileEnd, isPrefixOf)
-- readMaybe is used to parse a string to a Maybe Int
import Text.Read (readMaybe)

-- lexer is a function that takes a string and returns a list of LexedAutomatonParts
-- It follows the following steps:
-- 1. Splits the string into lines using `lines`.
-- 2. Finds the states from the list of lines using `parseStates`.
-- 3. Finds the alphabet from the list of lines using `parseAlphabet`.
-- 4. Finds the transitions from the list of lines using `parseTransitions`.
-- 5. Finds the lambda transitions from the list of lines using `parseLambdaTransitions`.
-- 6. Finds the initial state from the list of lines using `parseInitialState`.
-- 7. Finds the final states from the list of lines using `parseFinalStates`.
-- 8. Constructs a list of LexedAutomatonParts from the states, alphabet, transitions, lambda transitions, initial state and final states.
-- The resulting list of LexedAutomatonParts represents the lexed automaton parts.
lexer :: String -> [LexedAutomatonParts]
lexer input =
  let lines' = lines input
      states = parseStates $ headOrDefault "" $ filter (isPrefixOf "States:") lines'
      alphabet = parseAlphabet $ headOrDefault "" $ filter (isPrefixOf "Alphabet:") lines'
      transitionLines = takeWhile (not . isPrefixOf "Lambda Transitions:") $ dropWhile (not . isPrefixOf "  ") lines'
      transitions = parseTransitions transitionLines
      lambdaTransitionsLines = tail $ dropWhile (not . isPrefixOf "Lambda Transitions:") lines'
      lambdaTransitions = parseLambdaTransitions $ filter (isPrefixOf "  ") lambdaTransitionsLines
      initialState = parseInitialState $ headOrDefault "" $ filter (isPrefixOf "Initial State:") lines'
      finalStates = parseFinalStates $ headOrDefault "" $ filter (isPrefixOf "Final States:") lines'
   in [StatesLex states, AlphabetLex alphabet, TransitionsLex transitions, LambdaTransitionsLex lambdaTransitions, InitialStateLex initialState, FinalStatesLex finalStates]

-- The `parseStates` function takes a string `str` as input and parses it to extract a list of states.
-- It follows the following steps:
-- 1. Drops the characters until the first occurrence of ':' using `dropWhile (/= ':')`.
-- 2. Drops the first character after ':' using `drop 1`.
-- 3. Splits the remaining string into words using `words`.
-- 4. Converts each word to an integer using `map read`.
-- The resulting list of integers represents the parsed states.
parseStates :: String -> [State]
parseStates str = map read . words . drop 1 . dropWhile (/= ':') $ str

-- The `parseAlphabet` function takes a string `str` as input and parses it to extract a list of symbols.
-- It follows the following steps:
-- 1. Drops the characters until the first occurrence of ':' using `dropWhile (/= ':')`.
-- 2. Drops the first character after ':' using `drop 1`.
-- 3. Splits the remaining string into words using `words`.
-- 4. Takes the first character of each word using `map head`.
-- The resulting list of characters represents the parsed symbols.
parseAlphabet :: String -> [Symbol]
parseAlphabet str = map head . words . drop 1 . dropWhile (/= ':') $ str

-- The `parseTransition` function takes a string `str` as input and parses it to extract a transition.
-- It follows the following steps:
-- 1. Splits the string into two parts using `splitOn "->"`.
-- 2. The first part is the state and symbol pair. It is split into words using `words`.
-- 3. The first word is the state and the second word is the symbol.
-- 4. The second part is the destination states. It is split into words using `words`.
-- 5. Each word is converted to an integer using `map read`.
-- The resulting tuple represents the parsed transition.
parseTransition :: String -> Transition
parseTransition str =
  let [stateSymbol, destStatesRaw] = splitOn "->" str
      stateSymbolWords = words stateSymbol
      state = case stateSymbolWords of
        (stateStr : symbolStr : []) -> maybe (error $ "Invalid state: " ++ stateStr) id (readMaybe stateStr)
        _ -> error "Invalid format for state and symbol"
      symbol = case stateSymbolWords of
        (stateStr : symbolStr : []) -> head . trim $ symbolStr
        _ -> error "Invalid format for state and symbol"
      trimmedDestStatesRaw = trim . dropWhile (== '>') $ destStatesRaw
      destStates = map (maybe (error $ "Invalid destination state in: " ++ trimmedDestStatesRaw) id . readMaybe . trim) (words trimmedDestStatesRaw)
   in (state, symbol, destStates)

-- The `parseTransitions` function takes a list of strings `strs` as input and parses it to extract a list of transitions.
-- It follows the following steps:
-- 1. Maps `parseTransition` over the list of strings.
-- The resulting list of tuples represents the parsed transitions.
parseTransitions :: [String] -> [Transition]
parseTransitions = map parseTransition

-- The `parseLambdaTransition` function takes a string `str` as input and parses it to extract a lambda transition.
-- It follows the following steps:
-- 1. Replaces all occurrences of '-' and '>' with ' ' using `map (\c -> if c == '-' || c == '>' then ' ' else c)`.
-- 2. Splits the string into words using `words`.
-- 3. Converts each word to an integer using `mapM readMaybe`.
-- 4. The first word is the state and the remaining words are the destination states.
-- The resulting tuple represents the parsed lambda transition.
-- Note: `mapM readMaybe` returns `Just [Int]` if all the words can be converted to integers and `Nothing` otherwise.
parseLambdaTransition :: String -> (State, [State])
parseLambdaTransition str =
  let str' = map (\c -> if c == '-' || c == '>' then ' ' else c) str
      words' = words str'
   in case mapM readMaybe words' of
        Just (state : destStates) -> (state, destStates)
        _ -> error $ "Lambda transition parsing error: " ++ str

-- The `parseLambdaTransitions` function takes a list of strings `strs` as input and parses it to extract a list of lambda transitions.
-- It follows the following steps:
-- 1. Maps `parseLambdaTransition` over the list of strings.
-- The resulting list of tuples represents the parsed lambda transitions.
parseLambdaTransitions :: [String] -> [(State, [State])]
parseLambdaTransitions = map parseLambdaTransition

-- The `parseInitialState` function takes a string `str` as input and parses it to extract the initial state.
-- It follows the following steps:
-- 1. Drops the characters until the first occurrence of ':' using `dropWhile (/= ':')`.
-- 2. Drops the first character after ':' using `drop 1`.
-- 3. Trims the remaining string using `trim`.
-- 4. Converts the string to an integer using `read`.
-- The resulting integer represents the parsed initial state.
parseInitialState :: String -> State
parseInitialState = read . trim . last . words

-- The `parseFinalStates` function takes a string `str` as input and parses it to extract a list of final states.
-- It follows the following steps:
-- 1. Splits the string into words using `words`.
-- 2. Extracts the last word using `last`.
-- 3. Trims any leading or trailing whitespace from the last word using `trim`.
-- 4. Converts each word to a state using `map read`.
-- The resulting list of states represents the parsed final states.
parseFinalStates :: String -> [State]
parseFinalStates = map read . words . trim . last . words

-- The `splitOn` function takes a delimiter `delimiter` and a string `str` as input and splits the string on the delimiter.
-- It follows the following steps:
-- 1. Folds over the string using `foldr`.
-- 2. If the current character and the first character of the accumulator is the delimiter, returns a new accumulator with an empty string as the first element.
-- 3. Otherwise, returns a new accumulator with the current character appended to the first element of the accumulator.
-- The resulting list of strings represents the split string.
splitOn :: String -> String -> [String]
splitOn delimiter = foldr f [""]
  where
    f char acc@(x : xs)
      | take (length delimiter) (char : x) == delimiter = "" : acc
      | otherwise = (char : x) : xs
    f _ _ = error "Unexpected pattern in foldr for splitOn"

-- The `trim` function takes a string `str` as input and trims it by dropping all leading and trailing whitespace characters.
-- It follows the following steps:
-- 1. Drops all leading whitespace characters using `dropWhile isSpace`.
-- 2. Drops all trailing whitespace characters using `dropWhileEnd isSpace`.
-- The resulting string represents the trimmed string.
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- The `headOrDefault` function takes a default value `def` and a list `xs` as input and returns the head of the list if it is non-empty and the default value otherwise.
-- It follows the following steps:
-- 1. Checks if the list is empty using `null`.
-- 2. If the list is empty, returns the default value.
-- 3. Otherwise, returns the head of the list.
-- The resulting value represents the head of the list if it is non-empty and the default value otherwise.
headOrDefault :: a -> [a] -> a
headOrDefault def xs = if null xs then def else head xs

-- The `constructNFA` function takes a list of LexedAutomatonParts `lexs` as input and constructs an NFA from it.
-- It follows the following steps:
-- 1. Finds the states from the list of LexedAutomatonParts using `findLexedAutomatonParts`.
-- 2. Finds the alphabet from the list of LexedAutomatonParts using `findLexedAutomatonParts`.
-- 3. Finds the transitions from the list of LexedAutomatonParts using `findLexedAutomatonParts`.
-- 4. Finds the lambda transitions from the list of LexedAutomatonParts using `findLexedAutomatonParts`.
-- 5. Finds the initial state from the list of LexedAutomatonParts using `findLexedAutomatonParts`.
-- 6. Finds the final states from the list of LexedAutomatonParts using `findLexedAutomatonParts`.
-- 7. Constructs an NFA from the states, alphabet, transitions, lambda transitions, initial state and final states.
-- The resulting NFA represents the constructed NFA.
constructNFA :: [LexedAutomatonParts] -> Automaton
constructNFA lexs =
  let states = case findLexedAutomatonParts (StatesLex []) lexs of
        StatesLex state -> state
        _ -> error "States not found"
      alphabet = case findLexedAutomatonParts (AlphabetLex []) lexs of
        AlphabetLex alphabet -> alphabet
        _ -> error "Alphabet not found"
      transitions = case findLexedAutomatonParts (TransitionsLex []) lexs of
        TransitionsLex transitions -> transitions
        _ -> error "Transitions not found"
      lambdaTransitions = case findLexedAutomatonParts (LambdaTransitionsLex []) lexs of
        LambdaTransitionsLex lambdaTransitions -> lambdaTransitions
        _ -> error "Lambda transitions not found"
      initialState = case findLexedAutomatonParts (InitialStateLex 0) lexs of
        InitialStateLex initialstate -> initialstate
        _ -> error "Initial state not found"
      finalStates = case findLexedAutomatonParts (FinalStatesLex []) lexs of
        FinalStatesLex finalstates -> finalstates
        _ -> error "Final states not found"
   in NFA states alphabet transitions lambdaTransitions initialState finalStates

-- The `findLexedAutomatonParts` function takes a LexedAutomatonParts `lexemeType` and a list of LexedAutomatonParts `lexs` as input and returns the first LexedAutomatonParts in the list that matches the given LexedAutomatonParts.
-- It follows the following steps:
-- 1. Filters the list of LexedAutomatonParts to find the first LexedAutomatonParts that matches the given LexedAutomatonParts.
-- 2. Returns the first LexedAutomatonParts in the filtered list.
-- The resulting LexedAutomatonParts represents the first LexedAutomatonParts in the list that matches the given LexedAutomatonParts.
-- Note: The function assumes that there is at most one LexedAutomatonParts in the list that matches the given LexedAutomatonParts.
-- If there are multiple LexedAutomatonParts in the list that match the given LexedAutomatonParts, the function returns the first one.
findLexedAutomatonParts :: LexedAutomatonParts -> [LexedAutomatonParts] -> LexedAutomatonParts
findLexedAutomatonParts lexemeType =
  head
    . filter
      ( \x -> case (lexemeType, x) of
          (StatesLex _, StatesLex _) -> True
          (AlphabetLex _, AlphabetLex _) -> True
          (TransitionsLex _, TransitionsLex _) -> True
          (LambdaTransitionsLex _, LambdaTransitionsLex _) -> True
          (InitialStateLex _, InitialStateLex _) -> True
          (FinalStatesLex _, FinalStatesLex _) -> True
          _ -> False
      )
