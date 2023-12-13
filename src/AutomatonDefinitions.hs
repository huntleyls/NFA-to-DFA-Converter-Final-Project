-- Module: AutomatonDefinitions
module AutomatonDefinitions (State, Symbol, Partition, TransitionTable, Transition, DTransition, Automaton (..), LexedAutomatonParts (..)) where

-- State is an Int
type State = Int

-- Symbol is a Char
type Symbol = Char

-- Partition is a list of lists of states
type Partition = [[State]]

-- TransitionTable is a list of tuples of (state, symbol) and state
type TransitionTable = [((State, Symbol), State)]

-- Transition is a tuple of (state, symbol, [state])
type Transition = (State, Symbol, [State])

-- DTransition is a tuple of (state, symbol, state)
type DTransition = (State, Symbol, State)

-- Automaton is either an NFA or a DFA
data Automaton
  = -- NFA is a tuple of (states, alphabet, transitions, lambdaTransitions, initialState, finalStates)
    NFA
      { states :: [State],
        alphabet :: [Symbol],
        transitions :: [Transition],
        lambdaTransitions :: [(State, [State])],
        initialState :: State,
        finalStates :: [State]
      }
  | -- DFA is a tuple of (states, alphabet, transitions, initialState, finalStates)
    DFA
      { dstates :: [State],
        dalphabet :: [Symbol],
        dtransitions :: [DTransition],
        dinitialState :: State,
        dfinalStates :: [State]
      }
  deriving (Show, Eq)

-- LexedAutomatonParts is a data type that represents the different parts of an automaton
data LexedAutomatonParts
  = -- StatesLex is a list of states
    StatesLex [State]
  | -- AlphabetLex is a list of symbols
    AlphabetLex [Symbol]
  | -- TransitionsLex is a list of transitions
    TransitionsLex [Transition]
  | -- LambdaTransitionsLex is a list of lambda transitions
    LambdaTransitionsLex [(State, [State])]
  | -- InitialStateLex is a state
    InitialStateLex State
  | -- FinalStatesLex is a list of states
    FinalStatesLex [State]
  deriving (Show, Eq)
