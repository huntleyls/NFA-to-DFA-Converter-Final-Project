# NFA to DFA Converter in Haskell

## Introduction
This Haskell project provides a tool to convert Non-deterministic Finite Automatons (NFAs) into Deterministic Finite Automatons (DFAs). It's designed to be user-friendly and efficient, leveraging Haskell's functional programming capabilities for optimal performance.

## Features
- **Convert NFA to DFA:** Transforms any given NFA into its equivalent DFA.
- **Haskell Efficiency:** Utilizes Haskell's functional programming features for concise and effective processing.
- **Cabal Packaging:** Easy to build and install using Haskell's Cabal package manager.

## Installation
Ensure you have [Haskell](https://www.haskell.org/downloads/) and [Cabal](https://www.haskell.org/cabal/download.html) installed on your system. 

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/huntleyls/NFA-to-DFA-Converter-Final-Project.git
   cd NFA-to-DFA-Converter-Final-Project
   ```

2. **Build with Cabal:**
   ```bash
   cabal update
   cabal build
   ```

3. **Run with Cabal
   ```bash
   cabal run
   ```
## Usage 
once ran it will prompt you in the command line for a file 
```bash
Enter the name of the input file:
```
when that prompt appears you can input a file name 
```bash
test1.txt
```
it will respond with the output file
```
test1_DFA.txt
```
## Input Format
The input for the NFA should be specified as follows:
- **States:** A list of states in the automaton. Example: `0 1 2 3 4 5`
- **Alphabet:** The set of symbols in the automaton's alphabet. Example: `'a' 'b'`
- **Transitions:** A list of transitions, each specified by its starting state, input symbol, and resulting state. Example:  
  `0 a -> 1 3`  
  `1 a -> 2`  
  `1 b -> 3`  
  `2 b -> 4`  
  `3 a -> 4`  
  `4 b -> 0`
- **LamdaTransitions:** a list of transitions, each specified by its starting state, and resulting state. Example:  
  `0 -> 2`
- **Initial State:** The starting state of the automaton. Example: `3`  
- **Final States:** A list of accepting states. Example: `0 1 2`

## Output Format
The Specified output for the DFA will be as follow:
- **States:** A list of states in the DFA. Example: `0 1 2 3 4 5`
- **Alphabet:** The set of symbols in the DFA's alphabet. Example: `'a' 'b'`
- **Transitions:** A list of transitions for the DFA. Example:  
`2 b -> 3`  
`5 a -> 2`  
`4 a -> 0`  
`4 b -> 5`  
`0 b -> 1`  
`3 a -> 4`  
`3 b -> 2`  
`1 a -> 4`  
`1 b -> 1`  
- **Initial State:** The starting state of the DFA. Example: `3`
- **Final States:** A list of accepting states in the DFA. Example: `0 1 2`
  

  
