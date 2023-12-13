import AutomatonConversion (nfa2dfa)
import DFA (writeDFAToFile)
import DFAMinimization (dfaMinimization)
import NFA (constructNFA, lexer)

-- main reads a file, constructs an NFA, converts it to a DFA, minimizes it, and writes it to a file
main :: IO ()
main = do
  -- Prompt the user for the input file name
  putStrLn "Enter the name of the input file:"
  inputFileName <- getLine
  -- input takes the contents of the file specified by the user
  input <- readFile inputFileName
  -- lexs equals the result of applying the lexer to the input
  let lexs = lexer input
  -- nfa equals the result of applying constructNFA to lexs
  let nfa = constructNFA lexs
  -- dfa equals the result of applying nfa2dfa to nfa
  let dfa = nfa2dfa nfa
  -- dfamin equals the result of applying dfaMinimization to dfa
  let dfamin = dfaMinimization dfa
  -- this prints the minimized DFA
  print dfamin
  -- Remove the ".txt" extension from the input file name
  let baseFileName = takeWhile (/= '.') inputFileName
  -- Append "_output.txt" to the base file name
  let outputFileName = baseFileName ++ "_DFA.txt"
  -- this writes the minimized DFA to the output file
  writeDFAToFile dfamin outputFileName
  -- this prints the name of the output file
  print outputFileName
