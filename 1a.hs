import Prelude

main :: IO ()
main = do
  -- Get the inputs
  input <- getContents

  -- right to left:
  -- take the input and push it through the composition of:
  -- 1. split input by line (returning [String])
  -- 2. map read function over strings to turn them into integers (returning [Integer])
  -- 3. call 'someFunc' over that list
  -- 4. print the return of someFunc
  (putStr . someFunc . map read . lines) input

  -- Print the input back
  putStr input


-- The function to run everything through
someFunc :: [Integer] -> String
someFunc _ = "Hello"
