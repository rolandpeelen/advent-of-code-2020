import Prelude

main :: IO ()
main = do
  input <- getContents
  (putStr . show . fn . lines) input

filterChars :: String -> String -> String
filterChars xs = filter (not . (`elem` xs))

-- While we can parse (-20) as a number, we can't parse (+20). So let's
-- filter them out first, using the helper above, then we can simply
-- sum the numbers to get the answer.
fn :: [String] -> Int
fn = sum . map (read . filterChars "+")
