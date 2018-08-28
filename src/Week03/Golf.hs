module Week03.Golf
  ( skips
  , localMaxima
  , histogram
  , histogramGen
  ) where

skips :: [a] -> [[a]]
skips xs = map (`everyNth` xs) [1..(length xs)]
  where everyNth n = map snd . filter ((==0) . (`mod` n) . fst) . zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima = map (\(_, x, _) -> x) . filter (\(x, y, z) -> y > x && y > z) . triples
  where
    triples xs = zip3 xs (drop 1 xs) (drop 2 xs)

-- general version that allows for any domain (between 0 and 9), not just 0..9
-- e.g. putStr (histogramGen [0..3] [0,0,1,2,2,2,3])
histogramGen :: [Integer] -> [Integer] -> String
histogramGen domain xs =
  let
    freq n = length . filter (== n)
    freqs = map (`freq` xs) domain
    buildRow n = map (\f -> if f >= n then '*' else ' ') freqs
    rows = map buildRow (reverse [1..(maximum freqs)])
    separator = replicate (length domain) '='
    legend = concatMap show domain
  in
    unlines (rows ++ [separator, legend])

histogram :: [Integer] -> String
histogram = histogramGen [0..9]
