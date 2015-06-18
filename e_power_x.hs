solve :: Double -> Double
solve x = sum (take 10 [(x)**n/(product [1..n]) | n <- [0..] ])-- Insert your code here --

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words

