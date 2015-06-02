module Main where {
import Chess;
main :: IO();
main = print $ and $ map (test_retro2 test_directory) $ all_positions test_directory;
}
