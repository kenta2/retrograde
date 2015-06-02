module Main where {
import Chess;
main :: IO();
-- main = print $ and $ map (test_retro2 test_directory) $ all_positions test_directory;
-- boardsize 4 = 22 sec
-- boardsize 5 = 3m27s

main = do {print $ length $ all_positions  test_directory ; mapM_ (print.length) $ until_fixed $ iterate gens gen_0 } ;
-- boardsize 4 = 16 min
}
