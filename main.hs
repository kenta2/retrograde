module Main where {
import Chess;
import Retrograde;
main :: IO();
-- main = print $ and $ map (test_retro2 test_directory) $ all_positions test_directory;
-- boardsize 4 = 22 sec
-- boardsize 5 = 3m27s

-- main = do {print $ length $ all_positions  test_directory ; mapM_ (print.length) $ until_fixed $ iterate gens gen_0 } ;
-- boardsize 4 = 16 min

main = do {
let {l = iterate_mapreduce gen_0;};
mapM_ (\l2 -> random_entry l2 >>= putStrLn . show_entry test_directory) l;
};
--boardsize 5 = 289 min on mkc, added 16gb swap just in case. longest mate was 33.
}
