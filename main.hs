{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import Chess;
import System.Environment(getArgs);
import System.IO;
-- import Retrograde;

silly :: IO [String];
silly = getArgs;

main :: IO();
-- main = print $ and $ map (test_retro2 test_directory) $ all_positions test_directory;
-- boardsize 4 = 22 sec
-- boardsize 5 = 3m27s

-- main = do {print $ length $ all_positions  test_directory ; mapM_ (print.length) $ until_fixed $ iterate gens gen_0 } ;
-- boardsize 4 = 16 min

-- main = do {let {l = iterate_mapreduce gen_0;};mapM_ (\l2 -> random_entry l2 >>= putStrLn . show_entry test_directory) l;};
--boardsize 5 = 289 min on mkc, added 16gb swap just in case. longest mate was 33.

--main = eval_iterate;
-- board_size 4 = 23 minutes with stalemate

-- main = print $ length $ piece_set2 4 [];
-- size 2 = 11664
-- size 3 = 3790800
-- size 4 = 629236836 -- 4 minutes
-- after flip_color
-- 2 = 5886
-- 3 = 1895400
-- 4 = 314627193 -- 19 minutes, after assert sorted
-- omitting Dabbaba_rider
-- 2 = 2628
-- 3 = 562464
-- 4 = 62432010

-- main = show_longest;

-- main = three_pieces_length_check;

-- main = getArgs >>= try_three_pieces . read . head;
main = do {
 hSetBuffering stdout LineBuffering;
 putStrLn "start";
 -- rand_three_pieces;
 -- print $ length $ three_pieces();
 getArgs >>= try_three_pieces . read . head;
};
}
