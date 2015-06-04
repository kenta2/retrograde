{-# LANGUAGE ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving #-}
module Nim where {
import Data.List;
import Control.Monad;
import Control.Exception(assert);
import Debug.Trace;
import Retrograde;
import Data.Map(Map);
import qualified Data.Tuple as Tuple;
import qualified Data.Map as Map;

-- to avoid the redundancy warning
trace_placeholder :: ();
trace_placeholder = trace "trace" $ assert False ();

max_piles :: Piles;
max_piles = Piles 8;

max_coins :: Coins;
max_coins = Coins 7;

newtype Piles = Piles Integer deriving (Show, Ord, Eq);
newtype Coins = Coins Integer deriving (Show, Ord, Eq, Enum);

unCoins :: Coins -> Integer;
unCoins (Coins i)=i;

type Position = [Coins];

uniq :: (Ord a, Eq a) => [a] -> [a];
uniq = map head . group . sort;

final_entries :: [(Position,Value)];
final_entries = [([],loss)];

retrograde_positions :: Position -> [Position];
retrograde_positions p = assert (is_canonical p) $ canonicalize $ new_pile p ++ add_to_some_pile p;

new_pile :: Position -> [Position];
new_pile l = if Piles (genericLength l) < max_piles then do {
new <- enumFromTo (Coins 1) max_coins;
return $ new:l
} else [];

is_canonical :: Position -> Bool;
is_canonical [] = True;
is_canonical l@(Coins n:_) = if n<1 then False
else l==sort l;

canonicalize1 :: Position -> Position;
canonicalize1 = dropWhile (\n -> n == Coins 0) . sort;

canonicalize :: [Position] -> [Position];
canonicalize = uniq . map canonicalize1;

add_to_some_pile :: Position -> [Position];
add_to_some_pile = modify_some_pile $ \h -> enumFromTo (succ h) max_coins;

successors :: Position -> [Position];
successors = canonicalize . (modify_some_pile $ \h -> enumFromTo (Coins 0) (pred h));

modify_some_pile :: (Coins -> [Coins]) -> Position -> [Position];
modify_some_pile f l = assert (is_canonical l) $ do {
(p,q) <- zip (inits l) (tails l);
guard $ not $ null q;
let {h = head q};
newh <- f h;
return $ p ++ (newh:tail q);
};

value_via_successors :: Position -> [Entry] -> Maybe Value;
value_via_successors l succs = let {
 table :: Map Position Value;
 table = Map.fromList succs;
} in combine_values_greedy $ map ((flip Map.lookup) table) $ successors l;

type Entry = (Position, Value);

mapfn :: Entry -> [(Position, Epoch)];
mapfn (pos, _val) = (pos,Known):(map (\x -> (x, Unknown)) $ retrograde_positions pos);

redfn :: Position -> [(Entry, Epoch)] -> [Entry];
redfn pos esuccs = if any (\case {(_,Known) -> True; _->False}) esuccs
then []
else case value_via_successors pos (map fst esuccs) of {
Nothing -> [];
Just v -> [(pos,v)];
};

do_mapreduce :: [Entry] -> [Entry];
do_mapreduce = mapReduce mapfn redfn;

iterate_mapreduce :: [Entry] -> [[Entry]];
iterate_mapreduce start = let {
more = do_mapreduce start;
} in if null more then []
else more:iterate_mapreduce (start ++ more);

binary :: Integer -> [Bool];
binary = map (\x -> x==1) . (unfoldr $ \v -> if v==0 then Nothing else Just $ Tuple.swap $ divMod v 2);

bin_add :: [Bool] -> [Bool] -> [Bool];
bin_add x [] = x;
bin_add [] x = x;
bin_add (p:p2) (q:q2) = xor p q : bin_add p2 q2;

xor :: Bool -> Bool -> Bool;
xor = (/=);

nim_bsum :: [[Bool]] -> [Bool];
nim_bsum = foldl' bin_add [];

nim_sum :: [Coins] -> [Bool];
nim_sum = nim_bsum . map (binary . unCoins);

is_zero :: [Bool] -> Bool;
is_zero = not . any id;

test_item :: Position -> Value -> Bool;
test_item p v = (is_zero $ nim_sum p) == (v < draw);

all_answers :: [Entry];
all_answers = concat $ iterate_mapreduce final_entries;

all_test :: (Int,Bool);
all_test = (length all_answers, and $ map (uncurry test_item) all_answers);

lookup_answer :: Map Position Value;
lookup_answer = Map.fromList all_answers;

} --end
