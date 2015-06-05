{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Retrograde where {
import Data.List;
import Control.Monad(liftM);
import Data.Maybe;
import System.Random;
import Control.Monad.GenericReplicate;
mapReduce :: forall a key value b. Ord key => (a -> [(key,value)]) -> (key -> [(a,value)] -> [b]) -> [a] -> [b];
mapReduce mapfn redfn input = concatMap (uncurry redfn) $ shuffle $ do {
 x :: a <- input;
 y :: (key,value) <- mapfn x;
 return (x,y); -- all pairs
};

-- profiling reveals the majority of the computation time is spent here, not too surprising.
shuffle :: forall a key value. (Ord key) => [(a,(key,value))] -> [(key,[(a,value)])];
shuffle = let {
get_a :: (a,(key,value)) -> a;
get_a (a1,_)=a1;
get_key :: (a,(key,value)) -> key;
get_key (_,(k,_))=k;
get_value :: (a,(key,value)) -> value;
get_value (_,(_,v))=v;
rearrange :: [(a,(key,value))] -> (key,[(a,value)]);
rearrange l = (get_key $ head l, zip (map get_a l) (map get_value l));
} in map rearrange . groupBy (equating get_key) . sortOn get_key;

-- cf Data.Ord.comparing
equating :: Eq b => (a -> b) -> a -> a -> Bool;
equating f x y = (f x) == (f y);

{-
retrogrades :: Entry -> [Entry];
retrogrades (_,Nothing) = [];
retrogrades (_p, Just _v) = undefined;

type Entry = (Position, Maybe Value);
-}
newtype Value = Value Integer {-^ distance to conversion, negative is bad-}
deriving (Show,Eq);

instance Ord Value where {
compare (Value x) (Value 0) = compare x 0;
compare (Value 0) (Value y) = compare 0 y;
compare (Value x) (Value y) = if (x>0)&&(y>0)
then compare y x
else if (x<0)&&(y<0)
then compare y x
else compare x y;
};

loss :: Value;
loss = Value $ negate 1;

draw :: Value;
draw = Value 0;

backward :: Value -> Value;
backward (Value n) = negate_value $ Value $ case compare n 0 of {
EQ -> 0;
LT -> n-1;
GT -> n+1;
};

negate_value :: Value -> Value;
negate_value (Value n) = Value $ negate n;

combine_values_greedy :: [Maybe Value] -> Maybe Value;
combine_values_greedy mv = liftM backward $ let {
-- ^ There will always be at least one Just because retrograding from known values.
 the_best :: Value;
 the_best = minimum $ catMaybes mv;
} in if the_best < Value 0
then return the_best
-- ^We can greedily take the best value in the event of a win because
-- the best (fastest) wins come out of the algorithm first.
else if any isNothing mv then Nothing  -- unknown is better than losing
else return the_best;

data Epoch = Known | Unknown deriving (Show);

random_entry :: [a] -> IO a;
random_entry l = do {
xs :: [Float] <- genericReplicateM ((genericLength l)::Integer) randomIO;
return $ snd $ head $ sortOn fst $ zip xs l;
};

} --end
