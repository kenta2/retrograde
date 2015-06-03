{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Retrograde where {
import Data.List;
import Control.Monad(liftM);
import Data.Maybe;
import System.Random;
import Control.Monad.GenericReplicate;
mapReduce :: forall a key value b. Ord key => (a -> [(key,value)]) -> (key -> [(a,value)] -> [b]) -> [a] -> [b];
mapReduce mapfn redfn l = concatMap (uncurry redfn) $ shuffle $ do {
 x :: a <- l;
 y :: (key,value) <- mapfn x;
 return (x,y);
};

shuffle :: forall a key value. (Ord key) => [(a,(key,value))] -> [(key,[(a,value)])];
shuffle = let {
get_a :: (a,(key,value)) -> a;
get_a (a1,_)=a1;
get_key :: (a,(key,value)) -> key;
get_key (_,(k,_))=k;
get_value :: (a,(key,value)) -> value;
get_value (_,(_,v))=v;
rearrange :: [(a,(key,value))] -> (key,[(a,value)]);
rearrange l4 = (get_key $ head l4, zip (map get_a l4) (map get_value l4));
} in map rearrange . groupBy (eq_ing get_key) . sortOn get_key;

-- cf Data.Ord.comparing
eq_ing :: Eq b => (a -> b) -> a -> a -> Bool;
eq_ing f x y = (f x) == (f y);

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

-- | Maximizing or Minimizing the Value of a position
data Color = Biggerizer | Smallerizer deriving (Show, Eq, Ord);
other :: Color -> Color;
other Biggerizer = Smallerizer;
other Smallerizer = Biggerizer;

loss :: Color -> Value;
loss Biggerizer = Value $ negate 1;
loss Smallerizer = Value 1;

backward :: Value -> Value;
backward (Value n) = Value $ case compare n 0 of {
EQ -> 0;
LT -> n-1;
GT -> n+1;
};

negate_value :: Value -> Value;
negate_value (Value n) = Value $ negate n;

combine_values :: Color -> [Maybe Value] -> Maybe Value;
combine_values color mv = liftM backward $ let {
-- ^ There will always be at least one Just because retrograding from known values.
the_best = best color $ catMaybes mv;
} in if has_win color the_best
then return the_best
else if any isNothing mv then Nothing  -- unknown is better than losing
else return the_best;

-- We can greedily take the best value in the event of a win because
-- the best (fastest) wins come out of the algorithm first.

best :: Color -> [Value] -> Value;
best Biggerizer = maximum;
best Smallerizer = minimum;

has_win :: Color -> Value -> Bool;
has_win Biggerizer v = v>Value 0;
has_win Smallerizer v = v<Value 0;

data Epoch = Known | Unknown deriving (Show);

random_entry :: [a] -> IO a;
random_entry l = do {
xs :: [Float] <- genericReplicateM ((genericLength l)::Integer) randomIO;
return $ snd $ head $ sortOn fst $ zip xs l;
};

combine_nonpartizan_values :: [Maybe Value] -> Maybe Value;
combine_nonpartizan_values mv = if any isNothing mv then Nothing
else Just $ negate_value $ backward $ minimum $ map fromJust mv;

} --end
