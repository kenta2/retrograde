{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import System.Environment(getArgs);
import Data.List;
import qualified Data.Tuple as Tuple;

main :: IO();
main = getArgs >>= \case{
_ -> undefined;
};

mapReduce :: forall a b c. Ord b => (a -> [b]) -> (b -> [a] -> c) -> [a] -> [c];
mapReduce mapfn redfn l = map (uncurry redfn) $ group2nd $ do {
 x :: a <- l;
 y :: b <- mapfn x;
 return (x,y);
};

group2nd :: (Ord b) => [(a,b)] -> [(b,[a])];
group2nd = map (\l -> (fst $ head l, map snd l)) . groupBy (eq_ing fst) . sortOn fst . map Tuple.swap;

-- cf cData.Ord.comparing
eq_ing :: Eq b => (a -> b) -> a -> a -> Bool;
eq_ing f x y = (f x) == (f y);

data Position = Position;

retrogrades :: Entry -> [Entry];
retrogrades (_,Nothing) = [];
retrogrades (_p, Just _v) = undefined;

type Entry = (Position, Maybe Value);

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

backward :: Value -> Value;
backward (Value n) = Value $ case compare n 0 of {
EQ -> 0;
LT -> (negate n) +1;
GT -> (negate n) -1;
};

} --end
