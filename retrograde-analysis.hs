{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import System.Environment(getArgs);
import Data.List;
import qualified Data.Tuple as Tuple;
import Data.Array.IArray;

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

type Position = Array Piecenum (Maybe Location);
newtype Location = Location Offset;
newtype Piecenum = Piecenum Integer deriving (Eq,Ord,Ix);

data Orthogonal = NoOrthogonal | Wazir | Rook deriving (Show);
data Diagonal = NoDiagonal | Ferz | Bishop deriving (Show);
data Knight = NoKnight | Knight deriving (Show);
data Alfil = NoAlfil | Alfil deriving (Show);
data Dabbaba = NoDabbaba | Dabbaba_single | Dabbaba_rider deriving (Show);
data Royal = Commoner | Royal deriving (Show);
data Color = White | Black deriving (Show);

data Piece = Piece Royal Orthogonal Diagonal Knight Alfil Dabbaba Color deriving (Show);

king :: Color -> Piece;
king = Piece Royal Wazir Ferz NoKnight NoAlfil NoDabbaba;

queen :: Color -> Piece;
queen = Piece Commoner Rook Bishop NoKnight NoAlfil NoDabbaba;

rook :: Color -> Piece;
rook = Piece Commoner Rook NoDiagonal NoKnight NoAlfil NoDabbaba;

maxpieces :: Piecenum;
maxpieces = Piecenum 4;

test :: Position;
test = array (Piecenum 0, Piecenum 1) [(Piecenum 0,Just $ Location (0,0)), (Piecenum 1,Just $ Location (7,7))];

type Offset = (Integer,Integer);

reflect45 :: Offset -> Offset;
reflect45 (x,y) = (y,x);

reflectx :: Offset -> Offset;
reflectx (x,y) = (x,negate y);

reflecty :: Offset -> Offset;
reflecty (x,y) = (negate x,y);

eightway_with_duplicates :: Offset -> [Offset];
eightway_with_duplicates z = do {
p <- [id,reflect45];
q <- [id,reflectx];
r <- [id, reflecty];
return $ r $ q $ p $ z;
};

eightway :: Offset -> [Offset];
eightway = nub . eightway_with_duplicates;

extend :: Offset -> [Offset];
extend (x,y) = do {
s <- enumFromTo 1 (unBoardsize board_size);
return (s*x,s*y);
};

newtype Boardsize = Boardsize Integer deriving (Show);

unBoardsize :: Boardsize -> Integer;
unBoardsize (Boardsize x) =x;

board_size :: Boardsize;
board_size = Boardsize 8;

moves :: Piece -> [Offset];
moves (Piece _ orth diag jknight jalfil jda _) = orthmoves orth
++ diagmoves diag
++ knightmoves jknight
++ alfilmoves jalfil
++ dabbabamoves jda;

orthmoves :: Orthogonal -> [Offset];
orthmoves NoOrthogonal = [];
orthmoves Wazir = eightway (1,0);
orthmoves Rook = concatMap extend $ orthmoves Wazir;

diagmoves :: Diagonal -> [Offset];
diagmoves NoDiagonal = [];
diagmoves Ferz = eightway (1,1);
diagmoves Bishop = concatMap extend $ diagmoves Ferz;

knightmoves :: Knight -> [Offset];
knightmoves NoKnight = [];
knightmoves Knight = eightway (1,2);

alfilmoves :: Alfil -> [Offset];
alfilmoves NoAlfil = [];
alfilmoves Alfil = eightway (2,2);

dabbabamoves :: Dabbaba -> [Offset];
dabbabamoves NoDabbaba = [];
dabbabamoves Dabbaba_single = eightway (2,0);
dabbabamoves Dabbaba_rider = concatMap extend $ dabbabamoves Dabbaba_single;

} --end
