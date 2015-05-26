{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main where {
import System.Environment(getArgs);
import Data.List;
import Control.Monad;
import Data.Maybe;
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
newtype Location = Location Offset deriving (Eq, Ord, Ix, Show);
newtype Piecenum = Piecenum Integer deriving (Eq,Ord,Ix);

data Orthogonal = NoOrthogonal | Wazir | Rook deriving (Show);
data Diagonal = NoDiagonal | Ferz | Bishop deriving (Show);
data Knight = NoKnight | Knight deriving (Show);
data Alfil = NoAlfil | Alfil deriving (Show);
data Dabbaba = NoDabbaba | Dabbaba_single | Dabbaba_rider deriving (Show);
data Royal = Commoner | Royal deriving (Show);
data Color = White | Black deriving (Show, Eq);

data Piece = Piece Royal Orthogonal Diagonal Knight Alfil Dabbaba Color deriving (Show);

king :: Color -> Piece;
king = Piece Royal Wazir Ferz NoKnight NoAlfil NoDabbaba;

queen :: Color -> Piece;
queen = Piece Commoner Rook Bishop NoKnight NoAlfil NoDabbaba;

rook :: Color -> Piece;
rook = Piece Commoner Rook NoDiagonal NoKnight NoAlfil NoDabbaba;

knight :: Color -> Piece;
knight = Piece Commoner NoOrthogonal NoDiagonal Knight NoAlfil NoDabbaba;

maxpieces :: Piecenum;
maxpieces = Piecenum 4;

test :: Position;
test = array (Piecenum 0, Piecenum 1) [(Piecenum 0,Just $ Location (0,0)), (Piecenum 1,Just $ Location (7,7))];

test_directory :: Directory;
test_directory = listArray (Piecenum 0, Piecenum 1) [queen White, rook Black];

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

moves :: Directory -> Position -> Piecenum -> [Location];
moves directory position num = case position ! num of {
Nothing -> [];
Just mylocation -> let {
andBundle :: (Location -> Position -> [Location]) -> [Location];
andBundle f = f mylocation position;
} in nub $ filter (inRange board_bounds) $ case (directory ! num) of {
Piece _roy orth diag jknight jalfil jda color -> filter (not . stomp directory position color)
$ concatMap andBundle
[orthmoves orth
,diagmoves diag
,knightmoves jknight
,alfilmoves jalfil
,dabbabamoves jda]
}};

orthmoves :: Orthogonal -> Location -> Position -> [Location];
orthmoves NoOrthogonal _ _ = [];
orthmoves Wazir me _ = map (add_offset me) $ eightway (1,0);
orthmoves Rook me pos = concatMap (extendUntilOccupied me pos) $ eightway (1,0);

diagmoves :: Diagonal -> Location -> Position -> [Location];
diagmoves NoDiagonal _ _ = [];
diagmoves Ferz me _ = map (add_offset me) $ eightway (1,1);
diagmoves Bishop me pos = concatMap (extendUntilOccupied me pos) $ eightway (1,1);

knightmoves :: Knight -> Location -> Position -> [Location];
knightmoves NoKnight _ _ = [];
knightmoves Knight me _ = map (add_offset me) $ eightway (1,2);

alfilmoves :: Alfil -> Location -> Position -> [Location];
alfilmoves NoAlfil _ _ = [];
alfilmoves Alfil me _ = map (add_offset me) $ eightway (2,2);

dabbabamoves :: Dabbaba -> Location -> Position -> [Location];
dabbabamoves NoDabbaba _ _ = [];
dabbabamoves Dabbaba_single me _ = map (add_offset me) $ eightway (2,0);
dabbabamoves Dabbaba_rider me pos = concatMap (extendUntilOccupied me pos) $ eightway (2,0);

board_bounds :: (Location,Location);
board_bounds = (Location (0,0),Location (pred $ unBoardsize board_size, pred $ unBoardsize board_size));

empty :: Position -> Location -> Bool;
empty p l = inRange board_bounds l
&& (isNothing $ atPosition p l);

type Directory = Array Piecenum Piece;

add_offset :: Location -> Offset -> Location;
add_offset (Location (ox,oy)) (dx,dy) = Location (ox+dx,oy+dy);

take_including_first_failure :: (a -> Bool) -> [a] -> [a];
take_including_first_failure p l = case span p l of {
(l2,[]) -> l2;
(aa,zz) -> aa ++ [head zz]};

-- take_including_first_failure allows captures
extendUntilOccupied :: Location -> Position -> Offset -> [Location];
extendUntilOccupied me pos off = take_including_first_failure (empty pos) $ map (add_offset me) $ extend off;

-- trying to capture one's own piece
stomp :: Directory -> Position -> Color -> Location -> Bool;
stomp directory p mycolor mylocation = case atPosition p mylocation of {
Nothing -> False;
Just num -> case directory ! num of {
Piece _ _ _ _ _ _ othercol -> mycolor == othercol
}};

atPosition :: Position -> Location -> Maybe Piecenum;
atPosition pos loc = let {
check1 :: (Piecenum, Maybe Location) -> [Piecenum];
check1 (_,Nothing) = mzero;
check1 (num, Just loc2) = if loc==loc2 then return num else mzero;
} in case concatMap check1 $ assocs pos of {
[] -> Nothing;
[x] -> Just x;
_ -> error "multiple occupancy"
};




} --end
