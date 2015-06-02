{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Chess where {
import Data.List;
import Control.Monad;
import Data.Maybe;
import Data.Array.IArray;
import Debug.Trace;
import Retrograde;
import Data.Map(Map);
import qualified Data.Map as Map;
import qualified Data.Set as Set;
import Data.Set(Set);
-- import Control.Monad.GenericReplicate;
import Control.Exception(assert);

board_size :: Boardsize;
board_size = Boardsize 5;

-- to avoid the redundancy warning
trace_placeholder :: ();
trace_placeholder = trace "trace" ();

type Position = Array Piecenum (Maybe Location);
newtype Location = Location Offset deriving (Eq, Ord, Ix, Show);
newtype Piecenum = Piecenum Integer deriving (Eq, Ord, Ix, Show);

data Orthogonal = NoOrthogonal | Wazir | Rook deriving (Show);
data Diagonal = NoDiagonal | Ferz | Bishop deriving (Show);
data Knight = NoKnight | Knight deriving (Show);
data Alfil = NoAlfil | Alfil deriving (Show);
data Dabbaba = NoDabbaba | Dabbaba_single | Dabbaba_rider deriving (Show);
data Royal = Commoner | Royal deriving (Show);


data Piece = Piece Royal Orthogonal Diagonal Knight Alfil Dabbaba Color deriving (Show);

king :: Color -> Piece;
king = Piece Royal Wazir Ferz NoKnight NoAlfil NoDabbaba;

queen :: Color -> Piece;
queen = Piece Commoner Rook Bishop NoKnight NoAlfil NoDabbaba;

rook :: Color -> Piece;
rook = Piece Commoner Rook NoDiagonal NoKnight NoAlfil NoDabbaba;

knight :: Color -> Piece;
knight = Piece Commoner NoOrthogonal NoDiagonal Knight NoAlfil NoDabbaba;

test_piece_bounds :: (Piecenum, Piecenum);
test_piece_bounds = (Piecenum 0, Piecenum 3);

test_position :: Position;
test_position = listArray test_piece_bounds [Nothing, Just $ snd board_bounds, Nothing, Nothing];

test_directory :: Directory;
test_directory = listArray test_piece_bounds [king Biggerizer, king Smallerizer, queen Biggerizer, rook Smallerizer];

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


moves :: Directory -> Position -> Piecenum -> [Location];
moves directory position num = case position ! num of {
Nothing -> [];
Just mylocation -> let {
 andBundle :: (Location -> Position -> [Location]) -> [Location];
 andBundle f = f mylocation position;
} in nub $ filter (inRange board_bounds) $ case directory ! num of {
Piece _roy orth diag jknight jalfil jda color -> filter (not . stomp directory position color)
$ concatMap andBundle
[orthmoves orth
,diagmoves diag
,knightmoves jknight
,alfilmoves jalfil
,dabbabamoves jda
]}};

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
&& (isNothing $ at_location p l);

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
stomp directory p mycolor mylocation = case at_location p mylocation of {
Nothing -> False;
Just num -> mycolor == get_color (directory ! num);
};

get_color :: Piece -> Color;
get_color (Piece _ _ _ _ _ _ c) = c;

is_royal :: Piece -> Bool;
is_royal (Piece Royal _ _ _ _ _ _) = True;
is_royal _ = False;

at_location :: Position -> Location -> Maybe Piecenum;
at_location pos loc = let {
 check1 :: (Piecenum, Maybe Location) -> [Piecenum];
 check1 (_,Nothing) = mzero;
 check1 (num, Just loc2) = if loc==loc2 then return num else mzero;
} in case concatMap check1 $ assocs pos of {
[] -> Nothing;
[x] -> Just x;
_ -> error "multiple occupancy"
};

-- | position and player to move
type MovePosition = (Position, Color);

-- stalemate detection

has_king :: Directory -> MovePosition -> Bool;
{-
has_king Biggerizer pos = isJust $ pos ! Piecenum 0;
has_king Smallerizer pos = isJust $ pos ! Piecenum 1;
-}
-- generalize to any number of royal pieces
has_king dir (position,color) = any (\(ml, p) -> isJust ml && is_royal p && get_color p == color)
$ zip (elems position) (elems dir);

retrograde_positions :: Directory -> MovePosition -> [MovePosition];
retrograde_positions dir (pos, color) = do {
 (i :: Piecenum , p :: Piece) <- assocs dir;
 guard $ other color == get_color p;
 new_loc <- moves dir pos i;
 -- | No captures for retrograde analysis.  (Though later, uncaptures.)
 guard $ isNothing $ at_location pos new_loc;
 let { pos2 = pos // [(i,Just new_loc)]; };
 uncapture :: [(Piecenum, Maybe Location)] <- [] : do {
  (i2, ml) <- assocs pos2;
  guard $ (get_color $ dir ! i2) == color;
  guard $ isNothing ml;
  return [(i2, pos ! i)]; -- ^old position
 };
 let { pos3 = pos2 // uncapture; };
 guard $ has_king dir (pos3, other color);
 -- ^ optimization: other color has a king, possibly uncaptured
 return (pos3, other color);
};

overlapping :: Eq a => [a] -> Bool;
overlapping l = length l /= (length $ nub l);

all_positions :: Directory -> [MovePosition];
all_positions dir = do {
 l :: [Maybe Location] <- mapM (\_ -> Nothing:(map Just $ range board_bounds)) $ elems dir;
 guard $ not $ overlapping $ catMaybes l;
 color <- [Biggerizer, Smallerizer];
 return (listArray (bounds dir) l, color);
};

-- entries in which the value is known without analysis
final_entries :: Directory -> [(MovePosition,Value)];
final_entries dir = do {
 mp <- all_positions dir;
 guard $ not $ has_king dir mp;
 return (mp, loss $ snd mp);
};

value_via_successors :: Directory -> MovePosition -> [(MovePosition,Value)] -> Maybe Value;
value_via_successors dir mp@(_,color) succs = let {
 table :: Map [Maybe Location] Value;
 table = Map.fromList $ map (\((p, c2),v) -> assert (c2 == other color) (elems p,v)) succs;
} in combine_values color $ map ((flip Map.lookup) table) $ map (\(p,c2) -> assert (c2 == other color) $ elems p) $ successors dir mp ;

successors :: Directory -> MovePosition -> [MovePosition];
successors dir mp@(pos,color) = do {
 guard $ has_king dir mp;
 (i :: Piecenum, p :: Piece) <- assocs dir;
 guard $ color == get_color p;
 new_loc <- moves dir pos i;
 return (pos // ((i,Just new_loc):case at_location pos new_loc of {
Nothing -> [];
Just captured -> assert (captured /= i) [(captured, Nothing)]})
, other color);
};

redfn :: Directory -> MovePosition -> [(Epoch,(MovePosition,Value))] -> [(MovePosition,Value)];
redfn dir mp esuccs = if any (\case {(Known,_) -> True;_->False}) esuccs
then [] -- skip already known values
else case value_via_successors dir mp (map snd esuccs) of {
Nothing -> [];
Just v -> [(mp,v)];
};

mapfn :: Directory -> (MovePosition, Value) -> [(MovePosition, Epoch)];
mapfn dir (pos,_val) = (pos,Known):(map (\x -> (x, Unknown)) $ retrograde_positions dir pos);

do_mapreduce :: Directory -> [Entry] -> [Entry];
do_mapreduce dir = mapReduce (mapfn dir) (redfn dir);

test_retro1 :: Directory -> MovePosition -> [(MovePosition, Bool)];
test_retro1 dir pos = do {
p1 <- retrograde_positions dir pos;
return (p1, elem pos $ successors dir p1);
};

simple_pos :: MovePosition -> (Color, [Maybe Location]);
simple_pos (p,color) = (color, elems p);

test_retro2 :: Directory -> MovePosition -> Bool;
test_retro2 dir = and . map snd . test_retro1 dir;

-- omit answers which are already known

test_compare :: Directory -> [Entry] -> Set MovePosition;
test_compare dir seed = let {
 s1 :: Set MovePosition;
 s1 = Set.fromList $ map fst $ seed;
 s2 :: Set MovePosition;
 s2 = Set.fromList $ map fst $ do_mapreduce dir seed;
} in Set.intersection s1 s2;

type Entry = (MovePosition, Value);

gen_0 :: [Entry];
gen_0 = final_entries test_directory;
gen_1 :: [Entry];
gen_1 = do_mapreduce test_directory gen_0;

gen_2 :: [Entry];
gen_2 = do_mapreduce test_directory $ gen_0 ++ gen_1;

gens :: [Entry] -> [Entry];
gens l = l ++ do_mapreduce test_directory l;

until_fixed :: Eq a => [a] -> [a];
until_fixed (p:rest) = if (p==head rest)
then [p] else p:until_fixed rest;
until_fixed _ = error "too short list";

} --end
