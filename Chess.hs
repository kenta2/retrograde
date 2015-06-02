{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Chess where {
import Data.List;
import Control.Monad;
import Data.Maybe;
import Data.Array.IArray;
import Debug.Trace;

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

-- | Maximizing or Minimizing the Value of a position
data Color = Biggerizer | Smallerizer deriving (Show, Eq);

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

piece_bounds :: (Piecenum, Piecenum);
piece_bounds = (Piecenum 0, Piecenum $ case maxpieces of {
Piecenum n -> pred n;
});

test :: Position;
test = listArray piece_bounds [Just $ Location (0,0), Just $ Location (7,7), Nothing, Nothing];

test_directory :: Directory;
test_directory = listArray piece_bounds [king Biggerizer, king Smallerizer, queen Biggerizer, rook Smallerizer];

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
-- retrograde positions, including uncaptures

other_color :: Color -> Color;
other_color Biggerizer = Smallerizer;
other_color Smallerizer = Biggerizer;

has_king :: Directory -> MovePosition -> Bool;
{-
has_king Biggerizer pos = isJust $ pos ! Piecenum 0;
has_king Smallerizer pos = isJust $ pos ! Piecenum 1;
-}
-- generalize to any number of royal pieces
has_king dir (position,color) = any (\(ml, p) -> isJust ml && is_royal p && get_color p == color)
$ zip (elems position) (elems dir);

retrograde_positions :: Directory -> MovePosition -> [MovePosition];
retrograde_positions dir (pos, color) = let
{ othercolor = other_color color
; poss :: [Position]
; poss = do {
 (i :: Piecenum , p :: Piece) <- assocs dir;
 guard $ color == get_color p;
 new_loc <- moves dir pos i;
 -- | No captures for retrograde analysis.  (Though later, uncaptures.)
 guard $ isNothing $ at_location pos new_loc;
 let { pos2 = pos // [(i,Just new_loc)]; };
 uncapture :: [(Piecenum, Maybe Location)] <- [] : do {
  (i2, ml) <- assocs pos2;

  guard $ (get_color $ dir ! i2) == othercolor;
  guard $ isNothing ml;
  return [(i2, pos ! i)]; -- ^old position
 };
 return $ pos2 // uncapture;
};
} in do {
  p1 <- poss;
  guard $ has_king dir (p1,othercolor);
  return (p1,othercolor);
};


} --end
