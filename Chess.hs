{-# LANGUAGE ScopedTypeVariables, LambdaCase, GeneralizedNewtypeDeriving #-}
module Chess where {
import Data.List;
import Control.Monad;
import Data.Maybe;
import Data.Array.IArray;
import Debug.Trace;
import Retrograde;
import Data.Map.Strict(Map);
import qualified Data.Map.Strict as Map;
import qualified Data.Set as Set;
import Data.Set(Set);
import Control.Monad.GenericReplicate;
import Control.Exception(assert);
import Data.Ord;
import System.Random(randomRIO);

my_boardsize :: (Integer,Integer);
my_boardsize = (4,4);  -- col row

stalemate_draw :: Bool;
stalemate_draw = False;

pass_permitted :: Bool;
pass_permitted = False;

-- no stalemate
-- 43 = 2.5 min
-- 44 = 16 min
-- 45 = 55 min
-- 55 = 123 min on mkc(?)

-- test directory
test_directory :: Directory;
test_directory = dir_n;

max_row :: Row;
max_row = Row $ pred $ snd my_boardsize;

max_column :: Column;
max_column = Column $ pred $ fst my_boardsize;;

-- to avoid the redundancy warning
trace_placeholder :: ();
trace_placeholder = trace "trace" ();

type Offset = (Column,Row);
newtype Row = Row Integer deriving (Eq, Ord, Enum, Ix, Show);
newtype Column = Column Integer deriving (Eq, Ord, Enum, Ix, Show);

type Position = Array Piecenum (Maybe Location);
newtype Location = Location Offset deriving (Eq, Ord, Ix, Show);
newtype Piecenum = Piecenum Integer deriving (Eq, Ord, Ix, Show);

data Orthogonal = NoOrthogonal | Wazir | Rook deriving (Show, Eq, Ord, Bounded, Ix);
data Diagonal = NoDiagonal | Ferz | Bishop deriving (Show, Eq, Ord, Bounded, Ix);
data Knight = NoKnight | YesKnight deriving (Show, Eq, Ord, Bounded, Ix);
data Alfil = NoAlfil | YesAlfil deriving (Show, Eq, Ord, Bounded, Ix);
data Dabbaba = NoDabbaba | Dabbaba_single | Dabbaba_rider deriving (Show,Eq, Ord, Bounded, Ix);
data Royal = Commoner | Royal deriving (Show, Eq, Ord, Bounded, Ix);

-- | Maximizing or Minimizing the Value of a position
data Color = White | Black deriving (Show, Eq, Ord, Bounded, Ix);
other :: Color -> Color;
other White = Black;
other Black = White;

data Piece = Piece Royal Orthogonal Diagonal Knight Alfil Dabbaba Color deriving (Show, Eq, Ord, Bounded, Ix);

king :: Color -> Piece;
king = Piece Royal Wazir Ferz NoKnight NoAlfil NoDabbaba;

man :: Color -> Piece;
man = Piece Commoner Wazir Ferz NoKnight NoAlfil NoDabbaba;

queen :: Color -> Piece;
queen = Piece Commoner Rook Bishop NoKnight NoAlfil NoDabbaba;

rook :: Color -> Piece;
rook = Piece Commoner Rook NoDiagonal NoKnight NoAlfil NoDabbaba;

knight :: Color -> Piece;
knight = Piece Commoner NoOrthogonal NoDiagonal YesKnight NoAlfil NoDabbaba;

bishop :: Color -> Piece;
bishop = Piece Commoner NoOrthogonal Bishop NoKnight NoAlfil NoDabbaba;

td :: Directory;
td = test_directory;

test_position :: MovePosition;
test_position = (listArray (Piecenum 0, Piecenum 3) $ map (\(x,y) -> Just $ Location (Column x, Row y))
[(3,2),(2,0),(1,2),(1,1)],White);
--[(3,3),(3,1),(3,4),(0,3)],White);
--[(3,3),(1,0),(2,0),(0,1)],Black);
--[(3,3),(1,0),(2,2),(0,1)],White);

dir_experiment :: Directory;
dir_experiment = listArray (Piecenum 0, Piecenum 3) [king White, king Black
, Piece Commoner NoOrthogonal Ferz NoKnight NoAlfil NoDabbaba White
, Piece Commoner NoOrthogonal NoDiagonal YesKnight NoAlfil NoDabbaba White
];

dir_bn :: Directory;
dir_bn = listArray (Piecenum 0, Piecenum 3) [king White, king Black
, bishop White
, knight White
];

dir_nn :: Directory;
dir_nn = listArray (Piecenum 0, Piecenum 3) [king White, king Black
, knight White
, knight White
];

-- winnable when stalemate==loss
dir_n :: Directory;
dir_n = make_dir [king White, king Black , knight White];

dir_bb :: Directory;
dir_bb = listArray (Piecenum 0, Piecenum 3) [king White, king Black
, bishop White
, bishop White
];

dir_qr :: Directory;
dir_qr = listArray (Piecenum 0, Piecenum 3) [king White, king Black, queen White, rook Black];

dir_kmk :: Directory;
dir_kmk = listArray (Piecenum 0, Piecenum 2) [king White, king Black, man White];

type IOffset = (Integer,Integer);

reflect45 :: IOffset -> IOffset;
reflect45 (x,y) = (y,x);

reflectx :: IOffset -> IOffset;
reflectx (x,y) = (x,negate y);

reflecty :: IOffset -> IOffset;
reflecty (x,y) = (negate x,y);

eightway_with_duplicates :: IOffset -> [IOffset];
eightway_with_duplicates z = do {
p <- [id,reflect45];
q <- [id,reflectx];
r <- [id, reflecty];
return $ r $ q $ p $ z;
};

eightway :: IOffset -> [Offset];
eightway = map (\z -> (Column $ fst z, Row $ snd z)) . nub . eightway_with_duplicates;

extend :: Offset -> [Offset];
extend (Column x,Row y) = do {
s <- enumFromTo 1 $ unBoardsize board_max;
return (Column $ s*x,Row $ s*y);
};

board_max :: Boardsize;
board_max = case (max_row, max_column) of
{ (Row r, Column c) -> Boardsize $ max r c };

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
knightmoves YesKnight me _ = map (add_offset me) $ eightway (1,2);

alfilmoves :: Alfil -> Location -> Position -> [Location];
alfilmoves NoAlfil _ _ = [];
alfilmoves YesAlfil me _ = map (add_offset me) $ eightway (2,2);

dabbabamoves :: Dabbaba -> Location -> Position -> [Location];
dabbabamoves NoDabbaba _ _ = [];
dabbabamoves Dabbaba_single me _ = map (add_offset me) $ eightway (2,0);
dabbabamoves Dabbaba_rider me pos = concatMap (extendUntilOccupied me pos) $ eightway (2,0);

board_bounds :: (Location,Location);
board_bounds = (Location (Column 0,Row 0),Location (max_column, max_row));

empty :: Position -> Location -> Bool;
empty p l = inRange board_bounds l
&& (isNothing $ at_location p l);

type Directory = Array Piecenum Piece;

add_offset :: Location -> Offset -> Location;
add_offset (Location (ox,oy)) (dx,dy) = Location (column_add ox dx,row_add oy dy);

row_add :: Row -> Row -> Row;
row_add (Row x) (Row y) = Row $ x+y;

column_add :: Column -> Column -> Column;
column_add (Column x) (Column y) = Column $ x+y;

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
-- generalize to any number of royal pieces
has_king dir (position,color) = any (\(ml, p) -> isJust ml && is_royal p && get_color p == color)
$ zip (elems position) (elems dir);

retrograde_positions :: Directory -> MovePosition -> [MovePosition];
retrograde_positions dir mp@(pos, color) = do {
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
} ++ do {
 guard pass_permitted;
 let {newpos = do_pass mp};
 guard $ has_king dir newpos;
 return newpos;
};

overlapping :: Eq a => [a] -> Bool;
overlapping l = length l /= (length $ nub l);

all_positions :: Directory -> [MovePosition];
all_positions dir = do {
 l :: [Maybe Location] <- mapM (\_ -> Nothing:(map Just $ range board_bounds)) $ elems dir;
 guard $ not $ overlapping $ catMaybes l;
 color <- [White, Black];
 return (listArray (bounds dir) l, color);
};

-- entries in which the value is known without analysis
final_entries :: Directory -> [(MovePosition,Value)];
final_entries dir = losses (kingless dir)
++ if stalemate_draw then stalemates dir
else losses (locked_with_king dir);

losses :: [MovePosition] -> [(MovePosition,Value)];
losses = map (\a -> (a,loss));

kingless :: Directory -> [MovePosition];
kingless dir = filter (not . has_king dir) $ all_positions dir ;
locked_with_king :: Directory -> [MovePosition];
locked_with_king dir = filter (\p -> has_king dir p && (null $ successors dir p)) $ all_positions dir;

value_via_successors :: Directory -> MovePosition -> [(MovePosition,Value)] -> Maybe Value;
value_via_successors dir mp@(_,color) succs = let {
 table :: Map [Maybe Location] Value;
 table = Map.fromList $ map (\((p, c2),v) -> assert (c2 == other color) (elems p,v)) succs;
} in combine_values_greedy $ map ((flip Map.lookup) table) $ map (\(p,c2) -> assert (c2 == other color) $ elems p) $ successors dir mp ;

successors :: Directory -> MovePosition -> [MovePosition];
successors dir mp@(pos,color) = if not $ has_king dir mp
then []
else do {
 (i :: Piecenum, p :: Piece) <- assocs dir;
 guard $ color == get_color p;
 new_loc <- moves dir pos i;
 return (pos // ((i,Just new_loc):case at_location pos new_loc of {
Nothing -> [];
Just captured -> assert (captured /= i) [(captured, Nothing)]})
, other color);
} ++ if pass_permitted then [do_pass mp] else [];

redfn :: Directory -> MovePosition -> [((MovePosition,Value),Epoch)] -> Maybe (MovePosition,Value);
redfn dir mp esuccs = do {
 -- skip already known values
 guard $ all (\case {(_,Known) -> False;_->True}) esuccs;
 v <- value_via_successors dir mp (map fst esuccs);
 return (mp,v);
};

mapfn :: Directory -> (MovePosition, Value) -> [(MovePosition, Epoch)];
mapfn dir (pos,_val) = (pos,Known):(map (\x -> (x, Unknown)) $ retrograde_positions dir pos);

do_mapreduce :: Directory -> [Entry] -> [Entry];
do_mapreduce dir = catMaybes . mapReduce (mapfn dir) (redfn dir);

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

iterate_mapreduce :: Directory -> [Entry] -> [[Entry]];
iterate_mapreduce dir start = let {
more = do_mapreduce dir start;
} in if null more then []
else more:iterate_mapreduce dir (start ++ more);

display_piece :: Piece -> String;
display_piece p = if p == king White then "K"
else if p == king Black then "k"
else if p == queen White then "Q"
else if p == rook Black then "r"
else if p == rook White then "R"
else if p == bishop White then "B"
else if p == knight White then "N"
else if p == bishop Black then "b"
else if p == knight Black then "n"
else if p == man White then "M"
else if p == man Black then "m"
else "?";

show_board_p :: Directory -> Position -> String;
show_board_p dir = show_board_f (display_piece . (dir!));

show_board_numbers :: Position -> String;
show_board_numbers = show_board_f (\(Piecenum n) -> show n);

show_board_f ::(Piecenum -> String) -> Position -> String;
show_board_f f pos = unlines $ do { rank <- reverse $ enumFromTo (Row 0) max_row;
return $ unwords $ do {
file <- enumFromTo (Column 0) max_column;
return $ case at_location pos $ Location (file, rank) of {
Nothing -> "-";
Just num -> f num;
}}
};

show_mp :: Directory -> MovePosition -> String;
show_mp _dir (p,color) = show_board_numbers p ++ show color;

show_entry :: Directory -> Entry -> String;
show_entry dir (mp,val) = show_mp dir mp ++ " " ++ show val;

in_check :: Directory -> MovePosition -> Bool;
in_check dir mp = assert (has_king dir mp) $
 illegal_position dir $ do_pass mp;

-- | King can be captured
illegal_position :: Directory -> MovePosition -> Bool;
illegal_position dir mp = assert (has_king dir $ do_pass mp) $
 any (not . has_king dir) $ successors dir mp;

do_pass :: MovePosition -> MovePosition;
do_pass (pos,color) = (pos, other color);

-- Note: this method of discovering stalemates does not work, i.e.,
-- never stalemate, if passing is permitted.  I feel this logically
-- makes sense, though it would alternatively be possible to
-- special-case stalemate as a position in which one can instantly
-- claim a draw even if passing is permitted.
no_legal_moves :: Directory -> MovePosition -> Bool;
no_legal_moves dir = null . filter (not . illegal_position dir) . successors dir;

stalemate :: Directory -> MovePosition -> Bool;
stalemate dir mp = (not $ in_check dir mp) && no_legal_moves dir mp;

checkmate :: Directory -> MovePosition -> Bool;
checkmate dir mp = in_check dir mp && no_legal_moves dir mp;

stalemates :: Directory -> [Entry];
stalemates dir = do {
 mp <- all_positions dir;
 guard $ has_king dir mp;
 guard $ stalemate dir mp;
 return (mp, draw);
};

all_pieces :: [Piece];
all_pieces = whole_range;

piece_set :: Integer -> [[Piece]];
piece_set size = do {
z <- genericReplicateM size all_pieces;
guard $ any (\p -> is_royal p && (White == get_color p)) z;
guard $ any (\p -> is_royal p && (Black == get_color p)) z;
guard (z == sort z);
return z;
};

piece_set2 :: Integer -> [Piece] -> [[Piece]];
piece_set2 0 z {-^ accumulating parameter -} = assert (z == sort z) $ if (any (\p -> is_royal p && (White == get_color p)) z)
&& (any (\p -> is_royal p && (Black == get_color p)) z)
&& z <= sort (map flip_color z)
then return z
else mzero;
piece_set2 n z = do {
 h <- all_pieces;
 guard $ case z of {[] -> True; (x:_)->h<=x};
 piece_set2 (pred n) (h:z);
};

flip_color :: Piece -> Piece;
flip_color (Piece x1 x2 x3 x4 x5 x6 c) = Piece x1 x2 x3 x4 x5 x6 $ other c;

type Map_v = Map MovePosition Value;

all_list :: [[Entry]];
all_list = gen_0:iterate_mapreduce test_directory gen_0;

allmap :: Map_v;
allmap = Map.fromList $ concat all_list;

do_trace :: Directory -> Map_v -> Entry -> [Entry];
do_trace dir m p = p:case Map.lookup (fst p) m of {
Just v | v == loss -> []
| True -> let {next = minimumBy (comparing (snd::(Entry -> Value))) $ do {
s <- successors dir $ fst p;
let {nv = Map.lookup s m};
guard $ isJust nv;
return (s,fromJust nv)}} in do_trace dir m next;
Nothing -> error "do_trace"
};

longest_win :: [Entry] -> Entry;
longest_win = maximumBy (\x y -> winlength (snd x) (snd y));

show_longest :: IO();
show_longest = do {
eval_iterate;
mapM_ (putStrLn . show_entry test_directory)  $ do_trace test_directory allmap $ longest_win $ concat all_list;
};

integers_from_zero :: [Integer];
integers_from_zero = enumFrom 0;
eval_iterate :: IO();
eval_iterate = do {
 mapM_ print $ assocs test_directory;
 mapM_ print $ zip integers_from_zero  $map length all_list;
};

three_pieces :: [[Piece]];
three_pieces = piece_set2 4 [];

three_pieces_length :: Integer;
-- three_pieces_length = genericLength $ three_pieces ();
--three_pieces_length = 562464;
three_pieces_length = 62432010;

three_pieces_length_check :: IO();
three_pieces_length_check = assert (genericLength three_pieces == three_pieces_length) $ return ();

make_dir :: [Piece] -> Directory;
make_dir pcs = listArray (Piecenum 0, Piecenum $ pred $ genericLength pcs) pcs;

try_three_pieces :: Integer -> IO();
try_three_pieces n = do {
let {pcs = genericIndex three_pieces n};
mapM_ print $ zip integers_from_zero pcs;
let {
 dir :: Directory;
 dir = make_dir pcs;
 fin_en :: [Entry];
 fin_en = final_entries dir;
 ls :: [[Entry]];
 ls = fin_en:iterate_mapreduce dir fin_en;
 amap1 :: Map_v;
 amap1 = Map.fromList $ concat ls;
 longest :: Entry;
 longest = maximumBy (\x y -> winlength (snd x) (snd y)) $ concat ls;
 len :: Integer;
 len = genericLength ls;
};
putStrLn $"totals " ++ (show $ Map.size amap1);
putStr "length ";
print $ len;
if len>9 then mapM_ (putStrLn . show_entry dir) $ do_trace dir amap1 $ longest
else return ();

};

rand_three_pieces :: IO();
rand_three_pieces = do {
n :: Integer <- randomRIO (0,pred three_pieces_length);
putStr "seed ";
print n;
try_three_pieces n;
rand_three_pieces;
};

whole_range :: (Ix a, Bounded a) => [a];
whole_range = range (minBound, maxBound);

all_pieces_for_cplusplus :: String;
all_pieces_for_cplusplus = concat $ intersperse ",\n" $ do {
o :: Orthogonal <- whole_range;
d :: Diagonal <- whole_range;
k :: Knight <- whole_range;
a :: Alfil <- whole_range;
da :: Dabbaba <- whole_range;
return $ "Piece(Orthogonal::"++show o
           ++", Diagonal::"++show d
           ++", Knight::"++show k
           ++", Alfil::"++show a
           ++", Dabbaba::"++case da of {
                                      NoDabbaba -> "NoDabbaba";
                                      Dabbaba_single -> "Single";
                                      Dabbaba_rider -> "Rider";
                                    }
           ++", White, true)";
};

moves_on_empty_board :: Piece -> Location -> [Location];
moves_on_empty_board p mylocation = let {
 dir :: Directory;
 dir = make_dir [p];
 pos :: Position;
 pos = listArray (Piecenum 0, Piecenum 0) [Just mylocation];
} in sort $ moves dir pos (Piecenum 0);

all_simple_pieces :: [Piece];
all_simple_pieces = filter is_simple_piece all_pieces;

is_simple_piece :: Piece -> Bool;
is_simple_piece (Piece Royal _ _ _ _ _ White) = True;
is_simple_piece _ = False;

verify_piece_locs_inputs :: [(Piece, Location, [Location])];
verify_piece_locs_inputs = do {
p <- all_simple_pieces;
l <- range board_bounds;
return (p,l,moves_on_empty_board p l);
};

take2 :: [Integer] -> [Offset];
take2 [] = [];
take2 (x:y:rest) = (Column x, Row y): take2 rest;
take2 _ = error "odd number for take2";

verify_piece_locs :: IO ();
verify_piece_locs = do {
ll :: [[Location]] <- getContents >>= return . map (map Location . sort . take2 . map read . words) . lines;
if length ll /= length verify_piece_locs_inputs
then error "not same length"
else return();
zipWithM_ (\(a,b,l1) l2 -> print (a,b,if l1==l2 then (True,[],[]) else (False,l1,l2))) verify_piece_locs_inputs ll;
};

location_from_integer :: Integer -> Maybe Location;
location_from_integer n = if n<0
then error "negative location_from_integer"
else let
{(num_rows, num_columns) =
case (max_row, max_column) of
{(Row rmax, Column cmax) -> (rmax+1, cmax+1)};
maxsize = num_rows * num_columns
}
in if n == maxsize
then Nothing
else if n > maxsize
then error "too big location_from_integer"
else let {
ans = divMod n num_rows;
} in Just $ Location (Column $ fst ans, Row $ snd ans);

read_moveposition :: [Integer] -> MovePosition;
read_moveposition l = (listArray (bounds test_directory) $ map location_from_integer $ tail l,
case head l of {
0 -> White;
1 -> Black;
_ -> error "read_moveposition color invalid";
});

verify_successors_concat :: Integer -> IO();
verify_successors_concat depth = do {
s1 :: [MovePosition] <- getContents >>= return . sort . map (read_moveposition . map read . words) . lines;
-- mapM_ (putStrLn . show_mp undefined) s1;
let {correct = sort $ recursive_successors depth test_position};
-- mapM_ (putStrLn . show_mp undefined) correct;
mapM_ (putStrLn . unwords . map show .  position_to_integer) correct;
print $ s1==correct;
};

recursive_successors :: Integer -> MovePosition -> [MovePosition];
recursive_successors 0 p = [p];
recursive_successors n p = concatMap (recursive_successors $ pred n) $ successors test_directory p;

location_to_integer :: Maybe Location -> Integer;
location_to_integer l = let
{(num_rows, num_columns) =
case (max_row, max_column) of
{(Row rmax, Column cmax) -> (rmax+1, cmax+1)};
maxsize = num_rows * num_columns
} in case l of {
Nothing -> maxsize;
Just (Location (Column c, Row r)) -> c*num_rows +r;
};

position_to_integer :: MovePosition -> [Integer];
position_to_integer (p,c) = (case c of {
White->0;Black->1}):(map location_to_integer $ elems p);

n_chunk :: Integer -> [a] -> [[a]];
n_chunk i = unfoldr (\l -> if null l then Nothing else Just (genericSplitAt i l));

verify_successors :: IO();
verify_successors = do {
ii :: [[[Integer]]] <- getContents >>= return . map (n_chunk 5 . map read . words) . lines;
(flip mapM_) ii $ (\(h:t) -> do {
if sort (map read_moveposition t) == (sort $ successors test_directory $ read_moveposition h) then return ()
else do {
print h;
print $ elems $ fst $ read_moveposition h;
print t;
}});
return ();
};

table_line :: Entry -> [Integer];
table_line (mp,v) = position_to_integer mp ++ [c_value v];

read_table_line :: [Integer] -> Entry;
read_table_line is = (read_moveposition $ init is, read_c_value $ last is);

read_dump_from_file :: String -> IO [Entry];
read_dump_from_file fn = readFile fn >>= return . (map (read_table_line . map read . words)) . filter (not . is_comment) .  lines;

is_comment :: String -> Bool;
is_comment [] = False;
is_comment ('#':_) = True;
is_comment _ = False;

zip_map :: (a -> b) -> [a] -> [(a,b)];
zip_map f l = zip l $ map f l;

} --end
