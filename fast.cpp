// --std=c++11
#include <vector>
#include <utility>
#include <cstdint>
#include <algorithm>
#include <iostream>
#include <cstring>
#include <cassert>
#include <string>
#include <iomanip>

using namespace std;

const int8_t num_columns=4;
const int8_t num_rows=4;
//larger board sizes need to ulimit -s

const bool stalemate_draw=false;
const int8_t ACTUAL_SIZE=num_rows*num_columns;
const int8_t POSITION_POSSIBILITIES=ACTUAL_SIZE+1; // or piece is nowhere

// todo: not hardcode board size

typedef pair <int8_t,int8_t> Coord;

ostream& operator<<(ostream &os, const Coord& c){
  os << '(' << static_cast<int>(c.first) << ',' << static_cast<int>(c.second) << ')';
  return os;
}

Coord dir_orthogonal[]={Coord(0,1),Coord(1,0),Coord(-1,0),Coord(0,-1)};
Coord dir_diagonal[]={Coord(1,1),Coord(1,-1),Coord(-1,1),Coord(-1,-1)};

Coord dir_knight[]={Coord(1,2),Coord(-1,2),Coord(1,-2),Coord(-1,-2),
                    Coord(2,1),Coord(-2,1),Coord(2,-1),Coord(-2,-1)};

enum class Orthogonal {NoOrthogonal, Wazir, Rook};
enum class Diagonal {NoDiagonal, Ferz, Bishop};
enum class Dabbaba {NoDabbaba, Single, Rider};
enum class Knight {NoKnight, YesKnight};
enum class Alfil {NoAlfil, YesAlfil};

const int8_t board_size_max = max(num_rows, num_columns);

enum Color { White, Black};

class Bitboard{
public:
  uint8_t b[num_columns][num_rows];

  Bitboard() : b {0}
  {}
  bool occupied(const Coord& p) const {
    return b[p.first][p.second];
  }
  Color color_at(const Coord& p) const {
    return static_cast<Color>((b[p.first][p.second]>>1)&1);
  }
  uint8_t index(const Coord& p) const {
    return b[p.first][p.second]>>2;
  }
};

ostream& operator<<(ostream& os, const Bitboard& b){
  for(int i=num_rows-1;i>=0;--i){
    for(int j=0;j<num_columns;++j){
      os << setw(4) << static_cast<int>(b.index(Coord(j,i)));
    }
    os << endl << dec;
  }
  return os;
}

inline Coord add_coord(const Coord& a, const Coord& b){
  return Coord(a.first+b.first, a.second+b.second);
}

inline bool in_bounds(const Coord& p){
  return (p.first>=0)&&(p.first<num_columns)&&
    (p.second>=0)&&(p.second<num_rows);
}

inline bool duplicate_entry(const vector<Coord>& v, const Coord& target){
  return find(v.cbegin(), v.cend(), target)!=v.cend();
}

class Piece {
  Orthogonal orthogonal;
  Diagonal diagonal;
  Knight knight;
  Alfil alfil;
  Dabbaba dabbaba;
public:
  Color color;
  bool is_royal;
  Piece(Orthogonal o, Diagonal d, Knight k, Alfil a, Dabbaba da, Color c, bool roy)
    : orthogonal(o), diagonal(d), knight(k), alfil(a), dabbaba(da), color(c), is_royal(roy)
  {}
  vector<Coord> moves(const Coord& mylocation, const Bitboard& board, const uint8_t mycolor) const {
    vector<Coord> answer;

    if(Orthogonal::Wazir == orthogonal)
      //answer.insert(answer.end(),dir_orth,dir_orth+4);
      for(int i=0;i<4;++i){
        Coord pos=add_coord(mylocation,dir_orthogonal[i]);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }
    else if(Orthogonal::Rook == orthogonal)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;s<board_size_max;++s){
          Coord pos = add_coord(mylocation, Coord(s*dir_orthogonal[i].first,s*dir_orthogonal[i].second));
          if(!in_bounds(pos)) break;
          if(board.occupied(pos)){
            if(board.color_at(pos)!=mycolor)
              answer.push_back(pos);
            break;
          } else
            answer.push_back(pos);
        }


    if(Diagonal::Ferz == diagonal)
      for(int i=0;i<4;++i){
        Coord pos=add_coord(mylocation,dir_diagonal[i]);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }
    else if(Diagonal::Bishop == diagonal)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;s<board_size_max;++s){
          Coord pos = add_coord(mylocation, Coord(s*dir_diagonal[i].first,s*dir_diagonal[i].second));
          if(!in_bounds(pos)) break;
          if(board.occupied(pos)){
            if(board.color_at(pos)!=mycolor)
              answer.push_back(pos);
            break;
          } else
            answer.push_back(pos);
        }

    // dabbaba and alfil induce duplicates with rook and bishop
    if(Dabbaba::Single == dabbaba)
      for(int i=0;i<4;++i){
        Coord pos=Coord(mylocation.first+2*dir_orthogonal[i].first,
                        mylocation.second+2*dir_orthogonal[i].second);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)) && (orthogonal != Orthogonal::Rook || !duplicate_entry(answer,pos)))
          answer.push_back(pos);
      }
    else if(Dabbaba::Rider == dabbaba)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;s<board_size_max;++s){
          Coord pos = add_coord(mylocation, Coord(2*s*dir_orthogonal[i].first,2*s*dir_orthogonal[i].second));
          if(!in_bounds(pos)) break;
          if(board.occupied(pos)){
            if(board.color_at(pos)!=mycolor)
              if (orthogonal != Orthogonal::Rook || !duplicate_entry(answer,pos))
                answer.push_back(pos);
            break;
          } else
            if (orthogonal != Orthogonal::Rook || !duplicate_entry(answer,pos))
              answer.push_back(pos);
        }

    if(Knight::YesKnight == knight)
      for(int i=0;i<8;++i){
        Coord pos=add_coord(mylocation,dir_knight[i]);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }

    if(Alfil::YesAlfil == alfil)
      for(int i=0;i<4;++i){
        Coord pos=Coord(mylocation.first+2*dir_diagonal[i].first,
                        mylocation.second+2*dir_diagonal[i].second);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)) && (diagonal != Diagonal::Bishop || !duplicate_entry(answer,pos)) )
          answer.push_back(pos);
      }


    return answer;
  }
  string toString() const {
    string answer("Piece(Orthogonal::");
    if(Orthogonal::NoOrthogonal == orthogonal)
      answer+="NoOrthogonal";
    else if(Orthogonal::Wazir == orthogonal)
      answer+="Wazir";
    else if(Orthogonal::Rook == orthogonal)
      answer+="Rook";
    else assert(0);
    answer += ", Diagonal::";
    if(Diagonal::NoDiagonal == diagonal)
      answer+="NoDiagonal";
    else if(Diagonal::Ferz == diagonal)
      answer+="Ferz";
    else if(Diagonal::Bishop == diagonal)
      answer+="Bishop";
    else assert(0);
    answer += ", Knight::";
    if(Knight::NoKnight==knight)
      answer+="NoKnight";
    else if (Knight::YesKnight==knight)
      answer+="YesKnight";
    answer += ", Alfil::";
    if(Alfil::NoAlfil==alfil)
      answer+="NoAlfil";
    else if (Alfil::YesAlfil==alfil)
      answer+="YesAlfil";
    else assert(0);
    answer += ", Dabbaba::";
    if(Dabbaba::NoDabbaba == dabbaba)
      answer+="NoDabbaba";
    else if(Dabbaba::Single == dabbaba)
      answer+="Single";
    else if(Dabbaba::Rider == dabbaba)
      answer+="Rider";
    else assert(0);
    answer+=", ";
    if(0==color)
      answer+="White";
    else if (1==color)
      answer+="Black";
    else assert(0);
    answer+=", ";
    if(is_royal)
      answer+="true";
    else
      answer+="false";
    answer+=")";
    return answer;
  }
};

vector<Piece> all_pieces{
#include "all_pieces.cpp"
};

void printlocs(const Piece& p){
  Bitboard b;
  for(int8_t i=0;i<num_columns;++i)
    for(int8_t j=0;j<num_rows;++j){
      assert(!b.occupied(Coord(i,j)));
    }
  for(int8_t i=0;i<num_columns;++i)
    for(int8_t j=0;j<num_rows;++j){
      vector<Coord> v = p.moves(Coord(i,j),b,0);
      //for_each(v.begin(), v.end(), [](Coord& c){}); //lambda function
      for(const Coord& c : v)
        cout << " " << static_cast<int>(c.first) <<" "<< static_cast<int>(c.second);
      cout << endl;
    }
}

class MaybeLocation {
  Coord l;
public:
  bool alive;
  Coord get() const {
    assert(alive);
    return l;
  }
  void set(const Coord& c){
    assert(alive);
    l=c;
  }
  int to_numeric() const {
    if(alive){
      assert(in_bounds(l));
      return static_cast<int>(l.first)*num_rows+l.second;
    }else
      return static_cast<int>(num_rows)*num_columns;
  }
  void from_numeric(int input){
    if(input==static_cast<int>(num_rows)*num_columns)
      alive=false;
    else {
      alive=true;
      l.second=input%num_rows;
      l.first=input/num_rows;
    }
  }
};

ostream& operator<<(ostream& os, const MaybeLocation& object){
  os << object.to_numeric();
  return os;
}



typedef vector<Piece> Directory;

Piece king(Color c){
  return Piece(Orthogonal::Wazir, Diagonal::Ferz, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, c, true);
}

Directory dir_qr{king(White),king(Black),
    Piece(Orthogonal::Rook, Diagonal::Bishop, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, White, false),
    Piece(Orthogonal::Rook, Diagonal::NoDiagonal, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, Black, false)};

Directory dir_n{king(White),king(Black),
    Piece(Orthogonal::NoOrthogonal, Diagonal::NoDiagonal, Knight::YesKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, White, false)};

Directory test_directory(dir_qr);

const int MAX_PIECES=4;
typedef MaybeLocation Position[MAX_PIECES];

class MovePosition {
public:
  Color to_move;
  Position position;
};

ostream& operator<<(ostream& os, const MovePosition& object){
  os << static_cast<int>(object.to_move);
  for(int i=0;i<MAX_PIECES;++i)
  os << " " << object.position[i];
  return os;
}

typedef int16_t Value;

class Table {
// dimensions = 1+MAX_PIECES
  //Value table[2][POSITION_POSSIBILITIES][POSITION_POSSIBILITIES][POSITION_POSSIBILITIES][POSITION_POSSIBILITIES];
  vector<vector<vector<vector<vector<Value> > > > > table;
public:
  Table(int z) : table(2, vector<vector<vector<vector<Value> > > >
                      (z, vector<vector<vector<Value> > >
                      (z, vector<vector<Value> >
                      (z, vector<Value>(z)))))
  {}
  friend ostream& operator<<(ostream& os, const Table& table);
#define INDEX table[static_cast<int>(p.to_move)] \
      [p.position[0].to_numeric()] \
      [p.position[1].to_numeric()] \
      [p.position[2].to_numeric()] \
      [p.position[3].to_numeric()] \

  Value lookup(const MovePosition& p) const {
    return INDEX;
  }
  void set(const MovePosition&p, Value v){
    INDEX = v;
  }
};

#define forR(v) for(int v=0;v<POSITION_POSSIBILITIES;++v)
ostream& operator<<(ostream& os, const Table& table){
  for(int player=0;player<=1;++player){
    forR(i0)forR(i1)forR(i2)forR(i3){
      Value v=table.table[player][i0][i1][i2][i3];
      if(v)
        os << player << " " << i0 << " " << i1 << " " << i2 << " " << i3 << " " << v << endl;
    }
  }
  return os;
}


inline bool has_king(const Directory& dir, const MovePosition& mp){
  for(unsigned int i=0;i<dir.size();++i)
    if (dir[i].color==mp.to_move && dir[i].is_royal && mp.position[i].alive)
      return true;
  return false;
}

void fill_board(Bitboard *board, const Directory& dir, const Position& pos){
  for(unsigned int i=0;i<dir.size();++i)
    if(pos[i].alive){
      Coord xy=pos[i].get();
      assert(board->b[xy.first][xy.second]==0);
      board->b[xy.first][xy.second]=1|(dir[i].color<<1)|(i<<2);
    }
}

Color other(Color x){
  return static_cast<Color>(1-x);
}

vector<MovePosition> successors(const Directory& dir, const MovePosition& mp){
  vector<MovePosition> answer;
  if(!has_king(dir,mp))
    return answer;
  Bitboard board;
  fill_board(&board,dir,mp.position);
  for(unsigned i=0;i<dir.size();++i){
    if(dir[i].color == mp.to_move && mp.position[i].alive){
      for(const Coord& newloc : dir[i].moves(mp.position[i].get(), board, mp.to_move)){
        MovePosition next(mp);
        next.to_move= other(mp.to_move);
        next.position[i].set(newloc);
        if(board.occupied(newloc)) { // capture
          next.position[board.index(newloc)].alive=false;
        }
        answer.push_back(next);
      }
    }
  }
  return answer;
}

MovePosition gen_qr_test_position(){
  MovePosition answer;
  const int8_t pos_qr_arr[4][2]=
    //{{3,2},{2,0},{1,2},{1,1}};  //longest(4,3)
    {{0,0},{0,1},{1,0},{1,1}};
  answer.to_move=White;
  for(int i=0;i<4;++i){
    answer.position[i].alive=true;
    answer.position[i].set(Coord(pos_qr_arr[i][0],pos_qr_arr[i][1]));
  }
  return answer;
}

void recursive_successors_test(int depth, const MovePosition& start){
  if(depth==0)
    ; //cout << start << endl;
  else {
    cout << "# (depth " << depth << ") " << endl;
    cout << start;
    for(const MovePosition& p : successors(test_directory, start))
      cout << " " << p;
    cout << endl;
    for(const MovePosition& p : successors(test_directory, start))
      recursive_successors_test(depth-1,p);
  }
}

const Value LOSS = static_cast<Value>(0x8001);
const Value DRAW = 1;

void backward_the_easy_way(Value *v){
  assert(*v);
  if ((*v)==DRAW) return;
  if((*v)<0)
    (*v)=-(*v);
  else
    (*v)=-((*v)-1);  //-4 replicates backward_the_hard_way
  // but -1 allows mate in 32000
}

bool update_table_entry(const Directory& dir, Table* table, const MovePosition& p){
  Value old_value=table->lookup(p);
#ifdef NDEBUG
  if(old_value)return false;
#else
  if(old_value==LOSS) return false;
  //also need to avoid going beyond stalemate XXX
#endif
  vector<MovePosition> succs=successors(dir, p);
  if(succs.size()==0){
    assert(old_value);  // loss or stalemate
    return false;
  }
  bool found_one=false;
  Value minimum;
  bool found_unknown=false;
  for(const MovePosition& s : succs){
    Value v=table->lookup(s);
    if(v==0)
      found_unknown=true;
    else if(found_one)
      minimum=min(minimum,v);
    else {
      minimum=v;
      found_one=true;
    }
  }
  if(!found_one){
    assert(!old_value);
    return false;
  }
  if(minimum>0 and found_unknown){
    assert(!old_value);
    return false;
  } else {
    backward_the_easy_way(&minimum);
    assert(minimum != LOSS);
#ifdef NDEBUG
    table->set(p,minimum);
    return true;
#else
    if(old_value==0){
      table->set(p,minimum);
      return true;
    } else {
      assert(old_value == minimum);
      return false;
    }
#endif
  }
}

#define setpos(n) p.position[n].from_numeric(i##n)
#define distinct(a,b) (i##a==ACTUAL_SIZE || i##b==ACTUAL_SIZE || i##a!=i##b)

unsigned long update_table(const Directory& dir, Table* table){
  unsigned long improved=0;
  MovePosition p;
  for(int player=0;player<=1;++player){
    p.to_move=static_cast<Color>(player);
    forR(i0) {
      setpos(0);
      forR(i1) {
        if(!distinct(1,0)) continue;
        setpos(1);
        forR(i2) {
          //assume always at least 2 pieces
          if (dir.size()<3 && i2!=ACTUAL_SIZE) continue;
          if(!distinct(2,0) || !distinct (2,1)) continue;
          setpos(2);
          forR(i3){
            if (dir.size()<4 && i3!=ACTUAL_SIZE) continue;
            if(!distinct(3,0) || !distinct(3,1) || !distinct(3,2)) continue;
            setpos(3);
            bool code = update_table_entry(dir,table,p);
            improved+=code;
          }
        }
      }
    }
  }
  return improved;
}

bool currently_the_other_player_has_a_king(const Directory& dir, MovePosition mp){
  mp.to_move=other(mp.to_move);
  return has_king(dir,mp);
}

// The player to move can capture the opponent's king / last royal piece.
bool illegal_position(const Directory& dir, const MovePosition& mp){
  assert(currently_the_other_player_has_a_king(dir,mp));
  for(const MovePosition& next : successors(dir,mp)){
    if(!has_king(dir,next))
      return true;
  }
  return false;
}

bool in_check(const Directory& dir, MovePosition mp){
  assert(has_king(dir,mp));
  mp.to_move=other(mp.to_move);
  return illegal_position(dir,mp);
}

bool no_legal_moves(const Directory& dir, const MovePosition& mp){
  for(const MovePosition& next: successors(dir,mp))
    if(!illegal_position(dir,next))
      return false;
  return true;
}

bool stalemate(const Directory& dir, const MovePosition& mp){
  if(!has_king(dir,mp)) return false;
  return (!in_check(dir,mp)) && no_legal_moves(dir,mp);
}

unsigned long mark_terminal_nodes(const Directory& dir,Table* table){
  unsigned long total=0;
  unsigned long numterminal=0;
  MovePosition p;
  for(int player=0;player<=1;++player){
    p.to_move=static_cast<Color>(player);
    forR(i0) {
      setpos(0);
      forR(i1){
        if(!distinct(1,0)) continue;
        setpos(1);
        forR(i2){
          //assume always at least 2 pieces
          if (dir.size()<3 && i2!=ACTUAL_SIZE) continue;
          if(!distinct(2,0) || !distinct (2,1)) continue;
          setpos(2);
          forR(i3) {
            if (dir.size()<4 && i3!=ACTUAL_SIZE) continue;
            if(!distinct(3,0) || !distinct(3,1) || !distinct(3,2)) continue;
            setpos(3);
            ++total;
            assert(table->lookup(p)==0);

            if(stalemate_draw && stalemate(dir,p)){
              table->set(p,DRAW);
              numterminal++;
            }else if(successors(dir,p).size()==0){
              table->set(p,LOSS);
              numterminal++;
              //cout << p << endl;
            }
          }
        }
      }
    }
  }
  cout << "#mark_terminal_nodes " << numterminal << " " << total << endl;
  return numterminal;
}

int main(int argc, char**argv){
  if(argc<2){
    cerr << "need args"<< endl;
    return 1;
  }
  cout << "#size = " << static_cast<int>(num_columns) << " " << static_cast<int>(num_rows) << endl;
  cout << "#stalemate_draw = " << stalemate_draw << endl;
  for(const Piece& p : test_directory)
    cout << "#" << p.toString() << endl;
  if(0==strcmp(argv[1],"exit")){
    return 0;
  } else if (0==strcmp(argv[1],"list")){
    cout << all_pieces.size() << endl;
  } else if (0==strcmp(argv[1],"locs")){
    //for(int p_index=0;p_index<static_cast<int>(sizeof(all_pieces)/sizeof(Piece));++p_index)
    //for_each(all_pieces.begin(), all_pieces.end(), [](Piece &p){ printlocs(p); });
    for(const Piece& p : all_pieces)
      printlocs(p);
  } else if (0==strcmp(argv[1],"printp")){
    for(const Piece& p : all_pieces){
      cout << p.toString();
      cout << "," << endl;
    }
  } else if (0==strcmp(argv[1],"arrtest")){
    cout << sizeof(Position) << endl;
  } else if (0==strcmp(argv[1],"colortest")){
    assert(other(White)==Black);
    assert(other(Black)==White);
  } else if (0==strcmp(argv[1],"succtest")){
    MovePosition start=gen_qr_test_position();
    int depth=1;
    if(argc>2)
      depth=atoi(argv[2]);
    recursive_successors_test(depth,start);
  } else if(0==strcmp(argv[1],"valuetest")){
    cout << LOSS << endl;
    Value i=LOSS, i2=LOSS;
    for(int j=0;j<163840;++j){  //Mate in 8000
      cout << j << " "  << i << " " << (i>>1) << " " << i2 << endl;
      //backward_the_hard_way(&i);
      backward_the_easy_way(&i2);
    }
  } else if(0==strcmp(argv[1],"terminal")){
    Table egtb(POSITION_POSSIBILITIES);
    mark_terminal_nodes(test_directory,&egtb);
    cout << egtb;
  } else if(0==strcmp(argv[1],"go")){
    Table egtb(POSITION_POSSIBILITIES);
    unsigned long running_sum=mark_terminal_nodes(test_directory,&egtb);
    unsigned long how_many_updated;
    while((how_many_updated=update_table(test_directory,&egtb))){
      running_sum+=how_many_updated;
      //cout << how_many_updated << endl;
    }
    cout << "#running_sum " << running_sum << endl;
    cout << egtb;
  }else {
    cerr << "unknown arg" << endl;
  }
}
