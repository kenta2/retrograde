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

typedef pair <int8_t,int8_t> Coord;

ostream& operator<<(ostream &os, const Coord& c){
  os << '(' << static_cast<int>(c.first) << ',' << static_cast<int>(c.second) << ')';
  return os;
}

const Coord dir_orthogonal[]={Coord(0,1),Coord(1,0),Coord(-1,0),Coord(0,-1)};
const Coord dir_diagonal[]={Coord(1,1),Coord(1,-1),Coord(-1,1),Coord(-1,-1)};

const Coord dir_knight[]={Coord(1,2),Coord(-1,2),Coord(1,-2),Coord(-1,-2),
                          Coord(2,1),Coord(-2,1),Coord(2,-1),Coord(-2,-1)};

enum class Orthogonal {NoOrthogonal, Wazir, Rook};
enum class Diagonal {NoDiagonal, Ferz, Bishop};
enum class Dabbaba {NoDabbaba, Single, Rider};
enum class Knight {NoKnight, YesKnight};
enum class Alfil {NoAlfil, YesAlfil};

enum Color { White, Black};

struct Bitboard{
  vector<vector<uint8_t> > b;
  Bitboard(Coord dimensions)
    : b(dimensions.first, vector<uint8_t>(dimensions.second))

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
  for(unsigned i=b.b[0].size()-1;i>=0;--i){
    for(unsigned j=0;j<b.b[0].size();++j){
      os << setw(4) << static_cast<int>(b.index(Coord(j,i)));
    }
    os << endl << dec;
  }
  return os;
}

inline Coord add_coord(const Coord& a, const Coord& b){
  return Coord(a.first+b.first, a.second+b.second);
}

inline bool in_bounds(const Coord& sizes,const Coord& p){
  return (p.first>=0)&&(p.first<sizes.first)&&
    (p.second>=0)&&(p.second<sizes.second);
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
  vector<Coord> moves(const Coord& sizes, const Coord& mylocation, const Bitboard& board, const uint8_t mycolor) const {
    vector<Coord> answer;

    if(Orthogonal::Wazir == orthogonal)
      //answer.insert(answer.end(),dir_orth,dir_orth+4);
      for(int i=0;i<4;++i){
        Coord pos=add_coord(mylocation,dir_orthogonal[i]);
        if(in_bounds(sizes,pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }
    else if(Orthogonal::Rook == orthogonal)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;;++s){
          Coord pos = add_coord(mylocation, Coord(s*dir_orthogonal[i].first,s*dir_orthogonal[i].second));
          if(!in_bounds(sizes,pos)) break;
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
        if(in_bounds(sizes,pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }
    else if(Diagonal::Bishop == diagonal)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;;++s){
          Coord pos = add_coord(mylocation, Coord(s*dir_diagonal[i].first,s*dir_diagonal[i].second));
          if(!in_bounds(sizes,pos)) break;
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
        if(in_bounds(sizes,pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)) && (orthogonal != Orthogonal::Rook || !duplicate_entry(answer,pos)))
          answer.push_back(pos);
      }
    else if(Dabbaba::Rider == dabbaba)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;;++s){
          Coord pos = add_coord(mylocation, Coord(2*s*dir_orthogonal[i].first,2*s*dir_orthogonal[i].second));
          if(!in_bounds(sizes,pos)) break;
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
        if(in_bounds(sizes,pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }

    if(Alfil::YesAlfil == alfil)
      for(int i=0;i<4;++i){
        Coord pos=Coord(mylocation.first+2*dir_diagonal[i].first,
                        mylocation.second+2*dir_diagonal[i].second);
        if(in_bounds(sizes,pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)) && (diagonal != Diagonal::Bishop || !duplicate_entry(answer,pos)) )
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

const vector<Piece> all_pieces{
#include "all_pieces.cpp"
};

void printlocs(const Coord& sizes,const Piece& p){
  Bitboard b(sizes);
  for(int8_t i=0;i<sizes.first;++i)
    for(int8_t j=0;j<sizes.second;++j){
      assert(!b.occupied(Coord(i,j)));
    }
  for(int8_t i=0;i<sizes.first;++i)
    for(int8_t j=0;j<sizes.second;++j){
      vector<Coord> v = p.moves(sizes,Coord(i,j),b,0);
      //for_each(v.begin(), v.end(), [](Coord& c){}); //lambda function
      for(const Coord& c : v)
        cout << " " << static_cast<int>(c.first) <<" "<< static_cast<int>(c.second);
      cout << endl;
    }
}

class MaybeLocation {
  int value;
public:
  bool alive() const {
    return value;
  }
  Coord get(const Coord& sizes) const {
    assert(value);
    int temp=value-1;
    return Coord(temp/sizes.second, temp%sizes.second);
  }
  void set(const Coord& sizes, const Coord& l){
    value=1+static_cast<int>(l.first)*sizes.second+l.second;
  }
  void kill(){
    value=0;
  }
  int to_numeric() const {
    return value;
  }
  void from_numeric(const int input){
    value=input;
  }
};

ostream& operator<<(ostream& os, const MaybeLocation& object){
  os << object.to_numeric();
  return os;
}


typedef vector<Piece> Directory;

struct Parameters {
  Coord sizes;
  Directory dir;
  bool stalemate_draw;
};

Piece king(Color c){
  return Piece(Orthogonal::Wazir, Diagonal::Ferz, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, c, true);
}

const Directory dir_qr{king(White),king(Black),
    Piece(Orthogonal::Rook, Diagonal::Bishop, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, White, false),
    Piece(Orthogonal::Rook, Diagonal::NoDiagonal, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, Black, false)};

const Directory dir_n{king(White),king(Black),
    Piece(Orthogonal::NoOrthogonal, Diagonal::NoDiagonal, Knight::YesKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, White, false)};

const Directory dir_kmk{king(White),king(Black),
    Piece(Orthogonal::Wazir, Diagonal::Ferz, Knight::NoKnight, Alfil::NoAlfil, Dabbaba::NoDabbaba, White, false)};


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

ostream& operator<<(ostream& os, const Table& table){
  for(int player=0;player<=1;++player){
    for(unsigned i0=0;i0<table.table[0].size();++i0)
      for(unsigned i1=0;i1<table.table[0][i0].size();++i1)
        for(unsigned i2=0;i2<table.table[0][i0][i1].size();++i2)
          for(unsigned i3=0;i3<table.table[0][i0][i1][i2].size();++i3){
            Value v=table.table[player][i0][i1][i2][i3];
            if(v)
              os << player << " " << i0 << " " << i1 << " " << i2 << " " << i3 << " " << v << endl;
          }
  }
  return os;
}
#define forR(v) for(int v=0;v<position_possibilities;++v)


inline bool has_king(const Directory& dir, const MovePosition& mp){
  for(unsigned int i=0;i<dir.size();++i)
    if (dir[i].color==mp.to_move && dir[i].is_royal && mp.position[i].alive())
      return true;
  return false;
}

void fill_board(Bitboard *board, const Parameters& param, const Position& pos){
  for(unsigned int i=0;i<param.dir.size();++i)
    if(pos[i].alive()){
      Coord xy=pos[i].get(param.sizes);
      assert(board->b[xy.first][xy.second]==0);
      board->b[xy.first][xy.second]=1|(param.dir[i].color<<1)|(i<<2);
    }
}

Color other(Color x){
  return static_cast<Color>(1-x);
}

vector<MovePosition> successors(const Parameters& param, const MovePosition& mp){
  vector<MovePosition> answer;
  if(!has_king(param.dir,mp))
    return answer;
  Bitboard board(param.sizes);
  fill_board(&board,param,mp.position);
  for(unsigned i=0;i<param.dir.size();++i){
    if(param.dir[i].color == mp.to_move && mp.position[i].alive()){
      for(const Coord& newloc : param.dir[i].moves(param.sizes,mp.position[i].get(param.sizes), board, mp.to_move)){
        MovePosition next(mp);
        next.to_move= other(mp.to_move);
        next.position[i].set(param.sizes,newloc);
        if(board.occupied(newloc)) { // capture
          next.position[board.index(newloc)].kill();
        }
        answer.push_back(next);
      }
    }
  }
  return answer;
}

MovePosition gen_qr_test_position(const Coord& sizes){
  MovePosition answer;
  const int8_t pos_qr_arr[4][2]=
    //{{3,2},{2,0},{1,2},{1,1}};  //longest(4,3)
    {{0,0},{0,1},{1,0},{1,1}};
  answer.to_move=White;
  for(int i=0;i<4;++i){
    answer.position[i].set(sizes,Coord(pos_qr_arr[i][0],pos_qr_arr[i][1]));
  }
  return answer;
}

void recursive_successors_test(const Parameters& param, int depth, const MovePosition& start){
  if(depth==0)
    ; //cout << start << endl;
  else {
    cout << "# (depth " << depth << ") " << endl;
    cout << start;
    for(const MovePosition& p : successors(param, start))
      cout << " " << p;
    cout << endl;
    for(const MovePosition& p : successors(param, start))
      recursive_successors_test(param,depth-1,p);
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

bool update_table_entry(const Parameters& param, Table* table, const MovePosition& p){
  Value old_value=table->lookup(p);
#ifdef NDEBUG
  if(old_value)return false;
#else
  if(old_value==LOSS) return false;
  //also need to avoid going beyond stalemate XXX
#endif
  vector<MovePosition> succs=successors(param, p);
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
#define distinct(a,b) (i##a==0 || i##b==0 || i##a!=i##b)

unsigned long update_table(const Parameters& param, Table* table){
  unsigned long improved=0;
  MovePosition p;
  const int position_possibilities=1+param.sizes.first*param.sizes.second;
  for(int player=0;player<=1;++player){
    p.to_move=static_cast<Color>(player);
    forR(i0) {
      setpos(0);
      forR(i1) {
        if(!distinct(1,0)) continue;
        setpos(1);
        forR(i2) {
          //assume always at least 2 pieces
          if (param.dir.size()<3 && i2!=0) continue;
          if(!distinct(2,0) || !distinct (2,1)) continue;
          setpos(2);
          forR(i3){
            if (param.dir.size()<4 && i3!=0) continue;
            if(!distinct(3,0) || !distinct(3,1) || !distinct(3,2)) continue;
            setpos(3);
            bool code = update_table_entry(param,table,p);
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
bool illegal_position(const Parameters& param, const MovePosition& mp){
  assert(currently_the_other_player_has_a_king(param.dir,mp));
  for(const MovePosition& next : successors(param,mp)){
    if(!has_king(param.dir,next))
      return true;
  }
  return false;
}

bool in_check(const Parameters& param, MovePosition mp){
  assert(has_king(param.dir,mp));
  mp.to_move=other(mp.to_move);
  return illegal_position(param,mp);
}

bool no_legal_moves(const Parameters& param , const MovePosition& mp){
  for(const MovePosition& next: successors(param,mp))
    if(!illegal_position(param,next))
      return false;
  return true;
}

bool stalemate(const Parameters& param, const MovePosition& mp){
  if(!has_king(param.dir,mp)) return false;
  return (!in_check(param,mp)) && no_legal_moves(param,mp);
}

unsigned long mark_terminal_nodes(const Parameters& param,Table* table){
  unsigned long total=0;
  unsigned long numterminal=0;
  MovePosition p;
  const int position_possibilities=1+param.sizes.first*param.sizes.second;
  for(int player=0;player<=1;++player){
    p.to_move=static_cast<Color>(player);
    forR(i0) {
      setpos(0);
      forR(i1){
        if(!distinct(1,0)) continue;
        setpos(1);
        forR(i2){
          //assume always at least 2 pieces
          if (param.dir.size()<3 && i2!=0) continue;
          if(!distinct(2,0) || !distinct (2,1)) continue;
          setpos(2);
          forR(i3) {
            if (param.dir.size()<4 && i3!=0) continue;
            if(!distinct(3,0) || !distinct(3,1) || !distinct(3,2)) continue;
            setpos(3);
            ++total;
            assert(table->lookup(p)==0);

            if(param.stalemate_draw && stalemate(param,p)){
              table->set(p,DRAW);
              numterminal++;
            }else if(successors(param,p).size()==0){
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
  Parameters param;
  param.sizes=Coord(4,3);  //larger board sizes need to ulimit -s
  param.dir=dir_kmk;
  param.stalemate_draw=false;

  int8_t ACTUAL_SIZE=param.sizes.first*param.sizes.second;
  int8_t POSITION_POSSIBILITIES=ACTUAL_SIZE+1; // or piece is nowhere

  cout << "#size = " << static_cast<int>(param.sizes.first) << " " << static_cast<int>(param.sizes.second) << endl;
  cout << "#stalemate_draw = " << param.stalemate_draw << endl;
  for(const Piece& p : param.dir)
    cout << "#" << p.toString() << endl;
  if(0==strcmp(argv[1],"exit")){
    return 0;
  } else if (0==strcmp(argv[1],"list")){
    cout << all_pieces.size() << endl;
  } else if (0==strcmp(argv[1],"locs")){
    //for(int p_index=0;p_index<static_cast<int>(sizeof(all_pieces)/sizeof(Piece));++p_index)
    //for_each(all_pieces.begin(), all_pieces.end(), [](Piece &p){ printlocs(p); });
    for(const Piece& p : all_pieces)
      printlocs(param.sizes,p);
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
    MovePosition start=gen_qr_test_position(param.sizes);
    int depth=1;
    if(argc>2)
      depth=atoi(argv[2]);
    recursive_successors_test(param,depth,start);
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
    mark_terminal_nodes(param,&egtb);
    cout << egtb;
  } else if(0==strcmp(argv[1],"go")){
    Table egtb(POSITION_POSSIBILITIES);
    unsigned long running_sum=mark_terminal_nodes(param,&egtb);
    unsigned long how_many_updated;
    while((how_many_updated=update_table(param,&egtb))){
      running_sum+=how_many_updated;
      //cout << how_many_updated << endl;
    }
    cout << "#running_sum " << running_sum << endl;
    cout << egtb;
  }else {
    cerr << "unknown arg" << endl;
  }
}
