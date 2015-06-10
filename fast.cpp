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

const int8_t num_rows=4;
const int8_t num_columns=4;

//typedef vector< pair <int8_t,int8_t> > Coords;
//  dir_orth.push_back(pair<int8_t,int8_t>(0,1));

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
enum class Dabbaba {NoDabbaba, Dabbaba_single, Dabbaba_rider};
enum class Knight {NoKnight, YesKnight};
enum class Alfil {NoAlfil, YesAlfil};

const int8_t board_size_max = max(num_rows, num_columns);

enum Color { White, Black};

class Bitboard{
public:
  uint8_t b[num_rows][num_columns];

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
      //os << setw(4) << hex << static_cast<int>(b.b[i][j]);
      os << setw(4) << static_cast<int>(b.index(Coord(i,j)));
    }
    os << endl << dec;
  }
  return os;
}

inline Coord add_coord(const Coord& a, const Coord& b){
  return Coord(a.first+b.first, a.second+b.second);
}

inline bool in_bounds(const Coord& p){
  return (p.first>=0)&&(p.first<num_rows)&&
    (p.second>=0)&&(p.second<num_columns);
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
    if(Dabbaba::Dabbaba_single == dabbaba)
      for(int i=0;i<4;++i){
        Coord pos=Coord(mylocation.first+2*dir_orthogonal[i].first,
                        mylocation.second+2*dir_orthogonal[i].second);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)) && (orthogonal != Orthogonal::Rook || !duplicate_entry(answer,pos)))
          answer.push_back(pos);
      }
    else if(Dabbaba::Dabbaba_rider == dabbaba)
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
    else if(Dabbaba::Dabbaba_single == dabbaba)
      answer+="Dabbaba_single";
    else if(Dabbaba::Dabbaba_rider == dabbaba)
      answer+="Dabbaba_rider";
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
  for(int8_t i=0;i<num_rows;++i)
    for(int8_t j=0;j<num_columns;++j){
      assert(!b.occupied(Coord(i,j)));
    }
  for(int8_t i=0;i<num_rows;++i)
    for(int8_t j=0;j<num_columns;++j){
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
    if(alive)
      return static_cast<int>(l.first)*num_columns+l.second;
    else
      return static_cast<int>(num_rows)*num_columns;
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

Directory test_directory(dir_qr);

const int NUM_PIECES=4;
typedef MaybeLocation Position[NUM_PIECES];

class MovePosition {
public:
  Color to_move;
  Position position;
};

ostream& operator<<(ostream& os, const MovePosition& object){
  os << static_cast<int>(object.to_move);
  for(int i=0;i<NUM_PIECES;++i)
  os << " " << object.position[i];
  return os;
}

inline bool has_king(const Directory& dir, const MovePosition& mp){
  for(int i=0;i<NUM_PIECES;++i)
    if (dir[i].color==mp.to_move && dir[i].is_royal && mp.position[i].alive)
      return true;
  return false;
}

void fill_board(Bitboard *board, const Directory& dir, const Position& pos){
  for(int i=0;i<NUM_PIECES;++i)
    if(pos[i].alive)
      board->b[pos[i].get().first][pos[i].get().second]=1|(dir[i].color<<1)|(i<<2);
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
  for(int i=0;i<NUM_PIECES;++i){
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
  const int8_t pos_qr_arr[4][2]={{3,3},{1,0},{2,2},{0,1}};
  // const int8_t pos_qr_arr[4][2]={{3,3},{1,0},{2,0},{0,1}};
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
};

int main(int argc, char**argv){
  if(argc<2){
    cerr << "need args"<< endl;
    return 1;
  }
  if(0==strcmp(argv[1],"list")){
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
  }else {
    cerr << "unknown arg" << endl;
  }
}
