#include <vector>
#include <utility>
#include <cstdint>
#include <algorithm>
#include <iostream>
#include <cstring>
#include <cassert>
#include <string>

using namespace std;

//typedef vector< pair <int8_t,int8_t> > Coords;
//  dir_orth.push_back(pair<int8_t,int8_t>(0,1));

typedef pair <int8_t,int8_t> Coord;

Coord dir_orthogonal[]={Coord(0,1),Coord(1,0),Coord(-1,0),Coord(0,-1)};
Coord dir_diagonal[]={Coord(1,1),Coord(1,-1),Coord(-1,1),Coord(-1,-1)};

Coord dir_knight[]={Coord(1,2),Coord(-1,2),Coord(1,-2),Coord(-1,-2),
                    Coord(2,1),Coord(-2,1),Coord(2,-1),Coord(-2,-1)};

enum class Orthogonal {NoOrthogonal, Wazir, Rook};
enum class Diagonal {NoDiagonal, Ferz, Bishop};
enum class Dabbaba {NoDabbaba, Dabbaba_single, Dabbaba_rider};
enum class Knight {NoKnight, YesKnight};
enum class Alfil {NoAlfil, YesAlfil};

const int8_t num_rows=4;
const int8_t num_columns=4;
const int8_t board_size_max = max(num_rows, num_columns);

class Bitboard{
  uint8_t b[num_rows][num_columns];
public:
  Bitboard() : b {0}
  {}
  bool occupied(const Coord& p) const {
    return b[p.first][p.second];
  }
  uint8_t color_at(const Coord& p) const {
    return b[p.first][p.second]>>1;
  }
};

inline Coord add_coord(const Coord& a, const Coord& b){
  return Coord(a.first+b.first, a.second+b.second);
}

inline bool in_bounds(const Coord& p){
  return (p.first>=0)&&(p.first<num_rows)&&
    (p.second>=0)&&(p.second<num_columns);
}

enum Color { White, Black};

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


    if(Dabbaba::Dabbaba_single == dabbaba)
      for(int i=0;i<4;++i){
        Coord pos=Coord(mylocation.first+2*dir_orthogonal[i].first,
                        mylocation.second+2*dir_orthogonal[i].second);
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
          answer.push_back(pos);
      }
    else if(Dabbaba::Dabbaba_rider == dabbaba)
      for(int i=0;i<4;++i)
        for(uint8_t s=1;s<board_size_max;++s){
          Coord pos = add_coord(mylocation, Coord(2*s*dir_orthogonal[i].first,2*s*dir_orthogonal[i].second));
          if(!in_bounds(pos)) break;
          if(board.occupied(pos)){
            if(board.color_at(pos)!=mycolor)
              answer.push_back(pos);
            break;
          } else
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
        if(in_bounds(pos) && (!board.occupied(pos) || (board.color_at(pos)!=mycolor)))
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
  } else {
    cerr << "unknown arg" << endl;
  }
}
