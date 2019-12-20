:- use_module( [library(lists), io]).
% Author : LiangLiang Zheng

/*

license from https://github.com/996icu/996.ICU

                  Developers' lives matter 
Copyright (c) <year> <copyright holders>

"Anti 996" License 

Permission is hereby granted to any individual or legal entity
obtaining a copy of this licensed work (including the source code,
documentation and/or related items, hereinafter collectively referred
to as the "licensed work"), free of charge, to deal with the licensed
work for any purpose, including without limitation, the rights to use,
reproduce, modify, prepare derivative works of, distribute, publish 
and sublicense the licensed work, subject to the following conditions:

1. The individual or the legal entity must conspicuously display,
without modification, this License and the notice on each redistributed 
or derivative copy of the Licensed Work.

2. The individual or the legal entity must strictly comply with all
applicable laws, regulations, rules and standards of the jurisdiction
relating to labor and employment where the individual is physically
located or where the individual was born or naturalized; or where the
legal entity is registered or is operating (whichever is stricter). In
case that the jurisdiction has no such laws, regulations, rules and
standards or its laws, regulations, rules and standards are
unenforceable, the individual or the legal entity are required to
comply with Core International Labor Standards.

3. The individual or the legal entity shall not induce, metaphor or force
its employee(s), whether full-time or part-time, or its independent
contractor(s), in any methods, to agree in oral or written form, to
directly or indirectly restrict, weaken or relinquish his or her
rights or remedies under such laws, regulations, rules and standards
relating to labor and employment as mentioned above, no matter whether
such written or oral agreement are enforceable under the laws of the
said jurisdiction, nor shall such individual or the legal entity
limit, in any methods, the rights of its employee(s) or independent
contractor(s) from reporting or complaining to the copyright holder or
relevant authorities monitoring the compliance of the license about
its violation(s) of the said license.

THE LICENSED WORK IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN ANY WAY CONNECTION WITH THE
LICENSED WORK OR THE USE OR OTHER DEALINGS IN THE LICENSED WORK.

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% two player, one named 1, another is 2                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
player1(1).
player2(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_player1/1, player2/1 succeeds when its argument is the player tom or   %
%  jerry in the representation.                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_player1(Player1) :- 
      player1(Player1).

is_player2(Player2) :- 
      player2(Player2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is_merel/1 succeeds when its argument is either a player characters       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_merel(Merel) :- 
      is_player1(Merel).

is_merel(Merel) :- 
      is_player2(Merel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succeeds when two players(arguments) are different characters             % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
different(Player1, Player2) :- 
      \+ (Player1 = Player2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succeeds when both its arguments are player representation characters, but%
% they are different.                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
other_player(Player1, Player2) :- 
      is_merel(Player1), is_merel(Player2), different(Player1, Player2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succeeds when its first argument is a pair made up of its second, a point,%
% and its third, a merel.                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pair(pair(Point, Merel), Point, Merel) :-
      is_merel(Merel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succeeds when its first argument is a merel/point pair and its second is  %
% a representation of the merel positions on the board.                     %                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merel_on_board(Point_Merel, Board) :- 
      pair(Point_Merel, Point, Merel), is_merel(Merel), member(pair(Point, Merel), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succeeds when its three arguments are (in order) a connected row          %
% there are 16 different possibilities in legitimae_row(X,Y,Z)              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
legitimate_row(a, b, c).
legitimate_row(d, e, f).
legitimate_row(g, h, i).
legitimate_row(j, k, l).
legitimate_row(m, n, o).
legitimate_row(p, q, r).
legitimate_row(s, t, u).
legitimate_row(v, w, x).
legitimate_row(a, j, v).
legitimate_row(d, k, s).
legitimate_row(g, l, p).
legitimate_row(b, e, h).
legitimate_row(q, t, w).
legitimate_row(i, m, r).
legitimate_row(f, n, u).
legitimate_row(c, o, x).

row(First_point, Second_point, Third_point) :- 
      legitimate_row(First_point, Second_point, Third_point).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% succeeds when its two arguments are the names of points on the board which%
% are connected by a line                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(First, Second) :- 
      legitimate_row(First,Second,_).
connected(First, Second) :- 
      legitimate_row(_, First, Second).
connected(First, Second) :-
      legitimate_row(Second, First, _).
connected(First, Second) :-
      legitimate_row(_, Second, First).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% initialize the board                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_board([pair(a,a), pair(b,b), pair(c,c), pair(d,d), pair(e,e), pair(f,f),
               pair(g,g), pair(h,h), pair(i,i), pair(j,j), pair(k,k), pair(l,l),
               pair(m,m), pair(n,n), pair(o,o), pair(p,p), pair(q,q), pair(r,r),
               pair(s,s), pair(t,t), pair(u,u), pair(v,v), pair(w,w), pair(x,x)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% and_the_winner_is/2 succeeds when its first argument represents a         %
% board, and the second  is a player who has won on that board.             %
%   Ways of Losing:                                                         %
%   (1).  The opponent has no more than 3 merels to move                    %
%   (2).  The opponent can not move anymore                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% findall(+Template, +Goal, -List) 
not_enough_merel(Board, Player) :- 
      findall(Player, merel_on_board(pair(_, Player), Board) , L), length(L, N), N < 3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get all the point of the corresponding player and find if there are       %
% avaible connected place                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
can_move(Board, Player) :- 
      merel_on_board(pair(PointforPlayer, Player), Board), 
      connected(PointwithoutPlayer, PointforPlayer), 
      empty_point(PointwithoutPlayer, Board), 
      \+ merel_on_board((PointwithoutPlayer, _), Board).

and_the_winner_is(Board, Player) :- 
      other_player(Player, Opponent ), not_enough_merel(Board, Opponent ), report_winner(Player).

and_the_winner_is(Board, Player) :-
      other_player(Player, Opponent ),  \+ can_move(Board, Opponent ), report_winner(Player). 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Running a game for 2 human players                                      %
%   It has three arguments: the number of merels not yet placed, a player,  %
%   and a board state                                                       %
%   There are 3 possibilities                                               %
%   (1). All merels have been replaced, board represents winning state,     %
%        have to report a winner                                            %
%   (2). Not all merels have been placeed, get legal placing from player    %
%        named in argument 1, fill the point he gives, check for new mills, %
%        ask which to move, display, switch players then play again         %
%        and update board and new player.                                   %
%   (3). All merels have been placed, can get a legal move from the players %
%        named in argument 1, move merel, check for new mills, ask which    %
%        piece to remove, display the board, switch player and player again %
%        and update board and new player.                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


three_row_situation(Point, OtherPoint1, OtherPoint2) :-
      row(Point, OtherPoint1, OtherPoint2).

three_row_situation(Point, OtherPoint1, OtherPoint2) :-
      row(OtherPoint1, Point, OtherPoint2).

three_row_situation(Point, OtherPoint1, OtherPoint2) :-
      row(OtherPoint1, OtherPoint2, Point).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this two below can be used in section 5: heuristic place                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

three_row_situation(Point, OtherPoint1, OtherPoint2) :-
      row(OtherPoint2, OtherPoint1, Point).

three_row_situation(Point, OtherPoint1, OtherPoint2) :-
      row(OtherPoint2, Point, OtherPoint1).

check_mill(Point, Player, Board_updated, Board_after_remove) :-
      three_row_situation(Point, OtherPoint1, OtherPoint2),
      merel_on_board(pair(OtherPoint1, Player), Board_updated),
      merel_on_board(pair(OtherPoint2, Player), Board_updated),
      report_mill(Player),
      get_remove_point( Player, Point_remove, Board_updated ),
      select(pair(Point_remove, _), Board_updated, List_remove_1),
      append(List_remove_1, [pair(Point_remove, Point_remove)], Board_after_remove),
      display_board(Board_after_remove).

check_mill(_Point, _Player, Board_updated, Board_after_remove) :-
      findall(Pair, member(Pair, Board_updated), Board_after_remove),
      display_board(Board_after_remove).

decrease_one(NumberofMerel, NumberofMerelLeft) :-
      NumberofMerelLeft is (NumberofMerel - 1).


all_merel_been_placed_report(NumberofMerel, Player, Board) :-
      NumberofMerel is 0,
      and_the_winner_is(Board, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not available, then delete one pair and append a new one.                 %
% having mill and not have mill are mutually exclusive, so                  %
% cut is using here                                                         %
% multual excluded                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_all_merel_been_placed(NumberofMerel, Player, Board, Board_after_remove, NumberofMerelLeft) :- 
      NumberofMerel > 0,
      get_legal_place(Player, Point, Board),
      decrease_one(NumberofMerel, NumberofMerelLeft),
      % select(remove) pair and remove it, append a new one
      select(pair(Point, Point), Board, List1),
      append(List1, [pair(Point, Player)], Board_updated),
      %display_board(Board_updated),
      check_mill(Point, Player, Board_updated, Board_after_remove).

all_merel_been_placed_but_can_move(NumberofMerel, Player, Board, Board_after_remove) :-
      NumberofMerel is 0,
      get_legal_move( Player, OldPoint, NewPoint, Board ),
      % delete the oldpoint and add the new point
      select(pair(OldPoint, Player), Board, List1),
      append(List1, [pair(NewPoint, Player)], Board_updated),
      check_mill(NewPoint, Player, Board_updated, Board_after_remove).

/* 

code for section 3.6
play(NumberofMerel, Player, Board) :-
      all_merel_been_placed_report(NumberofMerel, Player, Board).


play(NumberofMerel, Player, Board) :-
      not_all_merel_been_placed(NumberofMerel, Player, Board, Board_updated, NumberofMerelLeft),
      other_player(Player, Opponent ),
      play(NumberofMerelLeft, Opponent , Board_updated).

play(NumberofMerel, Player, Board) :-
      all_merel_been_placed_but_can_move(NumberofMerel, Player, Board, Board_updated),
      other_player(Player, Opponent ),
      play(NumberofMerel, Opponent , Board_updated).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Section 4.Running a game for 1 human and the computer                   %
%       (1). Only adds is_player1 based on section 3.6 when human plays     %
%       (2). When is comes to Player 2, then choose a move or replacemeent  %
%            heuristically                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_mill_auto_remove(Point, Player, Board_updated, Board_after_remove) :-
      three_row_situation(Point, OtherPoint1, OtherPoint2),
      merel_on_board(pair(OtherPoint1, Player), Board_updated),
      merel_on_board(pair(OtherPoint2, Player), Board_updated),
      report_mill(Player),
      other_player(Player, Opponent),
      choose_remove( Opponent, Point_remove, Board_updated),
      select(pair(Point_remove, _), Board_updated, List_remove_1),
      append(List_remove_1, [pair(Point_remove, Point_remove)], Board_after_remove),
      display_board(Board_after_remove),
      format( 'The point ~w has been removed \n', [Point_remove] ).

check_mill_auto_remove(_Point, _Player, Board_updated, Board_after_remove) :-
      findall(Pair, member(Pair, Board_updated), Board_after_remove),
      display_board(Board_after_remove).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Section 5.Implementing heuristics                                       %
%   (1). Intuitively computer will do all it can do to made mills           %
%        So the priority of the making mills action ranked first,           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Section 5.Implementing heuristics                                       %
%   (1). 4 different kinds of heuristics for choose place                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% At the beginning of the game, it is more important to place pieces in     %
% versatile  locations rather than to try to form mills immediately and make%
% the mistake of concentrating one's pieces in one area of the board.       %
% An ideal position, which typically results in a win, allows a player to   %
% shuttle one piece back and forth between two mills, removing a piece      %
% every turn.(From wikipedia)                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% choose a point that have more connection, according to wikipedia          %
% with four points connected, can be listed eas                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

four_connected(e).
four_connected(k).
four_connected(n).
four_connected(t).

three_connected(h).
three_connected(l).
three_connected(q).
three_connected(m).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% something can be optimized is that when opponent is trying make           % 
% trap: put one in the middle, in the next row(right or left)               %
% form a two merel and can be connected with the middle one in the          %
% upper levle                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%choose a point that can form mill
heuristics_strategy_place(Player, Point, Board) :-
      merel_on_board(pair(OtherPoint1, Player), Board),
      merel_on_board(pair(OtherPoint2, Player), Board),
      \+ OtherPoint1 = OtherPoint2,
      three_row_situation(Point, OtherPoint1, OtherPoint2),
      empty_point( Point, Board ),
      \+ merel_on_board(pair(Point, _), Board).

% or prevent your opponent from forming mills
heuristics_strategy_place(Player, Point, Board) :-
      other_player(Player, Opponent),
      merel_on_board(pair(OtherPoint1, Opponent), Board),
      merel_on_board(pair(OtherPoint2, Opponent), Board),
      \+ OtherPoint1 = OtherPoint2,
      three_row_situation(Point, OtherPoint1, OtherPoint2),
      empty_point( Point, Board ),
      \+ merel_on_board(pair(Point, _), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if there are merels where is an intersection once your opponent put merels%
% in it then it forms 2 ways of mill, then put it in and prevent it,        %
% it's meaning if there is strictly                                         %
% 2 ways, if one of them have already been placed by Player, then ignore it.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heuristics_strategy_place(Player, Point, Board) :-
      other_player(Player, Opponent),
      merel_on_board(pair(Point1, Opponent), Board),
      merel_on_board(pair(Point2, Opponent), Board),
      % this two Points are connected with a point which is an empty point without merel
      \+ Point1 = Point2,
      connected(Point, Point1), 
      connected(Point, Point2),
      three_row_situation(Point, Point1, Point3),
      three_row_situation(Point, Point2, Point4),
      empty_point(Point3, Board),
      empty_point(Point4, Board),
      empty_point(Point, Board),
      \+ merel_on_board(pair(Point3, _), Board),
      \+ merel_on_board(pair(Point4, _), Board),
      \+ merel_on_board(pair(Point, _), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if there are merels where you can put merels in the intersection          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

heuristics_strategy_place(Player, Point, Board) :-
      merel_on_board(pair(Point1, Player), Board),
      merel_on_board(pair(Point2, Player), Board),
      % this two Points are connected with a point which is an empty point without merel
      \+ Point1 = Point2,
      connected(Point, Point1), 
      connected(Point, Point2),
      empty_point(Point, Board),
      \+ merel_on_board(pair(Point, _), Board).

heuristics_strategy_place(_Player, Point, Board) :-
      four_connected(Point),
      % this point has not been placed
      empty_point(Point, Board),
      \+ merel_on_board(pair(Point, _), Board).

% choose with three points connected
heuristics_strategy_place(_Player, Point, Board) :-
      three_connected(Point),
      empty_point(Point, Board),
      \+ merel_on_board(pair(Point, _), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Only one merel, place another need to modify to place pieces in           %
% versatile locations                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
heuristics_strategy_place(Player, Point, Board) :-
      merel_on_board(pair(OtherPoint1, Player), Board),
      connected(Point, OtherPoint1),
      empty_point(Point, Board),
      \+ merel_on_board(pair(Point, _), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Section 5.Implementing heuristics                                       %
%   (1). choose remove                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% if there are 2 different rows which are about to form mill, 
% remove the intersection of those
% I think remove can be optimzed a little bit later
choose_remove( Player, Point, Board ) :-
      % remove that point which is about to form a mill
      merel_on_board(pair(OtherPoint1, Player), Board),
      merel_on_board(pair(OtherPoint2, Player), Board),
      \+ OtherPoint1 = OtherPoint2,
      three_row_situation(EmptyPoint, OtherPoint1, OtherPoint2),
      empty_point( EmptyPoint, Board ),
      \+ merel_on_board( pair(EmptyPoint, _), Board),
      Point = OtherPoint2.

choose_remove( Player, Point, Board ) :-
      pair( Pair, Point, Player ),
      merel_on_board( Pair, Board ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Section 5.Implementing heuristics                                       %
%   (1). 4 different kinds of heuristics for moving                         %
% if there is no mill in the board now, and only move 1 step can form       %
% this mill                                                                 % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_move( Player, OldPoint, NewPoint, Board ) :-
      % find mill
      three_row_situation(Point, OtherPoint1, OtherPoint2),
      merel_on_board(pair(OtherPoint1, Player), Board),
      merel_on_board(pair(OtherPoint2, Player), Board),
      connected(Point, OldPoint),
      \+ merel_on_board(pair(Point, _), Board),
      \+ OldPoint = OtherPoint1,
      \+ OldPoint = OtherPoint2,
      merel_on_board(pair(OldPoint, Player), Board),
      NewPoint = Point.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if there is already a mill, and there is a empty point to move one of the %
% merel out, then do it back and forth                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_move( Player, OldPoint, NewPoint, Board ) :-
      % find mill
      three_row_situation(Point1, Point2, Point3),
      merel_on_board(pair(Point1, Player), Board),
      merel_on_board(pair(Point2, Player), Board),
      merel_on_board(pair(Point3, Player), Board),
      connected(Point1, NewPoint), 
      empty_point(NewPoint, Board),
      \+ merel_on_board(pair(NewPoint, _), Board),
      Point1 = OldPoint.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% need 2 steps to form a mill                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_move(Player, OldPoint, NewPoint, Board) :-
      three_row_situation(Point, OtherPoint1, OtherPoint2),
      merel_on_board(pair(OtherPoint1, Player), Board),
      merel_on_board(pair(OtherPoint2, Player), Board),
      \+ merel_on_board(pair(Point, _), Board),
      connected(Point, NewPoint),
      \+ NewPoint = OtherPoint1,
      \+ NewPoint = OtherPoint2,
      \+ merel_on_board(pair(NewPoint, _), Board),
      connected(OldPoint, NewPoint),
      merel_on_board(pair(OldPoint, Player), Board).

choose_move( Player, OldPoint, NewPoint, Board ) :-
      pair( pair(OldPoint, Player), OldPoint, Player ),
      merel_on_board( pair(OldPoint, Player), Board ),
      connected( OldPoint, NewPoint ),
      \+ merel_on_board(pair(NewPoint, _), Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not all the merel have been placed(for computer), first choose a point    %
% decrease number of merel, then place it on the board, then check if it    %
% forms a mill.                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not_all_merel_been_placed_computer(NumberofMerel, Player, Board, Board_after_remove, NumberofMerelLeft) :- 
      NumberofMerel > 0,
      % choose a point that can form mill
      heuristics_strategy_place(Player, Point, Board),
      decrease_one(NumberofMerel, NumberofMerelLeft),
      % select(remove) pair and remove it, append a new one
      select(pair(Point, Point), Board, List1),
      append(List1, [pair(Point, Player)], Board_updated),
      format( 'Player2 placed to point  ~w \n', [Point] ),
      %display_board(Board_updated),
      check_mill_auto_remove(Point, Player, Board_updated, Board_after_remove),
      !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% all the merel have been placed(for computer), first choose a point        %
% to move, then check if it forms a mill                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_merel_been_placed_but_can_move_computer(NumberofMerel, Player, Board, Board_after_remove) :-
      NumberofMerel is 0,
      write('test1'),
      choose_move( Player, OldPoint, NewPoint, Board ),
      % delete the oldpoint and add the new point
      select(pair(OldPoint, Player), Board, List1),
      append(List1, [pair(NewPoint, Player)], Board_updated),
      format( 'Player 2 move from ~w to point  ~w \n', [OldPoint, NewPoint] ),
      check_mill_auto_remove(NewPoint, Player, Board_updated, Board_after_remove),
      !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% play/3 is recursive. It has three arguments: the number of merels not yet %
% placed, a player. First is to check whether there is a winner in the game %
% if not, the next two player is for player 1, not_all_merel_been_placed and%
% all_merel_been_placed_but_can_move respectively, the following 2 after    %
% player1 is for computer (which is player 2)                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play(NumberofMerel, Player, Board) :-
      all_merel_been_placed_report(NumberofMerel, Player, Board).

play(NumberofMerel, Player, Board) :-
      is_player1(Player),
      not_all_merel_been_placed(NumberofMerel, Player, Board, Board_updated, NumberofMerelLeft),
      other_player(Player, Opponent ),
      play(NumberofMerelLeft, Opponent , Board_updated).

play(NumberofMerel, Player, Board) :-
      is_player1(Player),
      all_merel_been_placed_but_can_move(NumberofMerel, Player, Board, Board_updated),
      other_player(Player, Opponent ),
      play(NumberofMerel, Opponent , Board_updated).

play(NumberofMerel, Player, Board) :-
      is_player2(Player), 
      not_all_merel_been_placed_computer(NumberofMerel, Player, Board, Board_updated, NumberofMerelLeft),
      other_player(Player, Opponent ),
      play(NumberofMerelLeft, Opponent , Board_updated).

play(NumberofMerel, Player, Board) :-
      is_player2(Player), 
      all_merel_been_placed_but_can_move_computer(NumberofMerel, Player, Board, Board_updated),
      other_player(Player, Opponent ),
      play(NumberofMerel, Opponent , Board_updated).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assume that player 1 is always going to start. We will use a predicate    %
% called play/0 to begin a game                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
play :- welcome,
      initial_board( Board ),
      display_board( Board ),
      is_player1( Player ),
      play(18, Player, Board).