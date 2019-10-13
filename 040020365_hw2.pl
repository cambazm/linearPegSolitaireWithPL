% BLG435E-AI Fall/2005 HW#2
% Student Number: 040020365
% Student Name: Mehmet CAMBAZ
%-------------------------------------------------------------------------
% Linear Peg Solitaire (one dimensional peg solitaire)

% in the lists, b is BLACK, w is WHITE, - is the empty place

% generates all moves available and inserts to MoveList list
gen_all_moves(X,Movelist) :- 
	gen_all_white_moves(X,WhiteMoves),
      gen_all_black_moves(X,BlackMoves),
	append(WhiteMoves,BlackMoves,Movelist).

% generates all white moves available and inserts to WhiteMoves list
gen_all_white_moves(X,WhiteMoves) :- 
	gen_all_white_slides(X,WhiteSlides),
	gen_all_white_jumps(X,WhiteJumps),
	append(WhiteSlides,WhiteJumps,WhiteMoves).

% generates all black moves available and inserts to BlackMoves list
gen_all_black_moves(X,BlackMoves) :- 
	gen_all_black_slides(X,BlackSlides),
	gen_all_black_jumps(X,BlackJumps),
	append(BlackSlides,BlackJumps,BlackMoves).

% generates all white sliding moves available
gen_all_white_slides(Board,Newboards) :- 
      all_replacements(Board,[w,-],[-,w],Newboards).

% generates all white jumping moves available
gen_all_white_jumps(Board,Newboards) :-
      all_replacements(Board,[w,b,-],[-,b,w],Newboards).

% generates all black sliding moves available
gen_all_black_slides(Board,Newboards) :-
      all_replacements(Board,[-,b],[b,-],Newboards).

% generates all black jumping moves available
gen_all_black_jumps(Board,Newboards) :-
	all_replacements(Board,[-,w,b],[b,w,-],Newboards).

% edits all available replacements on board without any duplicates
all_replacements(Board,Oldseq,Newseq,Y) :-
	bagof(X,replace_subseq(Board,Oldseq,Newseq,X),Y).

all_replacements(Board,Oldseq,Newseq,[]).

replace_subseq(Oldseq,Oldsubseq,Newsubseq,Newseq) :- 
	append(L1,L2,Oldseq),
	append(Oldsubseq,L3,L2),
	append(Newsubseq,L3,L4),
	append(L1,L4,Newseq).

% appends to a list
append([],Z,Z).
append([A|X],Y,[A|Z]) :- append(X,Y,Z).

% code to here is based on the example code at "http://www.cs.ubc.ca/~eiselt/cs322/lectures/lec30pegpuzzlepl.html"


% goal states for N = 5 to 1

goal([b, b, b, b, b, -, w, w, w, w, w]).
goal([b, b, b, b, -, w, w, w, w]).
goal([b, b, b, -, w, w, w]).
goal([b, b, -, w, w]).
goal([b, -, w]).


% starts the program

start :-
    	nl,write('Please enter N: ( 0 < N < 6 )'),nl,
    	read(N),nl,
	
	solve(N,Start),						% for N, suitable Start state is returned

	depthfirstsearchforgoal(Start, PathToGoal),		% from start to goal, path to goal is returned on Path variable
	writeln('SOLUTION'),nl,
    	showboardstate(PathToGoal),				% with showboardstate predicate, path to goal list is shown to user as understandable output
	length(PathToGoal,Len),					% path to goal list's length is assigned to Len variable
	Steps is Len - 1,					% total number of steps is Steps
	nl,write('Total Number of Steps = '),
	writeln(Steps),nl.


% predicate assigns Start state for the number of pegs

solve(1,Start) :-
    	writeln('N = 1'),nl, 
	Start = [w, -, b].

solve(2,Start) :-
    	writeln('N = 2'),nl, 
	Start = [w, w, -, b, b].

solve(3,Start) :-
    	writeln('N = 3'),nl, 
	Start = [w, w, w, -, b, b, b].
  
solve(4,Start) :-
    	writeln('N = 4'),nl, 
	Start = [w, w, w, w, -, b, b, b, b].

solve(5,Start) :-
    	writeln('N = 5'),nl, 
	Start = [w, w, w, w, w, -, b, b, b, b, b].

solve(X,Start) :-
	writeln('Unrecognizable input, N is set to 1'),nl,
	Start = [w, -, b].



% predicate to show board states in the solution path
    
showboardstate([]).				% if empty list comes do nothing (base case)
showboardstate([Head|Tail]) :- 
    	writelist(Head),nl,			% write the list's elements one by one for head
    	showboardstate(Tail).			% write the list's elements one by one for tail of the given list (recursive case)



% predicate for searching for the goal with dfs approach

depthfirstsearchforgoal(State, [State|RestOfStates]):- goal(State).
depthfirstsearchforgoal(State, [State|RestOfStates]):- gen_all_moves(State, [Head|Tail]), depthfirstsearchforgoal(Head, RestOfStates).


% predicate for writing all elements in a list one by one on the screen

writelist([]).				     % empty list will not be shown on screen
writelist([-|T]) :- write('_'),writelist(T). % empty place on the board will be shown as '_'
writelist([b|T]) :- write('B'),writelist(T). % black peg (b) is shown as 'B'
writelist([w|T]) :- write('W'),writelist(T). % white peg (w) is shown as 'W'

