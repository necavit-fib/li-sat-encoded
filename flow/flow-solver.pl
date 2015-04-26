/*
 * flow-solver.pl
 *
 * Solver logic for the Flow Free game problem
 *
 * david.martinez.rodriguez@est.fib.upc.edu
 *
 * April, 2015
 */

/*
 * SOLUTION
 */

writeClauses :-
	assertColors,
	assertSourcesAndSinks,
	colorConstraints,
	successorConstraints.

%stores all the possible colors in the knowledge database
assertColors :-
	c(Color,   _, _,   _, _),
	assert(color(Color)),
	fail.
assertColors.

%stores all the source and sink cells in the knowledge database
assertSourcesAndSinks :-
	c(_,   Xsource, Ysource,   Xsink, Ysink),
	assert(source(Xsource, Ysource)),
	assert(sink(Xsink, Ysink)),
	fail.
assertSourcesAndSinks.

colorConstraints :-
	constrainOneColorPerCell, %ALO and AMO color per cell
	constrainSourcesAndSinks. %sinks and sources have a predefined color

constrainOneColorPerCell :-
	size(N), %given a board of size N
	between(1,N,I), between(1,N,J), %for each cell <i,j>
	atLeastOneColorPerCell(I,J),
	atMostOneColorPerCell(I,J),
	fail.
constrainOneColorPerCell.

atLeastOneColorPerCell(I,J) :-
	%all the combinations with the colors existing in the knowledge base
	findall(col-I-J-Color, color(Color), Clause),
	writeClause(Clause).

atMostOneColorPerCell(I,J) :-
	%all the colors existing in the knowledge base are stored in the Colors list
	findall(Color, color(Color), Colors),
	length(Colors, Length),
	between(0, Length, K1), between(0, Length, K2),
	K1 < K2,
	nth0(K1, Colors, C1),
	nth0(K2, Colors, C2),
	%standard, naïve, n^2 AMO encoding
	writeClause([ \+col-I-J-C1, \+col-I-J-C2]).

constrainSourcesAndSinks :-
	%enforce the color for both sources and sinks
	c(Color,   Xsource, Ysource,   Xsink, Ysink),
	writeClause([ col-Xsource-Ysource-Color ]),
	writeClause([ col-Xsink-Ysink-Color ]),
	fail.
constrainSourcesAndSinks.

successorConstraints :-
	size(N), %given a board of size N
	between(1,N,X), between(1,N,Y), %for each cell <x,y>
	constrainCellSuccessors(X,Y),
	fail.
successorConstraints.

constrainCellSuccessors(X,Y) :-
	\+sink(X,Y), %the cell must not be a sink
	successors(X,Y,Successors),!,
	at_most_k_list(Successors,1),
	at_least_k_list(Successors,1),
	constrainColorOfSuccessorsList(Successors),
	constrainSuccessorCycles(Successors).

constrainColorOfSuccessorsList(Successors) :-
	member(s-X-Y-I-J, Successors),
	findall(Color, color(Color), Colors),
	member(C, Colors),
	writeClause([ \+s-X-Y-I-J, \+col-X-Y-C, col-I-J-C ]).

constrainSuccessorCycles([]).
constrainSuccessorCycles([s-X-Y-I-J|L]) :-
	writeClause([ \+s-X-Y-I-J, \+s-I-J-X-Y ]),
	constrainSuccessorCycles(L).

%retrieves the list of possible successors of a given cell, taking into account
%  the board boundaries and source cells (which cannot be the successor of anyone)
successors(X,Y,Successors) :-
	XN is X - 1, YN is Y,
	northSuccessor(X,Y, XN,YN, North), append([], North, L1),

	XE is X, YE is Y + 1,
	eastSuccessor(X,Y, XE,YE,  East),  append(L1, East, L2),

	XS is X + 1, YS is Y,
	southSuccessor(X,Y, XS,YS, South), append(L2, South, L3),

	XW is X, YW is Y - 1,
	westSuccessor(X,Y, XW,YW,  West),  append(L3, West, Successors).

%the north successor cannot be a source cell nor out of bounds (X in [1,N])
northSuccessor(X,Y, I,J, [s-X-Y-I-J]) :-
	I > 0,
	\+source(I, J).
northSuccessor(_,_, _,_, []).

%the south successor cannot be a source cell nor out of bounds (X in [1,N])
southSuccessor(X,Y, I,J, [s-X-Y-I-J]) :-
	size(N),
	I =< N,
	\+source(I,J).
southSuccessor(_,_, _,_, []).

%the west successor cannot be a source cell nor out of bounds (Y in [1,N])
westSuccessor(X,Y, I,J, [s-X-Y-I-J]) :-
	J > 0,
	\+source(I,J).
westSuccessor(_,_, _,_, []).

%the east successor cannot be a source cell nor out of bounds (Y in [1,N])
eastSuccessor(X,Y, I,J, [s-X-Y-I-J]) :-
	size(N),
	J =< N,
	\+source(I,J).
eastSuccessor(_,_, _,_, []).

/* *********************************************************************************** */
/* *********************************************************************************** */

%generic at least K over a list of variables
at_least_k_list(L,K) :-
	length(L,NL),
	K1 is K - 1,
	K2 is NL - K1,
	subset(L,S),
	length(S,K2),
	writeClause(S).

%generic at most K over a list of variables
at_most_k_list(L,K) :-
	K1 is K + 1,
	subset(L,S),
	length(S,K1), %take all subsets of length K+1
	negate_list(S,C), %make all literals of S negative and store in C
	writeClause(C).

%subset(L,S) stores all subsets of L in S
subset([],[]).
subset([X|S],[X|Ss]) :- subset(S,Ss).
subset([_|S],Ss) :- subset(S,Ss).

%negate_list(L,N) stores in N all the negated terms of L
negate_list([],[]).
negate_list([V-I-J|L],C) :- negate_list(L,N), C = [\+V-I-J|N].
negate_list([\+V-I-J|L],C) :- negate_list(L,N), C = [V-I-J|N].

%use to get those elements of L different from the only one in [S]
rest_list([],_,[]).
rest_list([X|L],[S],[X|R]) :- X \= S, rest_list(L,[S],R).
rest_list([X|L],[S],R) :- X = S, rest_list(L,[S],R).

write_implication_clause(H,T) :-
	%head (H) implies tail (T), and we know [(H -> T) == (¬H v T)], therefore:
	negate_list(H,N),
	append(N,T,C),
	writeClause(C).

/* END OF:
 * flow-solver.pl
 */
