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
	graphConstraints.

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

graphConstraints :-
	size(N), %given a board of size N
	between(1,N,X), between(1,N,Y), %for each cell <x,y>
	constrainPath(X,Y),
	fail.
graphConstraints.

constrainPath(X,Y):-
	\+sink(X,Y),
	\+source(X,Y),
	inwardPaths(X,Y,In),
	at_most_k_list(In,1),
	at_least_k_list(In,1),
	outwardPaths(X,Y,Out),
	at_most_k_list(Out,1),
	at_least_k_list(Out,1).
constrainPath(X,Y):-
	\+sink(X,Y),
	inwardPaths(X,Y,In),
	at_most_k_list(In,0),
	outwardPaths(X,Y,Out),
	at_most_k_list(Out,1),
	at_least_k_list(Out,1).
constrainPath(X,Y):-
	\+source(X,Y),
	inwardPaths(X,Y,In),
	at_most_k_list(In,1),
	at_least_k_list(In,1),
	outwardPaths(X,Y,Out),
	at_most_k_list(Out,0).

outwardPaths(X,Y,Out):-
	XN is X - 1, YN is Y,
	northOut(X,Y, XN,YN, North), append([], North, L1),

	XE is X, YE is Y + 1,
	eastOut(X,Y, XE,YE,  East),  append(L1, East, L2),

	XS is X + 1, YS is Y,
	southOut(X,Y, XS,YS, South), append(L2, South, L3),

	XW is X, YW is Y - 1,
	westOut(X,Y, XW,YW,  West),  append(L3, West, Out).

%the north successor cannot be a source cell nor out of bounds (X in [1,N])
northOut(X,Y, I,J, [s-X-Y-I-J]) :- I > 0.
northOut(_,_, _,_, []).

%the south successor cannot be a source cell nor out of bounds (X in [1,N])
southOut(X,Y, I,J, [s-X-Y-I-J]) :- size(N), I =< N.
southOut(_,_, _,_, []).

%the west successor cannot be a source cell nor out of bounds (Y in [1,N])
westOut(X,Y, I,J, [s-X-Y-I-J]) :-	J > 0.
westOut(_,_, _,_, []).

%the east successor cannot be a source cell nor out of bounds (Y in [1,N])
eastOut(X,Y, I,J, [s-X-Y-I-J]) :- size(N), J =< N.
eastOut(_,_, _,_, []).

inwardPaths(X,Y,In):-
	XN is X - 1, YN is Y,
	northIn(X,Y, XN,YN, North), append([], North, L1),

	XE is X, YE is Y + 1,
	eastIn(X,Y, XE,YE,  East),  append(L1, East, L2),

	XS is X + 1, YS is Y,
	southIn(X,Y, XS,YS, South), append(L2, South, L3),

	XW is X, YW is Y - 1,
	westIn(X,Y, XW,YW,  West),  append(L3, West, In).

%the north successor cannot be a source cell nor out of bounds (X in [1,N])
northIn(X,Y, I,J, [s-I-J-X-Y]) :- I > 0.
northIn(_,_, _,_, []).

%the south successor cannot be a source cell nor out of bounds (X in [1,N])
southIn(X,Y, I,J, [s-I-J-X-Y]) :- size(N), I =< N.
southIn(_,_, _,_, []).

%the west successor cannot be a source cell nor out of bounds (Y in [1,N])
westIn(X,Y, I,J, [s-I-J-X-Y]) :-	J > 0.
westIn(_,_, _,_, []).

%the east successor cannot be a source cell nor out of bounds (Y in [1,N])
eastIn(X,Y, I,J, [s-I-J-X-Y]) :- size(N), J =< N.
eastIn(_,_, _,_, []).

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
