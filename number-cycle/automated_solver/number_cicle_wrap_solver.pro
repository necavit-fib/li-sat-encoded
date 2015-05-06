/*
 * number_cicle_wrap_solver.pro
 * 
 * Solver logic for the ciclic number wrapping problem.
 *
 * david.martinez.rodriguez@est.fib.upc.edu
 */

/*
 * PROBLEM
 * Given the number of rows and columns of the grid and some labeled
 * cells, with labels {0,1,2,3}, wrap the labeled cells with either
 * 1, 2, 3 or no edges around, connecting the edges to form closed
 * cicles with no overlappings. 
 * 
 * SOLUTION
 * Add constraints to write the edges of the labeled cells and
 * constraints to close the cicles:
 *     - Cell constraints: a cell labeled "k" has exactly k edges
 *         surrounding it
 *     - Cicle constraints: to close a cicle, we must draw, for every
 *         grid intersection, either 0 or 2 incoming segments, thus
 *         closing the cicle and avoiding overlaps.
 */

writeClauses :- cell_constraints, cicle_cross_constraints.

cell_constraints :-
	%for every cell (i,j) labeled k (given in the instance file) do:
	num(I,J,K), constrain_cell(I,J,K), fail.
cell_constraints.

constrain_cell(I,J,0) :- %cell labeled "0" -> no edges around it
	I1 is I + 1, J1 is J + 1,
	writeClause( [ \+h-I-J ] ),
	writeClause( [ \+h-I1-J ] ),
	writeClause( [ \+v-I-J ] ),
	writeClause( [ \+v-I-J1 ] ).
constrain_cell(I,J,K) :- %cell labeled "1"/"2"/"3" -> exactly 1/2/3 edges around it
	at_least_k_cell(I,J,K),
	at_most_k_cell(I,J,K).

%at least K edges of the cell are set
at_least_k_cell(I,J,K) :-
	%build a list with the surrounding segments of the cell
	I1 is I + 1, J1 is J + 1,
	L = [ h-I-J, h-I1-J, v-I-J, v-I-J1 ],
	%perform generic at least K with the list
	at_least_k_list(L,K).

%generic at least K over a list of variables	
at_least_k_list(L,K) :-
	length(L,NL),
	K1 is K - 1,
	K2 is NL - K1,
	subset(L,S),
	length(S,K2),
	writeClause(S).

%at most K edges of the cell are set
at_most_k_cell(I,J,K) :-
	%build a list with the surrounding segments of the cell
	I1 is I + 1, J1 is J + 1,
	L = [ h-I-J, h-I1-J, v-I-J, v-I-J1 ],
	%perform generic at most K with the list
	at_most_k_list(L,K).

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

cicle_cross_constraints :-
	rows(N), columns(M),
	%given a cross, 0 xor 2 segments can be set to true, no more, no less.
	N1 is N + 1, M1 is M + 1,
	between(1,N1,I), between(1,M1,J), %for i+1 rows and j+1 columns do:
	constrain_cross(I,J), fail.
cicle_cross_constraints.

constrain_cross(I,J) :- 
	obtain_edges(I,J,L), %store in L the list of valid variables of the cross,
	                     % taking grid borders into account
	constrain_0_or_2(L).

obtain_edges(I,J,L) :-
	%there are four edges in a cross or intersection:
	%   { h_i_j, h_i_j-1, v_i_j, v_i-1_j }
	% but some of them may be outside the borders of the grid,
	%  so we must not return them in the list
	I1 is I - 1, J1 is J - 1,
	obtain_h_edge(I,J,H1), %h_i_j
	append([],H1,L1),
	obtain_h_edge(I,J1,H2), %h_i_j-1
	append(L1,H2,L2),
	obtain_v_edge(I,J,V1), %v_i_j
	append(L2,V1,L3),
	obtain_v_edge(I1,J,V2), %v_i-1_j
	append(L3,V2,L).

%horizontal segments belong to the range: [1..N+1]x[1..M]
obtain_h_edge(I,J,[]) :- columns(M), (I < 1; J < 1; J > M).
obtain_h_edge(I,J,[h-I-J]).

%vertical segments belong to the range: [1..N]x[1..M+1]
obtain_v_edge(I,J,[]) :- rows(N), (I < 1; J < 1; I > N).
obtain_v_edge(I,J,[v-I-J]).

constrain_0_or_2(L) :-
	%if at most 2 variables are set, then neither 3 nor 4 can be true!
	% we are thus restraining the setting of 3 or 4 segments.
	at_most_k_list(L,2),
	%in order to forbid setting only 1 segment, we add a clause that
	% states that if 1 segment is set, then another one must be set
	constrain_1(L).

constrain_1(L) :-
	%generate subsets of L
	subset(L,S),
	%we pick a representative from the list
	length(S,1),
	%we get the rest of the elements of the list
	rest_list(L,S,R),
	%we write an implication clause: S -> (R1 v R2 v ... RN)
	% thus forcing: if 1 variable is set, at least another is set too,
	% which means that either 0 or at least 2 variables are set.
	write_implication_clause(S,R).

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
 * number_cicle_wrap_solver.pro 
 */
