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

%given the number of rows N and columns M:
rows(N).
columns(M).

writeClauses :- cell_constraints, cicle_cross_constraints.

cell_constraints :-
	%for every cell (i,j) labeled k (given in the instance file)
	num(I,J,K), constrain_cell(I,J,K), fail.
cell_constraints.

constrain_cell(I,J,0) :- %cell labeled "0" -> no edges around it
	I1 is I + 1, J1 is J + 1,
	writeClause( [ \+h-I-J, \+h-I1-J, \+v-I-J, \+v-I-J1 ] ).
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
	%TODO length(L,NL), subset(L,S),
	%TODO K1 is K + 1 - NL, length(S, K1), %take care of the NL
	%TODO writeClause(S).

%at most K edges of the cell are set
at_most_k_cell(I,J,K) :-
	%build a list with the surrounding segments of the cell
	I1 is I + 1, J1 is J + 1,
	L = [ h-I-J, h-I1-J, v-I-J, v-I-J1 ],
	%perform generic at most K with the list
	at_most_k_list(L,K).

%generic at most K over a list of variables
at_most_k_list(L,K) :-
	subset(L,S),
	K1 is K + 1, length(S,K1), %take all subsets of length K+1
	negate_list(S,C), %make all literals of S negative and store in C
	writeClause(C), fail.	

%subset(L,S) stores all subsets of L in S
subset([],[]).
subset([X|S],[X|Ss]) :- subset(S,Ss).
subset([_|S],Ss) :- subset(S,Ss).

%negate_list(L,N) stores in N all the negated terms of L
negate_list([],[]).
negate_list([V-I-J|L],C) :- negate_list(L,N), C = [\+V-I-J|N].
negate_list([\+V-I-J|L],C) :- negate_list(L,N), C = [V-I-J|N].


cicle_cross_constraints :-
	%given a cross, 0 xor 2 segments can be set to true, no more, no less.
	N1 is N + 1, M1 is M + 1,
	between(1,N1,I), between(1,M1,J), %for i+1 rows and j+1 columns do:
	constrain_cross(I,J), fail.
cicle_cross_constraints.

constrain_cross(I,J) :- 
	obtain_edges(I,J,L), %store in L the list of valid variables of the cross,
	                     % taking grid borders into account
	constrain_0_or_2(L).

%TODO obtain_edges(I,J,L).

%TODO constrain_0_or_2(L).

/* END OF:
 * number_cicle_wrap_solver.pro 
 */
