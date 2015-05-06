rows(40).
columns(40).
num(1,6,1).
num(1,15,3).
num(1,25,2).
num(1,27,3).
num(1,39,2).
num(2,11,2).
num(2,30,3).
num(2,35,2).
num(3,10,0).
num(3,13,1).
num(4,1,2).
num(4,3,0).
num(4,10,1).
num(4,14,2).
num(4,19,0).
num(4,37,0).
num(5,2,1).
num(5,8,2).
num(5,12,3).
num(5,19,2).
num(5,39,3).
num(6,13,2).
num(6,15,0).
num(6,17,2).
num(6,22,1).
num(6,29,1).
num(6,34,0).
num(6,35,2).
num(7,23,1).
num(7,26,1).
num(7,33,1).
num(7,37,3).
num(8,2,1).
num(8,5,2).
num(8,9,2).
num(8,24,0).
num(8,33,0).
num(8,38,1).
num(9,3,1).
num(9,4,3).
num(9,8,2).
num(9,18,2).
num(9,27,1).
num(9,37,0).
num(10,15,2).
num(10,18,1).
num(10,22,2).
num(10,26,1).
num(11,3,3).
num(11,21,0).
num(11,27,3).
num(11,28,1).
num(11,31,2).
num(11,36,2).
num(11,38,0).
num(12,9,0).
num(12,21,1).
num(12,27,3).
num(12,40,2).
num(13,22,2).
num(13,26,0).
num(13,34,2).
num(14,8,0).
num(14,12,2).
num(14,18,2).
num(14,22,3).
num(14,36,0).
num(15,6,3).
num(15,15,3).
num(15,26,2).
num(16,12,2).
num(16,13,0).
num(16,37,1).
num(17,12,1).
num(17,15,2).
num(17,34,3).
num(17,35,0).
num(18,8,3).
num(19,1,0).
num(19,14,3).
num(19,26,3).
num(19,29,1).
num(20,3,3).
num(20,9,3).
num(20,12,1).
num(20,22,2).
num(20,28,0).
num(20,31,3).
num(21,26,0).
num(21,27,1).
num(22,12,1).
num(22,36,0).
num(23,5,1).
num(23,20,1).
num(23,37,1).
num(23,40,0).
num(24,7,1).
num(24,10,1).
num(24,16,2).
num(25,19,1).
num(25,22,0).
num(26,2,2).
num(26,4,3).
num(26,6,0).
num(26,28,2).
num(26,38,3).
num(27,5,1).
num(27,18,3).
num(27,27,0).
num(28,5,0).
num(28,8,3).
num(28,10,0).
num(28,12,1).
num(28,16,0).
num(28,17,0).
num(28,22,3).
num(29,11,1).
num(29,14,0).
num(29,23,3).
num(30,2,1).
num(30,16,3).
num(30,29,2).
num(30,32,3).
num(31,9,0).
num(31,17,1).
num(32,2,3).
num(32,7,0).
num(32,13,2).
num(32,23,1).
num(32,34,2).
num(32,40,3).
num(33,2,1).
num(33,6,3).
num(33,30,0).
num(33,33,2).
num(34,3,3).
num(34,13,0).
num(34,15,3).
num(34,29,1).
num(34,33,1).
num(35,5,0).
num(35,15,2).
num(35,19,1).
num(35,24,2).
num(35,31,1).
num(36,20,1).
num(36,26,2).
num(36,29,3).
num(37,10,0).
num(37,18,2).
num(38,1,1).
num(38,21,0).
num(38,22,3).
num(39,4,0).
num(39,6,3).
num(39,38,1).
num(40,24,2).
num(40,32,1).
num(40,34,2).
num(40,35,0).
symbolicOutput(1).
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
writeHeaderPS:-
    writeln('%!PS'),
    writeln('matrix currentmatrix /originmat exch def'),
    writeln('/umatrix {originmat matrix concatmatrix setmatrix} def'),
    writeln('[28.3465 0 0 28.3465 10.5 100.0] umatrix').

writeGrid:-
    writeln('0.01 setlinewidth'),
    writeVertGrid,
    writeHorizGrid.

writeVertGrid:-
    rows(R), columns(C), C1 is C+1,
    between(1,R,I), between(1,C1,J), drawVertical(I,J),fail.
writeVertGrid.

writeHorizGrid:-
    rows(R), columns(C), R1 is R+1,
    between(1,R1,I), between(1,C,J), drawHorizontal(I,J),fail.
writeHorizGrid.

drawVertical(I,J):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size,
    Y is 23-(I-1)*Size,
    write(X), write(' '), write(Y), write(' moveto'),nl,
    Y1 is Y-Size,
    write(X), write(' '), write(Y1), write(' lineto'),nl,
    writeln('stroke').

drawHorizontal(I,J):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size,
    Y is 23-(I-1)*Size,
    write(X), write(' '), write(Y), write(' moveto'),nl,
    X1 is X+Size,
    write(X1), write(' '), write(Y), write(' lineto'),nl,
    writeln('stroke').

writeNumbers:-
    num(I,J,K),
    writeNumber(I,J,K),
    fail.
writeNumbers.

writeNumber(I,J,K):-
    rows(R),columns(C),
    Size is min(22/R,18/C),
    X is 1+(J-1)*Size + 3*Size/7,
    Y is 23-(I-1)*Size - 5*Size/7,
    writeln('0.001 setlinewidth'),
    S is Size/2,
    write('/Times-Roman findfont '), write(S), writeln(' scalefont setfont'),
    write(X), write(' '), write(Y), write(' moveto ('), write(K), writeln(') show').

writeSolution([X|M]):-
    writeLine(X),
    writeSolution(M).
writeSolution([]).
    
writeLine(X):-num2var(X,h-I-J),!,
    rows(R), columns(C), T is max(R,C),
    W is 2/T,
    write(W), 
    writeln(' setlinewidth'),
    drawHorizontal(I,J).
writeLine(X):-num2var(X,v-I-J),!,
    rows(R), columns(C), T is max(R,C),
    W is 2/T,
    write(W), 
    writeln(' setlinewidth'),
    drawVertical(I,J).
writeLine(_).

displaySol(M):-
    tell('graph.ps'),
    writeHeaderPS,
    writeGrid,
    writeNumbers,
    writeSolution(M),
    writeln('showpage'),
    told.
:-dynamic(varNumber/3).
% ========== No need to change the following: =====================================
main:- symbolicOutput(1), !, writeClauses, halt. % escribir bonito, no ejecutar
main:-  assert(numClauses(0)), assert(numVars(0)),
	tell(clauses), writeClauses, told,
	tell(header),  writeHeader,  told,
	unix('cat header clauses > infile.cnf'),
	unix('picosat -v -o model infile.cnf'),
	unix('rm header'), unix('rm clauses'), unix('rm infile.cnf'),
	see(model), readModel(M), seen,
	unix('rm model'),
	displaySol(M),
	halt.

var2num(T,N):- hash_term(T,Key), varNumber(Key,T,N),!.
var2num(T,N):- retract(numVars(N0)), N is N0+1, assert(numVars(N)), hash_term(T,Key),
	assert(varNumber(Key,T,N)), assert( num2var(N,T) ), !.

writeHeader:- numVars(N),numClauses(C),write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-  retract(numClauses(N)), N1 is N+1, assert(numClauses(N1)),!.
writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.
unix(Comando):-shell(Comando),!.
unix(_).

readModel(L):- get_code(Char), readWord(Char,W), readModel(L1), addIfPositiveInt(W,L1,L),!.
readModel([]).

addIfPositiveInt(W,L,[N|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, !.
addIfPositiveInt(_,L,L).

readWord(99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!.
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================
