:-include(sud22).
symbolicOutput(1).
:-dynamic(varNumber/3).

writeClauses :- write_filled, at_least_one_cell, at_most_one_row, at_most_one_column, at_most_one_subgrid.

write_filled :-
	filled(I,J,K), writeClause([x-I-J-K]), fail.
write_filled.

at_least_one_cell :-
	between(1,9,I), between(1,9,J),
	findall(x-I-J-K, between(1,9,K), C),
	writeClause(C), fail.
at_least_one_cell.

at_most_one_row :-
	between(1,9,J), between(1,9,K), between(1,8,I),
		X is I + 1, between(X,9,L),
	writeClause( [ \+x-I-J-K, \+x-L-J-K ] ), fail.
at_most_one_row.

at_most_one_column :-
	between(1,9,I), between(1,9,K), between(1,8,J),
		X is J + 1, between(X,9,L),
	writeClause( [ \+x-I-J-K, \+x-I-L-K ] ), fail.
at_most_one_column.

at_most_one_subgrid :- amo_subgrid_first_condition, amo_subgrid_second_condition.

amo_subgrid_first_condition :-
	between(1,9,K), between(0,2,X), between(0,2,Y),
		between(1,3,I), between(1,3,J),
		Z is J + 1, between(Z,3,L),
	I1 is 3 * X + I, J1 is 3 * Y + J,
	I2 is 3 * X + I, J2 is 3 * Y + L,
	writeClause( [ \+x-I1,J1,K, \+x-I2,J2,K ] ), fail.
amo_subgrid_first_condition.

amo_subgrid_second_condition :-
	between(1,9,K), between(0,2,X), between(0,2,Y),
		between(1,3,I), between(1,3,J),
		Z is I + 1, between(Z,3,L),
		between(1,3,M),
	I1 is 3 * X + I, J1 is 3 * Y + J,
	I2 is 3 * X + L, J2 is 3 * Y + M,
	writeClause( [ \+x-I1-J1-K, \+x-I2-J2-K ] ), fail.
amo_subgrid_second_condition.

/*
 * displayed solution will be in the form: "i j k" meaning:
 *  cell in the i-th row and j-th column has value k
 */
displaySol([]).
displaySol( [NumericVariable|Solution] ) :-
	num2var(NumericVariable, x-I-J-K), %get the x_ijk symbolic variable
	write(I), write(' '), write(J), write(' '), write(K), write(' '), write(-1), nl,
	displaySol(Solution).


% ========== No need to change the following: =====================================

main:- symbolicOutput(1), !, writeClauses, halt. % escribir bonito, no ejecutar
main:-  assert(numClauses(0)), assert(numVars(0)),
	tell(clauses), writeClauses, told,
	tell(header),  writeHeader,  told,
	unix('cat header clauses > infile.cnf'),
	unix('picosat -v -o model infile.cnf'),
	unix('cat model'),
	see(model), readModel(M), seen,	displaySol(M),
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
