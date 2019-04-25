:- begin_tests(typeInf).
:- include(typeInf). 
:- dynamic(gvar/2).
:- dynamic(lvar/2).

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(mockedFor, [nondet]) :-
    typeStatement(oFor(v, T, int, int, unit ), unit),
    assertion(T==int).

test(inferWhile, [nondet]) :-
    infer([oWhile(<(X,Y), unit)], unit),
    assertion(X==int), assertion(Y==int).

test(mockedFuncLet, [nondet]) :-
    typeStatement(funcLet(v, float, fplus(X,Y), unit, [lvLet(zar, float, fplus(X,Y))]), unit).

test(mockedMatch, [nondet]) :-
    typeStatement(oMatch(v, float, float), float).

% test to infer If statements
test(inferIf, [nondet]) :-
    infer([oIf(<(X,Y), iplus(A,B), fToInt(C))], T),
    assertion(T==int), assertion(X==int), assertion(Y==int), assertion(A==int), assertion(B==int), assertion(C==float).

% test to infer If with Variable statements
test(inferVarIf, [nondet]) :-
    infer([gvLet(v, T, fplus(X, Y)), oIf(<.(X,Y), fToInt(X), fToInt(Y))], Z),
    assertion(T==float), assertion(X==float), assertion(Y==float), assertion(Z==int).

% test to infer If with Variable statements
test(inferFuncLVar, [nondet]) :-
    infer([funcLet(v, F, float, S, [fplus(X,Y)])], L).



:-end_tests(typeInf).
