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

% test to infer function output
test(inferFuncF, [nondet]) :-
    infer([funcLet(v, F, float, S, [fplus(X,Y)])], L),
    assertion(L==float), assertion(F==float), assertion(S==float), assertion(X==float), assertion(Y==float).

% test to infer function with local let output
test(inferFuncLVar, [nondet]) :-
    infer([funcLet(v, F, float, S, [lvLet(x, float, float), print(_)])], L),
    assertion(L==unit), assertion(F==float), assertion(S==unit).

% test match
test(inferMatch, [nondet]) :-
    infer([oMatch(v, float, float), float], D),
    assertion(D==float).

% test for
test(inferFor, [nondet]) :-
    infer([oFor(v, T, int, int, unit ), unit], W),
    assertion(T==int), assertion(W==unit).

% test two local variables
test(inferFuncLVarLVar, [nondet]) :-
    infer([funcLet(v, F, float, S, [lvLet(x, float, float), lvLet(y, int, int)])], L),
    assertion(F==float), assertion(S==unit), assertion(L==unit).

% Test Global Variable in IF While
test(inferGvarIfWhile, [nondet]) :-
    infer([oFor(v, T, int, int, unit ), unit], W),
    assertion(T==int), assertion(W==unit).

% Test type of function through If
test(grandInfer, [nondet]):-
    infer([funcLet(v, F, int, S, [oIf(<.(X, Y), fToInt(X), fToInt(Y))])], W),
    assertion(F==int), assertion(S==int), assertion(X==float), assertion(Y==float), assertion(W==int).

% Test Global Variable and If and Match
test(inferMatch, [nondet]):-
    infer([gvLet(v, T, fplus(X, Y)), oIf(<.(X,Y), fToInt(X), fToInt(Y))], Z),
    infer([funcLet(ve, F, int, S, [oMatch(v, int, int)])], W),
    assertion(F==int), assertion(S==int), assertion(X==float), assertion(Y==float), assertion(W==int), assertion(T==float), assertion(Z==int).

test(infergvLet, [nondet]):-
    infer([gvLet(v, T, fplus(X, Y)), oIf(<.(X,Y), fToInt(X), fToInt(Y))], Z),
    assertion(T==float), assertion(Z==int), 
    gvar(v, int).

:-end_tests(typeInf).
