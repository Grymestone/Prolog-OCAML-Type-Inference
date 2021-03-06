/* match functions by unifying with arguments 
    and infering the result
*/
:-dynamic(gvar/2).
:-dynamic(lvar/2).

typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

typeStatement(lvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(lvar(Name, T)). /* add definition to database */


typeStatement(oIf(Cond, TCode, FCode), T) :- 
    typeExp(Cond, bool), /* Will this parse the conditional?S*/
    typeExp(TCode, T),  /* Parse the first conditional */
    typeExp(FCode, T), /* Parse the second conditional */
    bType(T). /* Return boolean? Maybe this should be unit. */

typeStatement(oFor(Var, T, Start, End, Code), unit) :-
    atom(Var),
    typeExp(Start, T),
    typeExp(End, T),
    bType(T), /* Parse begin val */
    typeExp(Code, unit). /* Return what needs to be returned based on input type */

typeStatement(oWhile(Cond, Code), T) :-
    typeExp(Cond, bool), /* Parse name of conditional */
    bType(T), /* Fetch type of val */
    typeExp(Code, unit). /* Return what needs to be returned based on input type */

typeStatement(funcLet(Name, T, TCode, S, Code), S) :-
    atom(Name), /* Parse name of Function*/
    typeExp(TCode, T), /* Parse params*/
    typeCode(Code, S), /* Parse function code*/
    bType(S),
    bType(T), /* Parse return type*/
    retractall(lvar(_, _)),
    asserta(gvar(Name, T)). /* add function name to global vars*/

typeStatement(oMatch(Name, Code, T), T) :-
    atom(Name),
    /*I feel like we should check for the variable's type and then */
    typeExp(Code, T).    



/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([], T) :- bType(T).
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).
typeCode([S, S2|Code], T):-
    typeExp(S,_T),
    typeCode([S2|Code], T).
typeCode([E], T):-typeExp(E, T).
% typeCode(E, T) :- typeExp(E, T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).
deleteLVars():-retractall(lvar), asserta(lvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).
fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(iToString, [int, string]).
fType(fToString, [float, string]).
fType(stringTof, [string, float]).
fType(stringToi, [string, int]).
fType(print, [_X, unit]). /* simple print */
fType('<', [int, int, bool]).
fType('>', [int, int, bool]).
fType('<.', [float, float, bool]).
fType('<.', [float, float, bool]).
/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

functionType(Name, Args):-
    lvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
lvar(_, _) :- false().
