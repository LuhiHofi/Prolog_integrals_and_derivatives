% Author: Lukáš Hofman
% Description: A program that calculates integrals and derivatives of functions.
%   I am using library created by Jakub Smolík that simplifies mathematical expressions.
%   integrating compound functions is not supported as the expression can be without an integral,
%   aswell as integrating by substitution is not supported as it's challenging to choose what to substitute for. 

:- ensure_loaded('Expression-simplification/simplify.pl').
% Load simple.pl because simplify doesn't take care of fractions and that way i get plenty of recursion errors.
:- ensure_loaded('simple.pl').


% ==========================================================
% substitute(+F, +X, +V, -F1) :- F1 is the result of
%                      replacing X with V in F
% ==========================================================

% Base case: If the term is a variable and matches X, replace it with V
substitute(X, X, V, V) :- !.
% If the term is a constant or a different variable, keep it unchanged
substitute(Term, _, _, Term) :- atomic(Term).
% in case of goniometrical functions, subsitute only the argument
substitute(sin(Term), X, V, sin(R)) :- substitute(Term, X, V, R), !.
substitute(cos(Term), X, V, cos(R)) :- substitute(Term, X, V, R), !.
substitute(tan(Term), X, V, tan(R)) :- substitute(Term, X, V, R), !.
substitute(cot(Term), X, V, cot(R)) :- substitute(Term, X, V, R), !.
substitute(sec(Term), X, V, sec(R)) :- substitute(Term, X, V, R), !.
substitute(csc(Term), X, V, csc(R)) :- substitute(Term, X, V, R), !.
substitute(arcsin(Term), X, V, arcsin(R)) :- substitute(Term, X, V, R), !.
substitute(arccos(Term), X, V, arccos(R)) :- substitute(Term, X, V, R), !.
substitute(arctan(Term), X, V, arctan(R)) :- substitute(Term, X, V, R), !.
substitute(arccot(Term), X, V, arccot(R)) :- substitute(Term, X, V, R), !.
substitute(arcsec(Term), X, V, arcsec(R)) :- substitute(Term, X, V, R), !.
substitute(arccsc(Term), X, V, arccsc(R)) :- substitute(Term, X, V, R), !.
% in case of logaritmic functions, subsitute only the argument
substitute(ln(Term), X, V, ln(R)) :- substitute(Term, X, V, R), !.
substitute(log(Term), X, V, log(R)) :- substitute(Term, X, V, R), !.
% other special cases
% abs
substitute(abs(Term), X, V, abs(R)) :- substitute(Term, X, V, R), !.
% power
substitute(exp(Term), X, V, exp(R)) :- substitute(Term, X, V, R), !.
% sqrt
substitute(sqrt(Term), X, V, sqrt(R)) :- substitute(Term, X, V, R), !.
% in case of negative Term, substitute only the argument
substitute(-Term, X, V, -R) :- substitute(Term, X, V, R), !.

% If the term is an operator, recursively apply substitute to its arguments
substitute(Term1 + Term2, X, V, R) :-
    substitute(Term1, X, V, R1),
    substitute(Term2, X, V, R2),
    R = R1 + R2, !.
substitute(Term1 - Term2, X, V, R) :-
    substitute(Term1, X, V, R1),
    substitute(Term2, X, V, R2),
    R = R1 - R2, !.
substitute(Term1 * Term2, X, V, R) :-
    substitute(Term1, X, V, R1),
    substitute(Term2, X, V, R2),
    R = R1 * R2, !.
substitute(Term1 / Term2, X, V, R) :-
    substitute(Term1, X, V, R1),
    substitute(Term2, X, V, R2),
    R = R1 / R2, !.
substitute(Term1^Term2, X, V, R) :-
    substitute(Term1, X, V, R1),
    substitute(Term2, X, V, R2),
    R = R1^R2, !.
substitute(A^Term, X, V, R) :-
    atom(A),
    substitute(Term, X, V, R1),
    R = A^R1, !.

% ==========================================================
% contains(+X, +Term) :- checks if the term contains
%                      the variable X
% ==========================================================
contains(X, X) :- !.
contains(X, Term) :-
    compound(Term),
    Term =.. [_|Args],
    member(Arg, Args),
    contains(X, Arg),
    !.

% ==========================================================
% derive(+Function,+X,-Result) derive is a helper 
% function for derivative
% ==========================================================

derive(X,X,1).
derive(Y,X,0) :- \+ contains(X, Y).
derive(-Y,X,-Z) :- derive(Y,X,Z).
derive(X^2,X,2*X).
derive(X^N, X, N*X^N1) :- number(N), N1 is N-1.
derive(X^A,X,A*X^(A-1)) :- atom(A).
derive(1/X,X,-1/X^2).

% goniometrical functions
derive(sin(X),X,cos(X)).	
derive(cos(X),X,-sin(X)).
derive(tan(X),X,1/cos(X)^2).
derive(cot(X),X,(-1/sin(X)^2)).
derive(sec(X),X,sec(X)*tan(X)).
derive(csc(X),X,-csc(X)*cot(X)).
% inverse goniometrical functions
derive(arcsin(X),X,1/sqrt(1-X^2)).
derive(arccos(X),X,(-1/sqrt(1-X^2))).
derive(arctan(X),X,1/(1+X^2)).
derive(arccot(X),X,(-1/(1+X^2))).
derive(arcsec(X),X,1/(abs(X)*sqrt(X^2-1))).
derive(arccsc(X),X,(-1/(abs(X)*sqrt(X^2-1)))).
derive(arctanh(X),X,1/(1-X^2)).
derive(arcoth(X),X,1/(1-X^2)).
derive(arcsec(X),X,1/(X*sqrt(X^2-1))).
derive(arccsc(X),X,(-1/(X*sqrt(X^2-1)))).

% exponential and logarithnic functions
derive(e^X,X,e^X).
derive(A^X,X,A^X*ln(A)) :- atomic(A).
derive(ln(X),X,1/X).
derive(log(X),X,1/X).

% other functions
derive(abs(X),X,X/abs(X)).
derive(sqrt(X),X,1/(2*sqrt(X))).

% rules for different operators
derive(F+G,X,DF+DG):- derive(F,X,DF), derive(G,X,DG).
derive(F*G,X, F*DG+DF*G):- derive(F,X,DF), derive(G,X,DG).
derive(F-G,X,DF-DG):- derive(F,X,DF), derive(G,X,DG).
derive(F/G,X,(G*DF-F*DG)/(G^2)):- derive(F,X,DF), derive(G,X,DG).

% derive of a compound function
derive(F_G_X,X,DF*DG):- F_G_X =.. [_,G_X], G_X\=X,
                     derive(F_G_X,G_X,DF), 
                     derive(G_X,X,DG).


% ==========================================================
% derivative(+Function,+X,-Result):- Result is a
%                       derive of Function   
%                       with respect to X. 
%                       Function and X are base terms.
% ==========================================================
derivative(F, X, Result) :- derive(F, X, Result1), simp(Result1, Result), !.

% ==========================================================
% integralFunction(+Function, +X, +Depth, -Result):- Result is the prime 
%                      function of the function Function 
%                      with respect to X. 
%                      Function and X are base terms.
%                      integralFunction is a helper function for integral and primitiveFunction
% ==========================================================
integralFunction(_, _, Depth, _) :- Depth > 6, throw(depth_limit_exceeded).  % Set depth limit to 10
integralFunction(0, _, _, 0).
integralFunction(Y, X, _, Y*X) :- atomic(Y), Y\=X.
integralFunction(X, X, _, (X^2)/2).
integralFunction(X^N, X, _, (X^N1)/N1) :- number(N), N1 is N+1.
integralFunction(X^A, X, _, (X^(A+1))/(A+1)) :- atom(A).

% elementary functions
integralFunction(1/X, X, _, ln(abs(X))).
integralFunction(X^A, X, _, (X^(A+1))/(A+1)).
integralFunction(ln(X), X, _, X*ln(X)-X).
integralFunction(log(X), X, _, X * log(X) - X).
integralFunction(e^X, X, _, e^X).
integralFunction(A^X, X, _, (A^X)/ln(A)) :- atomic(A).
integralFunction(-Y, X, Depth, -Z) :- integralFunction(Y, X, Depth, Z).
integralFunction(F/G, X, Depth, F1/G) :- \+ contains(X, G), integralFunction(F, X, Depth, F1).

% goniometrical functions
integralFunction(sin(X), X, _, (-cos(X))).
integralFunction(cos(X), X, _, sin(X)).
integralFunction(tan(X), X, _, (-ln(abs(cos(X))))).
integralFunction(cot(X), X, _, ln(abs(sin(X)))).
integralFunction(sec(X), X, _, ln(abs(sec(X)+tan(X)))).
integralFunction(csc(X), X, _, (-ln(abs(csc(X)+cot(X))))).

integralFunction(sin(2*X), X, Depth, 2*R) :- integralFunction(sin(X)*cos(X), X, Depth, R).
integralFunction(cos(2*X), X, Depth, 2*R) :- integralFunction(cos(X)^2-sin(X)^2, X, Depth, R).

% inverse goniometrical functions
integralFunction(arcsin(X), X, _, X*arcsin(X)+sqrt(1-X^2)).
integralFunction(arccos(X), X, _, X*arccos(X)-sqrt(1-X^2)).
integralFunction(arctan(X), X, _, X*arctan(X)-ln(abs(1+X^2))).
integralFunction(arccot(X), X, _, X*arccot(X)+ln(abs(1+X^2))).
integralFunction(arcsec(X), X, _, X*arcsec(X)+ln(abs(X*sqrt(X^2-1)+1))).
integralFunction(arccsc(X), X, _, X*arccsc(X)-ln(abs(X*sqrt(X^2-1)+1))).

integralFunction(1/sqrt(1-X^2), X, _, arcsin(X)).
integralFunction(-1/sqrt(1-X^2), X, _, arccos(X)).
integralFunction(1/(1+X^2), X, _, arctan(X)).
integralFunction(-1/(1+X^2), X, _, arccot(X)).
integralFunction(1/(1-X^2), X, _, arctanh(X)).
integralFunction(1/(X^2-1), X, _, arcoth(X)).

integralFunction(1/(X*sqrt(X^2-1)), X, _, arcsec(X)).
integralFunction(1/(X*sqrt(X^2+1)), X, _, arccsc(X)).

integralFunction(sin(X)*cos(X), X, _, sin(X)^2/2).
integralFunction(cos(X)*sin(X), X, _, sin(X)^2/2).


% rules for different operators
% per parts
integralFunction(F*G, X, Depth, Result) :- contains(X, F), contains(X, G), 
        NewDepth is Depth + 1,
        integralFunction(G, X, NewDepth, IG), 
        (
            (\+ contains(G, IG) ; contains(e^X, IG)) 
            -> derivative(F, X, DF), simple(DF*IG, DF_IG), integralFunction(DF_IG, X, NewDepth, G2), Result = F*IG-G2 ;
            derivative(G, X, DG), integralFunction(F, X, NewDepth, IF), simple(DG*IF, DG_IF), integralFunction(DG_IF, X, NewDepth, F2), Result = G*IF-F2
        ).
% division
integralFunction(F/G, X, Depth, Result) :- integralFunction(F*(1/G), X, Depth, Result).
% simple operations
integralFunction(F*C, X, Depth, C*F1) :- contains(X, F), integralFunction(F, X, Depth, F1).
integralFunction(C*F, X, Depth, C*F1) :- contains(X, F), integralFunction(F, X, Depth, F1).
integralFunction(F+G, X, Depth, F1+G1) :- integralFunction(F, X, Depth, F1), integralFunction(G, X, Depth, G1).
integralFunction(F-G, X, Depth, F1-G1) :- integralFunction(F, X, Depth, F1), integralFunction(G, X, Depth, G1).
% power to multiplication
integralFunction(F^2, X, Depth, Result) :- integralFunction(F*F, X, Depth, Result), !.
integralFunction(F^N, X, Depth, Result) :- number(N), N1 is N - 1, integralFunction(F^N1*F, X, Depth, Result).

% integralFunction of a compound function is no supported as there are functions that don't have a primitive function
% Integrate by substitution is not supported as it's extremely difficult to choose what to substitute for

% ==========================================================
% integral(+F, +X, +Start, +End, +Depth, -Result) :- Result is the
%                      integral of the function F with respect
%                      to X in the interval [Start, End]
% ==========================================================

% integral2 is a helper predicate for integrals
integral(F, X, Start, End, Result) :- catch(integral2(F, X, Start, End, Result), depth_limit_exceeded, fail).
integral2(F, X, Start, End, Result) :- integralFunction(F, X, 0, F1), 
    substitute(F1, X, End, F_End), 
    substitute(F1, X, Start, F_Start), 
    simp(F_End-F_Start, Result), 
    !.

% ==========================================================
% primitiveFunction(+F, +X, -Result) :- Result is the prime
%                      function of the function F with respect
%                      to X
% ==========================================================
primitiveFunction(F, X, Result) :- catch(primitiveFunction2(F, X, Result), depth_limit_exceeded, fail).
% primitiveFunction2 is a helper predicate for primitiveFunction
primitiveFunction2(F, X, Result + c) :- integralFunction(F, X, 0, F1), simp(F1, Result), !.
