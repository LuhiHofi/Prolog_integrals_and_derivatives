% Author: Lukáš Hofman
% Description: A program that calculates integrals and derivatives of functions.
%   I am using library created by Jakub Smolík that simplifies mathematical expressions.
%   Integration by substitution is supported only in the "u-substitution" form
%   ∫ f(g(x))·g'(x) dx = F(g(x)) (this also covers linear inner functions like sin(2*x+1)),
%   because in general it's challenging to choose what to substitute for.

:- ensure_loaded('Expression-simplification/simplify.pl').
% Load simple.pl because simplify doesn't take care of fractions and that way i get plenty of recursion errors.
:- ensure_loaded('simple.pl').

% ==========================================================
% simplify(+Expr, -Result) :- wrapper around simp/2 from the
%                      Expression-simplification library.
%   * The library only knows `log` (natural logarithm) while this program
%     uses `ln`, so `ln` is renamed to `log` before simplification and
%     back afterwards (`log` is treated as the natural logarithm everywhere
%     in this program, so this is a lossless rename).
%   * The library simplifies `exp(E)` much better than `e^E`, so e^E is
%     converted to exp(E) before and back to e^E after the simplification
%     (therefore exp(x) and e^x are the same thing for this program and
%     results are always printed as e^E).
%   * `abs(K)` of a numeric constant K (numbers, e, pi and arithmetic on them)
%     is evaluated, so that e.g. ln(abs(1)) can be simplified to 0.
%   * well known values like arctan(1) = pi/4 or sinh(0) = 0 (see known_value/2)
%     are substituted after the simplification and the result is simplified again.
% ==========================================================
simplify(Expr, Result) :-
    simplify_once(Expr, Result1),
    known_values(Result1, Result2),
    ( Result2 == Result1 -> Result = Result1 ; simplify_once(Result2, Result) ).

simplify_once(Expr, Result) :-
    prepare(Expr, Expr1),
    simp(Expr1, Result1),
    postprocess(Result1, Result).

known_values(T, R) :- known_value(T, R), !.
known_values(T, T) :- atomic(T), !.
known_values(T, R) :-
    T =.. [F|Args],
    maplist(known_values, Args, Args1),
    T1 =.. [F|Args1],
    ( known_value(T1, R) -> true ; R = T1 ).

known_value(sin(0), 0).        known_value(cos(0), 1).        known_value(tan(0), 0).
known_value(arcsin(0), 0).     known_value(arccos(1), 0).     known_value(arctan(0), 0).
known_value(arcsin(1), pi/2).  known_value(arccos(0), pi/2).  known_value(arctan(1), pi/4).
known_value(arccot(0), pi/2).  known_value(arccot(1), pi/4).  known_value(arcsec(1), 0).
known_value(sinh(0), 0).       known_value(cosh(0), 1).       known_value(tanh(0), 0).
known_value(arcsinh(0), 0).    known_value(arccosh(1), 0).    known_value(arctanh(0), 0).
known_value(ln(1), 0).         known_value(ln(e), 1).         known_value(log(1), 0).
known_value(log(e), 1).        known_value(exp(0), 1).        known_value(exp(1), e).

% prepare(+T, -R) :- ln -> log, e^E -> exp(E), abs(constant) -> constant
prepare(T, T) :- atomic(T), !.
prepare(abs(T), R) :-
    constant_value(T, V), !,
    ( V >= 0 -> prepare(T, R) ; prepare(-T, R) ).
prepare(e^E, exp(E1)) :- !, prepare(E, E1).
prepare(T, R) :-
    T =.. [F|Args],
    maplist(prepare, Args, Args1),
    ( F == ln -> F1 = log ; F1 = F ),
    R =.. [F1|Args1].

% postprocess(+T, -R) :- log -> ln, exp(E) -> e^E
postprocess(T, T) :- atomic(T), !.
postprocess(exp(E), e^E1) :- !, postprocess(E, E1).
postprocess(T, R) :-
    T =.. [F|Args],
    maplist(postprocess, Args, Args1),
    ( F == log -> F1 = ln ; F1 = F ),
    R =.. [F1|Args1].

% constant_value(+T, -V) :- T is an arithmetic expression built only from
%                      numbers, e, pi and + - * / ^ sqrt abs; V is its value
constant_value(T, V) :- numeric_constant(T), catch(V is T, _, fail).

numeric_constant(T) :- number(T), !.
numeric_constant(e) :- !.
numeric_constant(pi) :- !.
numeric_constant(T) :-
    compound(T),
    T =.. [Op|Args],
    memberchk(Op, [+, -, *, /, ^, sqrt, abs]),
    maplist(numeric_constant, Args).

% ==========================================================
% substitute(+F, +X, +V, -F1) :- F1 is the result of
%                      replacing X with V in F
%   X may be any (sub)term, not only a variable, e.g.
%   substitute(sin(2*x), 2*x, u, sin(u)).
% ==========================================================
substitute(Term, X, V, V) :- Term == X, !.
substitute(Term, _, _, Term) :- atomic(Term), !.
substitute(Term, X, V, Result) :-
    Term =.. [F|Args],
    maplist(substitute_(X, V), Args, Args1),
    Result =.. [F|Args1].
substitute_(X, V, Term, Result) :- substitute(Term, X, V, Result).

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

% powers with a constant exponent (chain rule included, so sin(x)^3 works too)
derive(F^N,X,N*F^N1*DF) :- number(N), N1 is N-1, derive(F,X,DF).
derive(F^A,X,A*F^(A-1)*DF) :- \+ number(A), \+ contains(X, A), derive(F,X,DF).
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
% hyperbolic functions
derive(sinh(X),X,cosh(X)).
derive(cosh(X),X,sinh(X)).
derive(tanh(X),X,1/cosh(X)^2).
derive(coth(X),X,(-1/sinh(X)^2)).
% inverse hyperbolic functions
derive(arcsinh(X),X,1/sqrt(X^2+1)).
derive(arccosh(X),X,1/sqrt(X^2-1)).
derive(arctanh(X),X,1/(1-X^2)).
derive(arcoth(X),X,1/(1-X^2)).

% exponential and logarithnic functions
derive(e^F,X,e^F*DF) :- derive(F,X,DF).
derive(exp(F),X,exp(F)*DF) :- derive(F,X,DF).
derive(A^F,X,A^F*ln(A)*DF) :- A \== e, \+ contains(X, A), derive(F,X,DF).
% general power F^G where both the base and the exponent depend on X
derive(F^G,X,F^G*(DG*ln(F)+G*DF/F)) :-
    contains(X, F), contains(X, G),
    derive(F,X,DF), derive(G,X,DG).
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
derivative(F, X, Result) :- derive(F, X, Result1), simplify(Result1, Result), !.

% ==========================================================
% integralFunction(+Function, +X, +Depth, -Result):- Result is the prime 
%                      function of the function Function 
%                      with respect to X. 
%                      Function and X are base terms.
%                      integralFunction is a helper function for integral and primitiveFunction
%                      Depth counts nested integrations by parts; when it exceeds
%                      max_depth/1 the exception depth_limit_exceeded is thrown.
% ==========================================================
max_depth(6).
integralFunction(_, _, Depth, _) :- max_depth(Max), Depth > Max, throw(depth_limit_exceeded).

% constants and the variable itself
integralFunction(0, _, _, 0).
integralFunction(Y, X, _, Y*X) :- \+ contains(X, Y).
integralFunction(X, X, _, (X^2)/2).

% powers of X
integralFunction(X^N, X, _, ln(abs(X))) :- number(N), N =:= -1.
integralFunction(X^N, X, _, (X^N1)/N1) :- number(N), N =\= -1, N1 is N+1.
integralFunction(X^A, X, _, (X^(A+1))/(A+1)) :- \+ number(A), \+ contains(X, A).
integralFunction(1/X, X, _, ln(abs(X))).
integralFunction(1/X^N, X, _, (X^N1)/N1) :- number(N), N =\= 1, N1 is 1-N.
integralFunction(sqrt(X), X, _, (2*X*sqrt(X))/3).
integralFunction(1/sqrt(X), X, _, 2*sqrt(X)).

% elementary functions
integralFunction(ln(X), X, _, X*ln(X)-X).
integralFunction(log(X), X, _, X * log(X) - X).
integralFunction(e^X, X, _, e^X).
integralFunction(exp(X), X, _, exp(X)).
integralFunction(A^X, X, _, (A^X)/ln(A)) :- A \== e, \+ contains(X, A).

% goniometrical functions
integralFunction(sin(X), X, _, (-cos(X))).
integralFunction(cos(X), X, _, sin(X)).
integralFunction(tan(X), X, _, (-ln(abs(cos(X))))).
integralFunction(cot(X), X, _, ln(abs(sin(X)))).
integralFunction(sec(X), X, _, ln(abs(sec(X)+tan(X)))).
integralFunction(csc(X), X, _, (-ln(abs(csc(X)+cot(X))))).

integralFunction(sin(X)*cos(X), X, _, sin(X)^2/2).
integralFunction(cos(X)*sin(X), X, _, sin(X)^2/2).

% powers of goniometrical functions
integralFunction(sin(X)^2, X, _, X/2 - sin(X)*cos(X)/2).
integralFunction(cos(X)^2, X, _, X/2 + sin(X)*cos(X)/2).
integralFunction(tan(X)^2, X, _, tan(X) - X).
integralFunction(cot(X)^2, X, _, -cot(X) - X).
integralFunction(sec(X)^2, X, _, tan(X)).
integralFunction(csc(X)^2, X, _, -cot(X)).
integralFunction(1/cos(X)^2, X, _, tan(X)).
integralFunction(1/sin(X)^2, X, _, -cot(X)).
% reduction formulas
integralFunction(sin(X)^N, X, Depth, -sin(X)^N1*cos(X)/N + N1/N*R) :-
    integer(N), N > 2, N1 is N-1, N2 is N-2,
    integralFunction(sin(X)^N2, X, Depth, R).
integralFunction(cos(X)^N, X, Depth, cos(X)^N1*sin(X)/N + N1/N*R) :-
    integer(N), N > 2, N1 is N-1, N2 is N-2,
    integralFunction(cos(X)^N2, X, Depth, R).
integralFunction(tan(X)^N, X, Depth, tan(X)^N1/N1 - R) :-
    integer(N), N > 2, N1 is N-1, N2 is N-2,
    integralFunction(tan(X)^N2, X, Depth, R).

% inverse goniometrical functions
integralFunction(arcsin(X), X, _, X*arcsin(X)+sqrt(1-X^2)).
integralFunction(arccos(X), X, _, X*arccos(X)-sqrt(1-X^2)).
integralFunction(arctan(X), X, _, X*arctan(X)-ln(1+X^2)/2).
integralFunction(arccot(X), X, _, X*arccot(X)+ln(1+X^2)/2).
integralFunction(arcsec(X), X, _, X*arcsec(X)-ln(abs(X+sqrt(X^2-1)))).
integralFunction(arccsc(X), X, _, X*arccsc(X)+ln(abs(X+sqrt(X^2-1)))).

integralFunction(1/sqrt(1-X^2), X, _, arcsin(X)).
integralFunction(-1/sqrt(1-X^2), X, _, arccos(X)).
integralFunction(1/(1+X^2), X, _, arctan(X)).
integralFunction(1/(X^2+1), X, _, arctan(X)).
integralFunction(-1/(1+X^2), X, _, arccot(X)).
integralFunction(-1/(X^2+1), X, _, arccot(X)).
integralFunction(1/(1-X^2), X, _, arctanh(X)).
integralFunction(1/(X^2-1), X, _, -arcoth(X)).

integralFunction(1/(X*sqrt(X^2-1)), X, _, arcsec(X)).
integralFunction(-1/(X*sqrt(X^2-1)), X, _, arccsc(X)).
integralFunction(1/sqrt(X^2+1), X, _, arcsinh(X)).
integralFunction(1/sqrt(X^2-1), X, _, arccosh(X)).

% the same standard forms with a positive constant A instead of 1
integralFunction(1/(X^2+A), X, _, arctan(X/sqrt(A))/sqrt(A)) :- positive_constant(X, A).
integralFunction(1/(A+X^2), X, _, arctan(X/sqrt(A))/sqrt(A)) :- positive_constant(X, A).
integralFunction(1/(A-X^2), X, _, arctanh(X/sqrt(A))/sqrt(A)) :- positive_constant(X, A).
integralFunction(1/(X^2-A), X, _, -arcoth(X/sqrt(A))/sqrt(A)) :- positive_constant(X, A).
integralFunction(1/sqrt(A-X^2), X, _, arcsin(X/sqrt(A))) :- positive_constant(X, A).
integralFunction(1/sqrt(X^2+A), X, _, arcsinh(X/sqrt(A))) :- positive_constant(X, A).
integralFunction(1/sqrt(A+X^2), X, _, arcsinh(X/sqrt(A))) :- positive_constant(X, A).
integralFunction(1/sqrt(X^2-A), X, _, arccosh(X/sqrt(A))) :- positive_constant(X, A).
% X^2/(X^2+A) = 1 - A/(X^2+A)   (appears when integrating X*arctan(X) per parts)
integralFunction(F, X, _, X - sqrt(A)*arctan(X/sqrt(A))) :- x2_over_x2_plus(F, X, A), positive_constant(X, A).

% hyperbolic functions
integralFunction(sinh(X), X, _, cosh(X)).
integralFunction(cosh(X), X, _, sinh(X)).
integralFunction(tanh(X), X, _, ln(cosh(X))).
integralFunction(coth(X), X, _, ln(abs(sinh(X)))).
integralFunction(arcsinh(X), X, _, X*arcsinh(X)-sqrt(X^2+1)).
integralFunction(arccosh(X), X, _, X*arccosh(X)-sqrt(X^2-1)).
integralFunction(arctanh(X), X, _, X*arctanh(X)+ln(1-X^2)/2).
integralFunction(arcoth(X), X, _, X*arcoth(X)+ln(abs(X^2-1))/2).

% rules for different operators
% simple operations
integralFunction(-Y, X, Depth, -Z) :- integralFunction(Y, X, Depth, Z).
integralFunction(F/G, X, Depth, F1/G) :- \+ contains(X, G), integralFunction(F, X, Depth, F1).
integralFunction(C/G, X, Depth, C*R) :- C \== 1, \+ contains(X, C), integralFunction(1/G, X, Depth, R).
integralFunction(F*C, X, Depth, C*F1) :- contains(X, F), \+ contains(X, C), integralFunction(F, X, Depth, F1).
integralFunction(C*F, X, Depth, C*F1) :- \+ contains(X, C), contains(X, F), integralFunction(F, X, Depth, F1).
integralFunction(F+G, X, Depth, F1+G1) :- integralFunction(F, X, Depth, F1), integralFunction(G, X, Depth, G1).
integralFunction(F-G, X, Depth, F1-G1) :- integralFunction(F, X, Depth, F1), integralFunction(G, X, Depth, G1).
integralFunction(_^0, X, _, X).
integralFunction(F^1, X, Depth, R) :- integralFunction(F, X, Depth, R).
integralFunction(F^N, X, Depth, R) :- number(N), N < 0, N1 is -N, integralFunction(1/F^N1, X, Depth, R).

% products of exponentials and goniometrical functions (cyclic integration by parts)
integralFunction(e^X*sin(X), X, _, e^X*(sin(X)-cos(X))/2).
integralFunction(sin(X)*e^X, X, _, e^X*(sin(X)-cos(X))/2).
integralFunction(e^X*cos(X), X, _, e^X*(sin(X)+cos(X))/2).
integralFunction(cos(X)*e^X, X, _, e^X*(sin(X)+cos(X))/2).

% substitution: ∫ Coef * f(G) dx = C * F(G)  where Coef = C * G' for a constant C
%               and F is the primitive function of f.
% This covers e.g. sin(2*x+1), e^(3*x), 1/(2*x+1), (x+1)^5, x*sin(x^2),
% x/(x^2+1), e^x/(1+e^x), sin(x)^2*cos(x), ln(x)/x, ...
integralFunction(F, X, Depth, C*R) :-
    split_product(F, X, Coef, Body),
    inner_term(Body, X, G),
    fresh_atom(F, X, U),
    substitute(Body, G, U, BodyU),
    \+ contains(X, BodyU),
    derivative(G, X, DG),
    DG \== 0,
    constant_ratio(Coef, DG, X, C),
    integralFunction(BodyU, U, Depth, RU),
    substitute(RU, U, G, R).

% quotients: split sums over a common denominator and cancel powers of X
integralFunction((F+G)/H, X, Depth, R) :- contains(X, H), integralFunction(F/H + G/H, X, Depth, R).
integralFunction((F-G)/H, X, Depth, R) :- contains(X, H), integralFunction(F/H - G/H, X, Depth, R).
integralFunction(F/G, X, Depth, R) :-
    power_of(F, X, N), power_of(G, X, M), N1 is N - M,
    integralFunction(X^N1, X, Depth, R).
integralFunction(C*F/G, X, Depth, C*R) :-
    \+ contains(X, C), power_of(F, X, N), power_of(G, X, M), N1 is N - M,
    integralFunction(X^N1, X, Depth, R).

% per parts
%   The part G is integrated and F is derived if integrating G doesn't make it
%   more complex (IG doesn't contain G) or G is an exponential (a^X, e^X, exp(X)).
%   Otherwise the roles are swapped.
integralFunction(F*G, X, Depth, Result) :- contains(X, F), contains(X, G), 
        NewDepth is Depth + 1,
        integralFunction(G, X, NewDepth, IG), 
        (
            (\+ contains(G, IG) ; contains_exponential(X, IG)) 
            -> derivative(F, X, DF), simplify_product(DF*IG, DF_IG), integralFunction(DF_IG, X, NewDepth, G2), Result = F*IG-G2 ;
            derivative(G, X, DG), integralFunction(F, X, NewDepth, IF), simplify_product(DG*IF, DG_IF), integralFunction(DG_IF, X, NewDepth, F2), Result = G*IF-F2
        ).
% division
integralFunction(F/G, X, Depth, Result) :- contains(X, F), contains(X, G), integralFunction(F*(1/G), X, Depth, Result).
% power to multiplication
integralFunction(F^2, X, Depth, Result) :- integralFunction(F*F, X, Depth, Result).
integralFunction(F^N, X, Depth, Result) :- integer(N), N > 2, N1 is N - 1, integralFunction(F^N1*F, X, Depth, Result).

% positive_constant(+X, +A) :- A does not depend on X and, if its value
%                      can be computed, it is positive (symbolic constants
%                      like a or a^2+1 are assumed to be positive)
positive_constant(X, A) :-
    \+ contains(X, A),
    ( constant_value(A, V) -> V > 0 ; true ).

% x2_over_x2_plus(+F, +X, -A) :- F is X^2/(X^2+A) written in one of several ways
x2_over_x2_plus(X^2/(X^2+A), X, A).
x2_over_x2_plus(X^2/(A+X^2), X, A).
x2_over_x2_plus(1/(X^2+A)*X^2, X, A).
x2_over_x2_plus(1/(A+X^2)*X^2, X, A).
x2_over_x2_plus(X^2*(1/(X^2+A)), X, A).
x2_over_x2_plus(X^2*(1/(A+X^2)), X, A).

% power_of(+F, +X, -N) :- F is X^N (X itself is X^1)
power_of(X, X, 1).
power_of(X^N, X, N) :- number(N).

% contains_exponential(+X, +T) :- T contains an exponential function of X,
%                      i.e. a^E or exp(E) where a is constant and E contains X
contains_exponential(X, T) :-
    sub_term(S, T),
    ( S = A^E, \+ contains(X, A), contains(X, E) ; S = exp(E), contains(X, E) ), !.

% simplify_product(+T, -R) :- simplifies the product that appears in
%                      integration by parts. simple/2 is tried first; if it
%                      can't do anything and the product contains a fraction,
%                      the simplification library is used with divisions
%                      rewritten as negative powers (it can cancel e.g.
%                      1/x*(2*ln(x))*(x^2/2) to x*ln(x) that way).
simplify_product(T, R) :-
    simple(T, R1),
    (   R1 == T, sub_term(_/_, T)
    ->  to_powers(T, P), simplify(P, R)
    ;   R = R1
    ).

% ---- helpers for the substitution rule ----

% split_product(+F, +X, -Coef, -Body) :- F = Coef * Body, Body contains X
split_product(A*B, X, A, B) :- contains(X, B).
split_product(A*B, X, B, A) :- contains(X, A).
split_product(A/B, X, A, 1/B) :- contains(X, B).
split_product(A/B, X, 1/B, A) :- contains(X, A).
split_product(A/(B*D), X, A/B, 1/D) :- contains(X, D).
split_product(A/(B*D), X, A/D, 1/B) :- contains(X, B).
split_product(F, X, 1, F) :- F \= _*_, F \= _/_, contains(X, F).

% inner_term(+Body, +X, -G) :- G is a compound subterm of Body (Body itself,
%                      an argument, an argument of an argument, ...) that
%                      contains X; candidates are generated from the outside in
inner_term(Body, X, G) :-
    compound(Body), Body \== X, contains(X, Body),
    ( G = Body
    ; Body =.. [_|Args], member(Arg, Args), inner_term(Arg, X, G)
    ).

% constant_ratio(+A, +B, +X, -C) :- C is the simplified A/B and does not
%                      contain X. The simplification library sometimes
%                      can't cancel fractions of fractions like (1/x)/(1/x),
%                      so as a fallback A/B is rewritten with negative powers
%                      (A*B^(-1)) and simplified again.
constant_ratio(A, B, X, C) :-
    simplify(A/B, C0),
    (   \+ contains(X, C0)
    ->  C = C0
    ;   to_powers(A/B, T),
        simplify(T, C),
        \+ contains(X, C)
    ).

% to_powers(+T, -R) :- every division P/Q (Q not a number) in T is rewritten as P*Q^(-1)
to_powers(T, T) :- atomic(T), !.
to_powers(P/Q, R) :-
    \+ number(Q), !,
    to_powers(P, P1), to_powers(Q, Q1),
    ( P1 == 1 -> R = Q1^(-1) ; R = P1*Q1^(-1) ).
to_powers(T, R) :-
    T =.. [F|Args],
    maplist(to_powers, Args, Args1),
    R =.. [F|Args1].

% fresh_atom(+F, +X, -U) :- U is an atom that does not occur in F
fresh_atom(F, X, U) :-
    member(U, [u, v, w, t, s, u1, u2, u3]),
    U \== X, U \== c,
    \+ contains(U, F), !.

% ==========================================================
% integral(+F, +X, +Start, +End, -Result) :- Result is the
%                      integral of the function F with respect
%                      to X in the interval [Start, End]
% ==========================================================

% integral2 is a helper predicate for integrals
integral(F, X, Start, End, Result) :- catch(integral2(F, X, Start, End, Result), depth_limit_exceeded, fail).
integral2(F, X, Start, End, Result) :- integralFunction(F, X, 0, F1), 
    substitute(F1, X, End, F_End), 
    substitute(F1, X, Start, F_Start), 
    simplify(F_End-F_Start, Result), 
    !.

% ==========================================================
% primitiveFunction(+F, +X, -Result) :- Result is the prime
%                      function of the function F with respect
%                      to X
% ==========================================================
primitiveFunction(F, X, Result) :- catch(primitiveFunction2(F, X, Result), depth_limit_exceeded, fail).
% primitiveFunction2 is a helper predicate for primitiveFunction
primitiveFunction2(F, X, Result + c) :- integralFunction(F, X, 0, F1), simplify(F1, Result), !.
