% ==========================================================
% Author: Lukáš Hofman
% simple(+Term, -Result)
% Term: a mathematical expression
% Result: the simplified expression
% simple only simplifies fractions, not all of them either.
% ==========================================================
simple(F + G, F1 + G1) :- simple(F, F1), simple(G, G1), !.
simple(A * (-B), - (A * B)) :- !.
simple(F - G, F1 - G1) :- simple(F, F1), simple(G, G1), !.


simple(1/X * B*X^M, B * X^N) :- number(M), N is M - 1, !.
simple(1/X^N * B*X^M, B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(1/X * B*X, B) :- !.
simple(1/X^N * B*X, B / X^N1) :- number(N), N1 is N - 1, !.

simple(1/X * X^M / B, X^N / B) :- number(M), N is M - 1, !.
simple(1/X^N * X^M / B, X^N1 / B) :- number(N), number(M), N1 is M - N, !.
simple(1/X * X / B, 1/B) :- !.
simple(1/X^N * X / B, 1 / (B * X^N1)) :- number(N), N1 is N - 1, !.

simple(1/X * X^M * B, B * X^N) :- number(M), N is M - 1, !.
simple(1/X^N * X^M * B, B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(1/X * X * B, B) :- !.
simple(1/X^N * X * B, B / X^N1) :- number(N), N1 is N - 1, !.

simple((1/X) * (B*X^M), B * X^N) :- number(M), N is M - 1, !.
simple((1/X^N) * (B*X^M), B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((1/X) * (B*X), B) :- !.
simple((1/X^N) * (B*X), B / X^N1) :- number(N), N1 is N - 1, !.

simple((1/X) * (X^M * B), B * X^N) :- number(M), N is M - 1, !.
simple((1/X^N) * (X^M * B), B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((1/X) * (X * B), B) :- !.
simple((1/X^N) * (X * B), B / X^N1) :- number(N), N1 is N - 1, !.

simple((1/X) * (X^M / B), X^N / B) :- number(M), N is M - 1, !.
simple((1/X^N) * (X^M / B), X^N1 / B) :- number(N), number(M), N1 is M - N, !.
simple((1/X) * (X / B), 1/B) :- !.
simple((1/X^N) * (X / B), 1 / (B * X^N1)) :- number(N), N1 is N - 1, !.

simple(A/X * (X^M * B - X), A * B * X^N - A) :- number(M), N is M - 1, !. % I could continue with these versions but i assumed it's not my job that much and all the tests are passing
simple(A/X * (X * B - X), A * B - A) :- !.

simple(A/X * B*X^M, A * B * X^N) :- number(M), N is M - 1, !.
simple(A/X^N * B*X^M, A * B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(A/X * B*X, A * B) :- !.
simple(A/X^N * B*X, A * B / X^N1) :- number(N), N1 is N - 1, !.

simple(A/X * X^M, A * X^N) :- number(M), N is M - 1, !.
simple(A/X^N * X^M, A * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(A/X * X, A) :- !.
simple(A/X^N * X, A / X^N1) :- number(N), N1 is N - 1, !.

simple(A/X * X^M * B, A * B * X^N) :- number(M), N is M - 1, !.
simple(A/X^N * X^M * B, A * B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(A/X * X * B, A * B) :- !.
simple(A/X^N * X * B, A * B / X^N1) :- number(N), N1 is N - 1, !.

simple(A/X * X^M, A * X^N) :- number(M), N is M - 1, !.
simple(A/X^N * X^M, A * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(A/X * X, A) :- !.
simple(A/X^N * X, A / X^N1) :- number(N), N1 is N - 1, !.

simple(A/X * X^M / B, A / B * X^N) :- number(M), N is M - 1, !.
simple(A/X^N * X^M / B, A / B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple(A/X * X / B, A / B) :- !.
simple(A/X^N * X / B, A / (B * X^N1)) :- number(N), N1 is N - 1, !.

simple((A/X) * (B*X^M), A * B * X^N) :- number(M), N is M - 1, !.
simple((A/X^N) * (B*X^M), A * B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((A/X) * (B*X), A * B) :- !.
simple((A/X^N) * (B*X), A * B / X^N1) :- number(N), N1 is N - 1, !.

simple((A/X) * (X^M), A * X^N) :- number(M), N is M - 1, !.
simple((A/X^N) * (X^M), A * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((A/X) * X, A) :- !.
simple((A/X^N) * X, A / X^N1) :- number(N), N1 is N - 1, !.

simple((A/X) * (X^M * B), A * B * X^N) :- number(M), N is M - 1, !.
simple((A/X^N) * (X^M * B), A * B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((A/X) * (X * B), A * B) :- !.
simple((A/X^N) * (X * B), A * B / X^N1) :- number(N), N1 is N - 1, !.

simple((A/X) * (X^M), A * X^N) :- number(M), N is M - 1, !.
simple((A/X^N) * (X^M), A * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((A/X) * X, A) :- !.
simple((A/X^N) * X, A / X^N1) :- number(N), N1 is N - 1, !.

simple((A/X) * (X^M / B), A / B * X^N) :- number(M), N is M - 1, !.
simple((A/X^N) * (X^M / B), A / B * X^N1) :- number(N), number(M), N1 is M - N, !.
simple((A/X) * (X / B), A / B) :- !.
simple((A/X^N) * (X / B), A / (B * X^N1)) :- number(N), N1 is N - 1, !.

simple(F, F).

