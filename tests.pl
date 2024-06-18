% Author: Lukáš Hofman
% ==========================================================
% Description: This file contains tests for the functions defined in the integrals.pls file.
% ==========================================================

:- use_module(library(plunit)).
:- ensure_loaded('Expression-simplification/simplify.pl').
:- ensure_loaded('simple.pl').

% Test cases for derivative/3

:- begin_tests(derivatives).

test(constant) :-
    derivative(5, x, R),
    R == 0.

test(variable) :-
    derivative(x, x, R),
    R == 1.

test(polynomial_linear) :-
    derivative(2*x, x, R),
    R == 2.

test(polynomial_quadratic) :-
    derivative(3*x^2, x, R),
    R == 6*x.

test(polynomial_cubic) :-
    derivative(3*x^3, x, R),
    R == 9*x^2.

test(sum_of_constants) :-
    derivative(3 + 5, x, R),
    R == 0.

test(sum_of_functions) :-
    derivative(x^2/2 + x, x, R),
    R == x + 1.

test(difference_of_functions) :-
    derivative(x^2 - x/3, x, R),
    R == 2*x - 1/3.

test(product_of_functions) :-
    derivative(x * sin(x), x, R),
    R == x * cos(x) + sin(x).

test(quotient_of_functions) :-
    derivative(x / (1 + x^2), x, R),
    R == (-x^2 + 1) / (x^2+1)^2.

test(trigonometric_function_sin) :-
    derivative(sin(x), x, R),
    R == cos(x).

test(trigonometric_function_cos) :-
    derivative(cos(x), x, R),
    R == -sin(x).

test(trigonometric_function_tan) :-
    derivative(tan(x), x, R),
    R == 1 / cos(x)^2.

test(inverse_trigonometric_function_arcsin) :-
    derivative(arcsin(x), x, R),
    R == 1 / sqrt(-x^2 + 1).

test(inverse_trigonometric_function_arccos) :-
    derivative(arccos(x), x, R),
    R == - 1 / sqrt(-x^2 + 1).

test(inverse_trigonometric_function_arctan) :-
    derivative(arctan(x), x, R),
    R == 1 / (x^2 + 1).

test(exponential_function) :-
    derivative(e^x, x, R),
    R == e^x.

test(logarithmic_function) :-
    derivative(ln(x), x, R),
    R == 1/x.

test(complex_function_1) :-
    derivative(x^2 * e^x, x, R),
    R == x^2 * e^x + 2*x * e^x.

test(complex_function_2) :-
    derivative(sin(x) * cos(x), x, R),
    R == -sin(x)^2 + cos(x)^2.

test(complex_function_3) :-
    derivative((x^3 + x) / (x^2 + 1), x, R),
    R == (- (2*x^4)+3*((x^2+1)*(x^2+1/3))-2*x^2)/(x^2+1)^2.

:- end_tests(derivatives).

% Test cases for integralFunction/3

:- begin_tests(integral_functions).

test(polynomial) :-
    once(integralFunction(x^2, x, 0, R)),
    R == x^3 / 3.

test(sum_of_functions) :-
    once(integralFunction(x^2 + x, x, 0, R)),
    R == (x^3) / 3 + (x^2) / 2.

test(trigonometric_function) :-
    once(integralFunction(sin(x), x, 0, R)),
    R == -cos(x).

test(exponential_function) :-
    once(integralFunction(e^x, x, 0, R)),
    R == e^x.

test(logarithmic_function) :-
    once(integralFunction(1/x, x, 0, R)),
    R == ln(abs(x)).

:- end_tests(integral_functions).

% Test cases for integral/5

:- begin_tests(integrals).

test(definite_integral_polynomial) :-
    integral(x^2, x, 0, 1, R),
    R == 1 / 3.

test(definite_integral_trigonometric) :-
    integral(sin(x), x, 0, pi, R),
    R == 2.

test(definite_integral_exponential) :-
    integral(e^x, x, 0, 1, R),
    R == e - 1.


:- end_tests(integrals).

% Test cases for primitiveFunction/3

:- begin_tests(prime_functions).

test(multiplication1) :-
    primitiveFunction(x*ln(x), x, R),
    R == 1/2*(ln(x)*x^2)-1/4*x^2 + c.

test(multiplication2) :-
    primitiveFunction(x*sin(x), x, R),
    R == - (x*cos(x))+sin(x)+c.

test(multiplication3) :-
    primitiveFunction(x^2*e^x, x, R),
    R == x^2*e^x-(2*x*e^x-2*e^x)+c.

test(multiplication4) :-
    primitiveFunction(x^3*sin(x), x, R),
    R == 6*(x*cos(x))-x^3*cos(x)+3*(x^2*sin(x))-6*sin(x) + c.

test(multiplication5) :-
    primitiveFunction(x*x*x, x, R),
    R == 1/4*x^4 + c.

test(logaritmic_exponential) :-
    primitiveFunction(ln(x)^2, x, R),
    R == x*ln(x)^2+2*x-2*(x*ln(x))+c.

% Additional test cases for combined functionality

:- begin_tests(combined).

test(derivative_integral_consistency) :-
    derivative(x^3, x, D),
    primitiveFunction(D, x, I),
    I == x^3 + c.

test(derivative_of_integral) :-
    primitiveFunction(sin(x), x, I),
    derivative(I, x, D),
    D == sin(x).

:- end_tests(combined).

% Run all tests
:- run_tests.
