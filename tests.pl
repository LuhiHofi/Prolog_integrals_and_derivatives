% Author: Lukáš Hofman
% ==========================================================
% Description: This file contains tests for the functions defined in the integrals.pls file.
% ==========================================================

:- use_module(library(plunit)).
:- ensure_loaded('integrals.pl').

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
    R == x^2*e^x+2*(x*e^x).

test(complex_function_2) :-
    derivative(sin(x) * cos(x), x, R),
    R == -sin(x)^2 + cos(x)^2.

test(complex_function_3) :-
    derivative((x^3 + x) / (x^2 + 1), x, R),
    R == (- (2*x^4)+3*((x^2+1)*(x^2+1/3))-2*x^2)/(x^2+1)^2.

% chain rule for powers and exponentials of compound functions
test(power_of_compound_function) :-
    derivative(sin(x)^3, x, R),
    R == 3*(sin(x)^2*cos(x)).

test(exponential_of_compound_function) :-
    derivative(e^(x^2), x, R),
    R == 2*(e^(x^2)*x).

test(general_exponential) :-
    derivative(2^x, x, R),
    R == 2^x*ln(2).

test(exp_function) :-
    % exp(x) and e^x are the same thing, results are printed as e^x
    derivative(exp(x), x, R),
    R == e^x.

test(variable_in_base_and_exponent) :-
    derivative(x^x, x, R),
    R == x^x*(ln(x)+1).

test(symbolic_exponent) :-
    derivative(x^(a+1), x, R),
    R == (a+1)*x^a.

test(hyperbolic_function) :-
    derivative(sinh(x), x, R),
    R == cosh(x).

test(inverse_hyperbolic_function) :-
    derivative(arcsinh(x), x, R),
    R == 1/sqrt(x^2+1).

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

test(definite_integral_logarithm) :-
    integral(1/x, x, 1, e, R),
    R == 1.

test(definite_integral_logarithm_abs) :-
    integral(1/x, x, 1, 2, R),
    R == ln(2).

test(definite_integral_arctan) :-
    integral(1/(x^2+4), x, 0, 2, R),
    R == 1/8*pi.

test(definite_integral_sin_squared) :-
    integral(sin(x)^2, x, 0, pi, R),
    R == 1/2*pi.

test(definite_integral_symbolic_bounds) :-
    integral(x, x, a, b, R),
    R == 1/2*b^2-1/2*a^2.

test(definite_integral_hyperbolic) :-
    integral(1/(1-x^2), x, 0, 1/2, R),
    R == arctanh(1/2).

test(definite_integral_substitution) :-
    integral(x*e^(x^2), x, 0, 1, R),
    R == 1/2*e-1/2.

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
    R == - (2*(x*e^x))+x^2*e^x+2*e^x+c.

test(multiplication4) :-
    primitiveFunction(x^3*sin(x), x, R),
    R == 6*(x*cos(x))-x^3*cos(x)+3*(x^2*sin(x))-6*sin(x) + c.

test(multiplication5) :-
    primitiveFunction(x*x*x, x, R),
    R == 1/4*x^4 + c.

test(logaritmic_exponential) :-
    primitiveFunction(ln(x)^2, x, R),
    R == x*ln(x)^2+2*x-2*(x*ln(x))+c.

% constants and powers of x
test(symbolic_constant) :-
    primitiveFunction(a^2, x, R),
    R == a^2*x+c.

test(negative_power_minus_one) :-
    primitiveFunction(x^(-1), x, R),
    R == ln(abs(x))+c.

test(one_over_x_squared) :-
    primitiveFunction(1/x^2, x, R),
    R == - (1/x)+c.

test(square_root) :-
    primitiveFunction(sqrt(x), x, R),
    R == 2/3*(x*sqrt(x))+c.

test(rational_exponent) :-
    primitiveFunction(x^(3/2), x, R),
    R == 2/5*x^(5/2)+c.

test(sum_over_x) :-
    primitiveFunction((x^2+1)/x, x, R),
    R == 1/2*x^2+ln(abs(x))+c.

% goniometrical functions
test(sin_squared) :-
    primitiveFunction(sin(x)^2, x, R),
    R == - (1/2*(sin(x)*cos(x)))+1/2*x+c.

test(sin_cubed) :-
    primitiveFunction(sin(x)^3, x, R),
    R == - (1/3*(sin(x)^2*cos(x)))-2/3*cos(x)+c.

test(sin_first_power) :-
    primitiveFunction(sin(x)^1, x, R),
    R == -cos(x)+c.

test(sin_negative_power) :-
    primitiveFunction(sin(x)^(-2), x, R),
    R == -cot(x)+c.

test(cos_double_angle) :-
    primitiveFunction(cos(2*x), x, R),
    R == 1/2*sin(2*x)+c.

test(sin_linear_argument) :-
    primitiveFunction(sin(3*x+1), x, R),
    R == - (1/3*cos(3*x+1))+c.

test(exp_times_sin) :-
    primitiveFunction(e^x*sin(x), x, R),
    R == 1/2*(sin(x)*e^x)-1/2*(cos(x)*e^x)+c.

% inverse goniometrical and hyperbolic functions
test(arctan) :-
    primitiveFunction(arctan(x), x, R),
    R == x*arctan(x)-1/2*ln(x^2+1)+c.

test(one_over_x_squared_minus_one) :-
    primitiveFunction(1/(x^2-1), x, R),
    R == -arcoth(x)+c.

test(arctan_with_constant) :-
    primitiveFunction(1/(x^2+4), x, R),
    R == 1/2*arctan(1/2*x)+c.

test(arcsinh) :-
    primitiveFunction(1/sqrt(x^2+1), x, R),
    R == arcsinh(x)+c.

test(sinh) :-
    primitiveFunction(sinh(x), x, R),
    R == cosh(x)+c.

% exponential functions
test(exp_linear_argument) :-
    primitiveFunction(e^(2*x), x, R),
    R == 1/2*e^(2*x)+c.

test(exp_function) :-
    primitiveFunction(exp(x), x, R),
    R == e^x+c.

test(x_times_general_exponential) :-
    primitiveFunction(x*2^x, x, R),
    R == 2^x/ln(2)*x-2^x/ln(2)*(1/ln(2))+c.

test(x_times_exp_linear) :-
    primitiveFunction(x*e^(2*x), x, R),
    R == 1/2*(x*e^(2*x))-1/4*e^(2*x)+c.

test(x_squared_times_exp_negative) :-
    primitiveFunction(x^2*e^(-x), x, R),
    R == - (2*(x*e^(-x)))-x^2*e^(-x)-2*e^(-x)+c.

% substitution
test(substitution_x_sin_x_squared) :-
    primitiveFunction(x*sin(x^2), x, R),
    R == - (1/2*cos(x^2))+c.

test(substitution_x_over_x_squared_plus_one) :-
    primitiveFunction(x/(x^2+1), x, R),
    R == 1/2*ln(abs(x^2+1))+c.

test(substitution_linear_denominator) :-
    primitiveFunction(1/(2*x+1), x, R),
    R == 1/2*ln(abs(2*x+1))+c.

test(substitution_linear_power) :-
    primitiveFunction((x+1)^2, x, R),
    R == 1/3*(x+1)^3+c.

test(substitution_sin_squared_cos) :-
    primitiveFunction(sin(x)^2*cos(x), x, R),
    R == 1/3*sin(x)^3+c.

test(substitution_ln_over_x) :-
    primitiveFunction(ln(x)/x, x, R),
    R == 1/2*ln(x)^2+c.

test(substitution_exp_fraction) :-
    primitiveFunction(e^x/(1+e^x), x, R),
    R == ln(abs(e^x+1))+c.

test(substitution_one_over_x_ln_x) :-
    primitiveFunction(1/(x*ln(x)), x, R),
    R == ln(abs(ln(x)))+c.

% functions without an elementary primitive function must fail instead of
% returning a wrong result
test(non_elementary_sin_x_squared, [fail]) :-
    primitiveFunction(sin(x^2), x, _).

test(non_elementary_exp_x_squared, [fail]) :-
    primitiveFunction(e^(x^2), x, _).

test(non_elementary_x_to_x, [fail]) :-
    primitiveFunction(x^x, x, _).

test(non_elementary_sin_sqrt, [fail]) :-
    primitiveFunction(sin(x)*sqrt(x), x, _).

:- end_tests(prime_functions).

% Test cases for substitute/4

:- begin_tests(substitution).

test(substitute_variable) :-
    substitute(cos(x^2 + 3*x)*x, x, a, R),
    R == cos(a^2+3*a)*a.

test(substitute_unknown_functions) :-
    substitute(arctanh(x)+sinh(x), x, 1, R),
    R == arctanh(1)+sinh(1).

test(substitute_subterm) :-
    substitute(sin(2*x)*x, 2*x, u, R),
    R == sin(u)*x.

:- end_tests(substitution).

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
