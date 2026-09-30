# Integral and Derivative Calculator

**author**: Lukáš Hofman  
Updated September 2026 

## Description
This Prolog program calculates integrals and derivatives of mathematical functions. It uses a library created by Jakub Smolík for simplifying mathematical expressions - https://github.com/Couleslaw/Expression-simplification. (The whole Expression-simplification folder). Integration by substitution is supported only in the form ∫ f(g(x))·g'(x) dx = F(g(x)) (see [Integrals](#integrals)), because in general it's challenging to choose what to substitute for. Functions without an elementary primitive function (e.g. `sin(x^2)`, `e^(x^2)`, `x^x`) fail instead of returning a wrong result. Additionally, it utilizes `simple.pl` to handle fractions and prevent recursion errors.

## Files
- `simplify.pl`: Library for simplifying mathematical expressions.
- `simple.pl`: Additional simplifications for fractions and recursion handling.
- `integrals.pl`: The main program file containing the integral and derivative logic.
- `tests.pl`: Tests for the integrals.pl predicates. Run them with `swipl tests.pl`.

## Notation
- The variable is any atom, e.g. `x`. Other atoms (`a`, `y`, ...) are treated as constants.
- `e` and `pi` are the usual constants, `e^x` and `exp(x)` are the same function (results are always printed as `e^x`).
- `ln(x)` and `log(x)` are both the natural logarithm (results are printed as `ln`).
- Supported functions: `sin, cos, tan, cot, sec, csc`, `arcsin, arccos, arctan, arccot, arcsec, arccsc`, `sinh, cosh, tanh, coth`, `arcsinh, arccosh, arctanh, arcoth`, `ln, log, exp, sqrt, abs`.

# Usage

1. **Load the Required Libraries:**

   Ensure that the `simplify.pl` and `simple.pl` files are available and loaded in the correct folders. All you need to do is load the `integrals.pl` file.

## Simplify
`simplify/2` is a small wrapper around `simp/2` from the library. It renames `ln` to `log` and `e^E` to `exp(E)` before calling the library (and back afterwards), evaluates `abs` of numeric constants and substitutes well known values (`arctan(1) = pi/4`, `ln(e) = 1`, `sinh(0) = 0`, ...). All results of `derivative`, `integral` and `primitiveFunction` go through it.
```prolog
> ?- simplify(ln(abs(e^2)) - arctan(1), R).
> R = - (1/4*pi)+2.
```

## Simple
Bevare that the simple predicate is in no way complete or perfect as it wasn't in my job description, however, it simplifies number of fractions that occur often in the process of integrating.
```prolog
% simple(+Term, -Result)
% Term: a mathematical expression
% Result: the simplified expression
% simple only simplifies fractions, not all of them either.
> ?- simple(1/x*(2*x^2),R). 
> R = 2*x^1.
```

## Substitution

Replace a variable \( X \) with a value \( V \) in a function \( F \).

\( X \) may be any subterm, not only a variable.

```prolog
% substitute(+F, +X, +V, -F1)
% F1 is the result of replacing X with V in F

> ?- substitute(cos(x^2 + 3*x)*x, x, a, R).
> R = cos(a^2+3*a)*a.

> ?- substitute(sin(2*x)*x, 2*x, u, R).
> R = sin(u)*x.
```


## Check for variable
Check if Term contains variable we want to work with (Derive, integrate)
```prolog
% contains(+X, +Term)
% True if Term contains the variable X

> ?- contains(x, x^2 + 3*y).
> true.

> ?- contains(z, x^2 + 3*y).
> false.
```

# Derivatives
Derives the function by the chosen variable
- derive(+Function, +X, -Result): does the derivation only. Can show more then 1 result as it's only a helper predicate. (all are correct)
- derivative(+Function, +X, -Result) simplifies the expression after the derivation using simplify made by Jakub Smolík

The chain rule is applied to all supported functions, including powers and exponentials of compound functions (`sin(x)^3`, `e^(x^2)`, `2^x`, `x^x`, `x^(a+1)`).

```prolog
% derive(+Function,+X,-Result) derive is a helper 
% function for derivative

> ?- derive(cos(x^2 + 3*x)*x, x, R).        
> R = cos(x^2+3*x)*1+ -sin(x^2+3*x)*(2*x^1*1+(3*1+0*x))*x ;
> false.

% derivative(+Function,+X,-Result):- Result is a
%                       derive of Function   
%                       with respect to X. 
%                       Function and X are base terms.

> ?- derivative(cos(x^2 + 3*x)*x, x, R). 
> R = - (2*(sin(x^2+3*x)*x*(x+3/2)))+cos(x^2+3*x).

> ?- derivative(x^x, x, R).
> R = x^x*(ln(x)+1).
```

# Integrals
- integralFunction(+Function, +X, +Depth, -Result) is a helper predicate that does most of the computations. Depth counts nested integrations by parts; when it exceeds `max_depth/1` (6) the exception `depth_limit_exceeded` is thrown, which `integral` and `primitiveFunction` turn into a failure.
- integral(+F, +X, +Start, +End, -Result) is the function that calculates the definite integrals. integral2/5 is a helper predicate and it exists only because if there was only one integral predicate, the exception in case of a too difficult integral would not be handled correctly.
- primitiveFunction(+F, +X, -Result) is the function that calculates the primitive function. primitiveFunction2/3 is a helper predicate for the same reason as integral2/5.

What can be integrated:
- constants (anything not containing the variable), `x^n` for any constant `n` (including `-1`, negative and rational exponents), `1/x^n`, `sqrt(x)`, `1/sqrt(x)`
- all functions listed in [Notation](#notation), `a^x`, and the standard forms `1/(x^2+a)`, `1/(a-x^2)`, `1/(x^2-a)`, `1/sqrt(a-x^2)`, `1/sqrt(x^2+a)`, `1/sqrt(x^2-a)`, `1/(x*sqrt(x^2-1))` for a positive constant `a`
- powers of goniometrical functions: `sin(x)^n`, `cos(x)^n`, `tan(x)^n` (reduction formulas), `cot(x)^2`, `sec(x)^2`, `csc(x)^2`, `1/cos(x)^2`, `1/sin(x)^2`, `sin(x)*cos(x)`
- `e^x*sin(x)`, `e^x*cos(x)`
- sums, differences, constant multiples, `(f+g)/h`, `x^n/x^m`
- substitution ∫ c·f(g(x))·g'(x) dx = c·F(g(x)): the program looks for a subterm `g(x)` such that the rest of the integrand is a constant multiple of `g'(x)`, e.g. `sin(2*x+1)`, `e^(3*x)`, `1/(2*x+1)`, `(x+1)^5`, `x*sin(x^2)`, `x/(x^2+1)`, `e^x/(1+e^x)`, `sin(x)^2*cos(x)`, `ln(x)/x`, `1/(x*ln(x))`
- integration by parts for products, e.g. `x^3*sin(x)`, `x^2*e^(-x)`, `x*ln(x)^2`, `x*arctan(x)`, `x*2^x`

Not supported: general rational functions (`x/(2*x+1)`), products that would require a substitution inside integration by parts, and of course functions without an elementary primitive function (`sin(x^2)`, `e^(x^2)`, `x^x`, `sin(x)*sqrt(x)`) - these fail.

```prolog
% integralFunction(+Function, +X, +Depth, -Result):- Result is the prime 
%                      function of the function Function 
%                      with respect to X. 
%                      Function and X are base terms.
%                      integralFunction is a helper function for integral and primitiveFunction

> ?- integralFunction(sin(x), x, 0, R).
> R = -cos(x) ;
> false.

% integral(+F, +X, +Start, +End, -Result) :- Result is the
%                      integral of the function F with respect
%                      to X in the interval [Start, End]

> ?- integral(sin(x), x, 0, pi, R).    
> R = 2.

> ?- integral(1/(x^2+4), x, 0, 2, R).
> R = 1/8*pi.

> ?- integral(x, x, a, b, R).
> R = 1/2*b^2-1/2*a^2.

% primitiveFunction(+F, +X, -Result) :- Result is the prime
%                      function of the function F with respect
%                      to X

> ?- primitiveFunction(x^3*sin(x), x, R).
> R = 6*(x*cos(x))-x^3*cos(x)+3*(x^2*sin(x))-6*sin(x)+c.

> ?- primitiveFunction(x*sin(x^2), x, R).
> R = - (1/2*cos(x^2))+c.

> ?- primitiveFunction(e^x/(1+e^x), x, R).
> R = ln(abs(e^x+1))+c.

> ?- primitiveFunction(sin(x^2), x, R).
> false.
```
