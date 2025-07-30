# Integral and Derivative Calculator

**author**: Lukáš Hofman  
MFF UK 2024 

## Description
This Prolog program calculates integrals and derivatives of mathematical functions. It uses a library created by Jakub Smolík for simplifying mathematical expressions - https://github.com/Couleslaw/Expression-simplification. (The whole Expression-simplification folder). However, it doesn't support integrating compound functions as it can be without an integral aswell as integrating  by substitution as it's challenging to choose what to substitute for. Additionally, it utilizes `simple.pl` to handle fractions and prevent recursion errors.

## Files
- `simplify.pl`: Library for simplifying mathematical expressions.
- `simple.pl`: Additional simplifications for fractions and recursion handling.
- `integrals.pl`: The main program file containing the integral and derivative logic.
- `tests.pl`: Tests for the integrals.pl predicates. 

# Usage

1. **Load the Required Libraries:**

   Ensure that the `simplify.pl` and `simple.pl` files are available and loaded in the correct folders. All you need to do is load the `integrals.pl` file.

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

```prolog
% substitute(+F, +X, +V, -F1)
% F1 is the result of replacing X with V in F

> ?- substitute(cos(x^2 + 3*x)*x, x, a, R).
> R = cos(a^2+3*a)*a.
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

```prolog
% derive(+Function,+X,-Result) derive is a helper 
% function for derivative

> ?- derive(cos(x^2 + 3*x)*x, x, R).        
> R = cos(x^2+3*x)*1+ -sin(x^2+3*x)*(2*x+(3*1+0*x))*x ;
> R = cos(x^2+3*x)*1+ -sin(x^2+3*x)*(2*x^1+(3*1+0*x))*x ;
> false.

% derivative(+Function,+X,-Result):- Result is a
%                       derive of Function   
%                       with respect to X. 
%                       Function and X are base terms.

> ?- derivative(cos(x^2 + 3*x)*x, x, R). 
> R = - (2*(sin(x^2+3*x)*x*(x+3/2)))+cos(x^2+3*x).
```

# Integrals
- integralFunction(+Function, +X, +Depth, -Result) is a helper predicate that does most of the computations. Depth is usually not important, it's only taken into consideration in integrating multiplications where both parts have variable that is being integrated. Max depth is set to 5.
- integral(F, X, Start, End, Result) is the function that calculates the definite integrals. integral2/5 is a helper predicate and it exists only because if there was only one integral predicate, the exception in case of a too difficult integral would not be handled correctly.
- primitiveFunction(F, X, Result) is the function that calculates the primitive function. primitiveFunction2/3 is a helper predicate for the same reason as integral2/5.

```prolog
% integralFunction(+Function, +X, +Depth, -Result):- Result is the prime 
%                      function of the function Function 
%                      with respect to X. 
%                      Function and X are base terms.
%                      integralFunction is a helper function for integral and primitiveFunction

> ?- integralFunction(sin(x), x, 0, R).
> R = -cos(x) ;
> false.

% integral(+F, +X, +Start, +End, +Depth, -Result) :- Result is the
%                      integral of the function F with respect
%                      to X in the interval [Start, End]

> ?- integral(sin(x), x, 0, pi, R).    
> R = 2.

% primitiveFunction(+F, +X, -Result) :- Result is the prime
%                      function of the function F with respect
%                      to X

> ?- primitiveFunction(x^3*sin(x), x, R).
> R = 6*(x*cos(x))-x^3*cos(x)+3*(x^2*sin(x))-6*sin(x)+c.
```
