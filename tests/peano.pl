% nat test
nat(o).
nat(s(X)) :- nat(X).

% addition
add(o, Y, Y).
add(s(X), Y, Z) :- add(X, s(Y), Z).

% subtraction
sub(o, _, o).
sub(X, o, X).
sub(s(X), s(Y), Z) :- sub(X, Y, Z).

% nat equality
eqNat(s(X), s(Y)) :- eqNat(X, Y).
eqNat(o, o).

% less than
lt(o, s(_)).
lt(s(A), s(B)) :- lt(A, B).

% greater than
gt(s(_), o).
gt(s(A), s(B)) :- gt(A, B).
