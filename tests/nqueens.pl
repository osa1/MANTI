:load peano.pl

range(M, N, #cons(M, Ns)) :-
    lt(M, N),
    add(M, 1, M1),
    range(M1, N, Ns).
range(N, N, [N]).

% remove one occurance of X from list
select(X, #cons(X, Xs), Xs).
select(X, #cons(Y, Ys), #cons(Y, Zs)) :- select(X, Ys, Zs).

permutation(Xs, #cons(Z, Zs)) :-
    select(Z, Xs, Ys),
    permutation(Ys, Zs).
permutation(nil, nil).

queens(N, Qs) :-
    % generate
    range(1, N, Ns),
    permutation(Ns, Qs),
    % test
    safe(Qs).

safe(#cons(Q, Qs)) :- safe(Qs), not(attack(Q, Qs)).
safe(nil).

attack(X, Xs) :- attack(X, 1, Xs).

attack(X, N, #cons(Y, _))  :- add(Y, N, X).
attack(X, N, #cons(Y, _))  :- sub(Y, N, X).
attack(X, N, #cons(_, Ys)) :- add(N, 1, N1), attack(X, N1, Ys).
