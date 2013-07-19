:load peano.pl

% distinct solutions: 4: 2, 5: 10,  6: 4, 7: 40

range(M, N, [M|Ns]) :-
    lt(M, N),
    add(M, 1, M1),
    range(M1, N, Ns).
range(N, N, [N]).

% remove one occurance of X from list
select(X, #cons(X, Xs), Xs).
select(X, #cons(Y, Ys), [Y|Zs]) :- select(X, Ys, Zs).

permutation(Xs, [Z|Zs]) :-
    select(Z, Xs, Ys),
    permutation(Ys, Zs).
permutation(nil, nil).

queens(N, Qs) :-
    % generate
    range(1, N, Ns),
    permutation(Ns, Qs),
    % test
    safe(Qs).

safe([Q|Qs]) :- safe(Qs), not(attack(Q, Qs)).
safe(nil).

attack(X, Xs) :- attack(X, 1, Xs).

attack(X, N, [Y|_])  :- add(Y, N, X).
attack(X, N, [Y|_])  :- sub(Y, N, X).
attack(X, N, [_|Ys]) :- add(N, 1, N1), attack(X, N1, Ys).
