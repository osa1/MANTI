member(A, [A|_]).
member(A, [_|Rest]) :- member(A, Rest).

append([], A, A).
append([H|T], L, [H|R1]) :- append(T, L, R1).

% remove one occurance of X from list
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :- select(X, Ys, Zs).
