member(A, #cons(A, _)).
member(A, #cons(_, Rest)) :- member(A, Rest).

concat(nil, A, A).
concat(#cons(H, T), L, #cons(H, R1)) :- concat(T, L, R1).
