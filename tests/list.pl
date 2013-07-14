member(A, #cons(A, _)).
member(A, #cons(_, Rest)) :- member(A, Rest).
