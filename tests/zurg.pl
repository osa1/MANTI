:load peano.pl list.pl

time(buzz, 5).
time(woody, 10).
time(rex, 20).
time(hamm, 25).

toys([buzz, hamm, rex, woody]).

max(A, B, A) :-
    time(A, TimeA),
    time(B, TimeB),
    %TimeA > TimeB.
    gt(TimeA, TimeB).
max(A, B, B) :-
    time(A, TimeA),
    time(B, TimeB),
    %TimeA =< TimeB.
    lte(TimeA, TimeB).

min(A, B, A) :-
    time(A, TimeA),
    time(B, TimeB),
    %TimeA < TimeB.
    lt(TimeA, TimeB).
min(A, B, B) :-
    time(A, TimeA),
    time(B, TimeB),
    %TimeA >= TimeB.
    gte(TimeA, TimeB).

% lefts, flashlight position, rights, steps taken, time spent
start([ [buzz, woody, rex, hamm], left, [], [], 0 ]).

end([ [], right, Right, _, TimeSpent ]) :-
    %TimeSpent =< 60,
    lte(TimeSpent, 60),
    member(buzz, Right),
    member(woody, Right),
    member(rex, Right),
    member(hamm, Right).

steps([ _, _, _, Steps, _ ], Steps).

step([ Lefts, left, Rights, Steps, TimeSpent ],
        [ Lefts2, right, Rights1, Steps1, TimeSpent1 ]) :-
    select(X, Lefts, Lefts1),
    select(Y, Lefts1, Lefts2),
    max(X, Y, Max),
    time(Max, MaxTime),

    %TimeSpent1 = TimeSpent + MaxTime,
    add(TimeSpent, MaxTime, TimeSpent1),
    %TimeSpent1 =< 60,
    lte(TimeSpent1, 60),

    append(Rights, [X, Y], Rights1),
    append(Steps, [step(X, Y, right)], Steps1).

step([ Lefts, right, Rights, Steps, TimeSpent ],
        [ Lefts1, left, Rights1, Steps1, TimeSpent1 ]) :-
    select(X, Rights, Rights1),
    time(X, XTime),

    %TimeSpent1 is TimeSpent + XTime,
    add(TimeSpent, XTime, TimeSpent1),
    %TimeSpent1 =< 60,
    lte(TimeSpent1, 60),

    append(Lefts, [X], Lefts1),
    append(Steps, [step(X, left)], Steps1).

% step( [ [ buzz, woody, rex, hamm ], left, [], [], 0 ], Step ).
% step( [ [ rex, hamm ], right, [ buzz, woody ], [], 0 ], Step).

solve(State, Solution) :- end(State), steps(State, Solution).
solve(State, Solution) :-
    step(State, NextState),
    solve(NextState, Solution).

run(Solution) :-
    start(State),
    solve(State, Solution).
