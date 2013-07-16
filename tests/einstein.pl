:load peano.pl list.pl

% http://en.wikipedia.org/wiki/Zebra_Puzzle

% one improvement here might be making mutually exclusive choices,
% this should replace `member` with `choiceExclusive` etc.

world( [ [ 1, _, _, _, _, _ ]
       , [ 2, _, _, _, _, _ ]
       , [ 3, _, _, _, _, _ ]
       , [ 4, _, _, _, _, _ ]
       , [ 5, _, _, _, _, _ ] ] ).

nextTo(Idx1, Idx2) :- eqNat(Idx1, s(Idx2)).
nextTo(Idx1, Idx2) :- eqNat(Idx2, s(Idx1)).

solve(DrinksWater, OwnsZebra) :-
    % There are five houses.
    world(World),
    % [ Index, Nation, Color, Pet, Smoke, Drink ]

    % The Englishman lives in the red house.
    member([_, englishman, red, _, _, _], World),
    % The Spaniard owns a dog.
    member([_, spaniard, _, dog, _, _], World),
    % Coffee is drunk in the green house.
    member([_, _, green, _, _, coffee], World),
    % The Ukranian drinks tea.
    member([_, ukranian, _, _, _, tea], World),

    % green house is immediately to the right of the ivory house.
    member([GreenHouseIdx, _, green, _, _, _], World),
    sub(GreenHouseIdx, 1, IvoryhouseIdx),
    member([IvoryhouseIdx, _, ivory, _, _, _], World),

    % The Old Gold smoker owns snails.
    member([_, _, _, snails, theoldgold, _], World),
    % Kools are smoked in the yellow house.
    member([_, _, yellow, _, kools, _], World),
    % Milk is drunk in the middle house.
    member([3, _, _, _, _, milk], World),
    % The Norwegian lives in the first house.
    member([1, norwegian, _, _, _, _], World),

    % The man who smokes Chesterfields lives in a house next to the man with the fox.
    member([ChesterfieldsIdx, _, _, _, chesterfields, _], World),
    member([FoxIdx, _, _, fox, _, _], World),
    nextTo(FoxIdx, ChesterfieldsIdx),

    % Kools are smoked in a house next to the house where the horse is kept.
    member([KoolsIdx, _, _, _, kools, _], World),
    member([HorseIdx, _, _, horse, _, _], World),
    nextTo(KoolsIdx, HorseIdx),

    % The Lucky Strike smoker drinks orange juice.
    member([_, _, _, _, theluckystrike, orangejuice], World),
    % The Japanese smokes Parliaments.
    member([_, japanese, _, _, parliaments, _], World),

    % The Norwegian lives next to the blue house.
    member([NorwegianIdx, norwegian, _, _, _, _], World),
    member([BlueIdx, _, blue, _, _, _], World),
    nextTo(NorwegianIdx, BlueIdx),

    % Answers
    member([_, DrinksWater, _, _, _, water], World),
    member([_, OwnsZebra, _, zebra, _, _], World).
