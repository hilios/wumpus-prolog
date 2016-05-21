stench_at(-1, -1).
breeze_at(-1, -1).

% ---------------------------- %
% Inferences rules             %
% ---------------------------- %
% Infer pit or wumpus if sensed an danger in two adjacents blocks.
is_dangerous(X, Y) :- has_pit(X, Y); has_wumpus(X, Y).

has_pit(X, Y) :-
  E is X + 1, N is Y + 1, breeze_at(E, Y), breeze_at(X, N), !;
  N is Y + 1, W is X - 1, breeze_at(X, N), breeze_at(W, Y), !;
  W is X - 1, S is Y - 1, breeze_at(W, Y), breeze_at(X, S), !;
  S is Y - 1, E is X + 1, breeze_at(X, S), breeze_at(E, Y), !.

has_wumpus(X, Y) :-
  E is X + 1, N is Y + 1, stench_at(E, Y), stench_at(X, N), !;
  N is Y + 1, W is X - 1, stench_at(X, N), stench_at(W, Y), !;
  W is X - 1, S is Y - 1, stench_at(W, Y), stench_at(X, S), !;
  S is Y - 1, E is X + 1, stench_at(X, S), stench_at(E, Y), !.

% Knowledge database, mark where I found stench and breeze
mark(breeze) :-
  hunter(X, Y, _), assertz( breeze_at(X, Y) ),

mark(stench) :-
  hunter(X, Y, _), assertz( stench_at(X, Y) ),

% ---------------------------- %
% Define heuristics            %
% ---------------------------- %

% perceptions([Stench, Breeze, Glitter, Bump, Scream])
heuristic([_, _, _, _, _], exit) :- hunter(1, 1, _), has_gold(yes), !.

heuristic([_, _, yes, _, _], grab) :- !.

heuristic([_, _, _, yes, _], grab) :- .

heuristic([yes, _, _, _, _], noop) :- % I don't know were Wumpus is
  mark(stench),
  !.

heuristic([_, yes, _, _, _], noop) :-
  mark(breeze),
  !.
