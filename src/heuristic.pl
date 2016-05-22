:- dynamic([
  stench_at/2,
  breeze_at/2
]).

% ---------------------------- %
% Inferences rules             %
% ---------------------------- %
% Infer pit or wumpus if sensed an danger in two adjacents blocks.
is_dangerous(yes) :-
  perceptions([yes, _, _, _, _]);
  perceptions([_, yes, _, _, _]).
is_dangerous(no).

has_pit(X, Y, yes) :-
  E is X + 1, N is Y + 1, breeze_at(E, Y), breeze_at(X, N), !;
  N is Y + 1, W is X - 1, breeze_at(X, N), breeze_at(W, Y), !;
  W is X - 1, S is Y - 1, breeze_at(W, Y), breeze_at(X, S), !;
  S is Y - 1, E is X + 1, breeze_at(X, S), breeze_at(E, Y), !.
has_pit(X, Y, maybe) :-
  E is X + 1, breeze_at(E, Y), !;
  N is Y + 1, breeze_at(X, N), !;
  W is X - 1, breeze_at(W, Y), !;
  S is Y - 1, breeze_at(X, S), !.
has_pit(_, _, no).

has_wumpus(X, Y, yes) :-
  E is X + 1, N is Y + 1, stench_at(E, Y), stench_at(X, N), !;
  N is Y + 1, W is X - 1, stench_at(X, N), stench_at(W, Y), !;
  W is X - 1, S is Y - 1, stench_at(W, Y), stench_at(X, S), !;
  S is Y - 1, E is X + 1, stench_at(X, S), stench_at(E, Y), !;
  N is Y + 1, S is Y - 1, stench_at(X, N), stench_at(X, S), !;
  E is X + 1, W is Y + 1, stench_at(E, Y), stench_at(W, Y), !.
has_wumpus(X, Y, maybe) :-
  E is X + 1, stench_at(E, Y), !;
  N is Y + 1, stench_at(X, N), !;
  W is X - 1, stench_at(W, Y), !;
  S is Y - 1, stench_at(X, S), !.
has_wumpus(_, _, no).

% Knowledge database, add knowledge where I found stench and breeze
add_knowledge(breeze) :- hunter(X, Y, _), assertz(breeze_at(X, Y)).
add_knowledge(stench) :- hunter(X, Y, _), assertz(stench_at(X, Y)).

% ---------------------------- %
% Define heuristics            %
% ---------------------------- %

% [Stench, Breeze, Glitter, Bump, Scream]
heuristic([_, _, _, _, _], exit) :- hunter(1, 1, _), has_gold(yes), !.

heuristic([_, _, yes, _, _], grab) :- !.

heuristic([yes, _, _, _, _], [move, X, Y]) :- % I don't know were Wumpus is
  write('The Wumpus is near. '),
  add_knowledge(stench), safest_path(X, Y),
  !.

heuristic([_, yes, _, _, _], [move, X, Y]) :- % I don't know were Pit is
  write('There is a pit somewhere. '),
  add_knowledge(breeze), safest_path(X, Y),
  !.

% Get back to the beginning
heuristic(_, [move, X, Y]) :-
  has_gold(yes), write('Get back. '), safest_path(X, Y),
  !.

% Exit if have found the same place ten times
heuristic(_, exit) :- hunter(X, Y, _), findall(1, visited(X, Y), V),
  length(V, L), L > 10, write('I Give up! ').

heuristic(_, [move, X, Y]) :-
  safest_path(X, Y),
  !.

% ---------------------------- %
% Helpers                      %
% ---------------------------- %
safest_path(X, Y) :-
  % Calculat all neighbors cost and select the minimun
  findall(C, neighbors_cost(C), L), min_list(L, Min),
  % Returns the position with less cost hence the safest
  index_of(L, Min, I),
  neighbors(N), nth0(I, N, [X, Y]).

neighbors_cost(C) :- neighbors(X, Y), cost(X, Y, C).

% Cost predicate
cost(X, Y, C) :- \+visited(X, Y), has_gold(yes), C is -5, !.
cost(X, Y, C) :- visited(X, Y),         C is 5,   !.
cost(X, Y, C) :- has_pit(X, Y, yes),    C is 100, !.
cost(X, Y, C) :- has_wumpus(X, Y, yes), C is 100, !.
cost(_, _, C) :- is_dangerous(yes),     C is 10,  !.
cost(_, _, C) :- C is 0.

% Get the first occurence of certain value
index_of([H|_], H, 0):- !.
index_of([_|T], H, Index):- index_of(T, H, OldIndex), !, Index is OldIndex + 1.
