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

% ---------------------------- %
% Define heuristics            %
% ---------------------------- %
% heuristic(avoid_pit) :-
%   write('Avoiding pit. '),
%   hunter(X, Y, _), assertz( breeze_at(X, Y) ).
%
% heuristic(avoid_wumpus) :-
%   write('Avoiding wumpus. '),
%   hunter(X, Y, _), assertz( stench_at(X, Y) ).
%
% heuristic(get_back) :- write('Get back. ').

% perceptions([Stench, Breeze, Glitter, Bump, Scream])
heuristic([_, _, _, _, _], exit) :- hunter(1, 1, _), has_gold(yes), !.

% take_action([_, _, _, _, _],  A) :-
%   hunter(X, Y, _), has_gold(G), X \== 1, Y \== 1, G == gold,
%   heuristic(get_back),
%   A = forward,
%   !.

heuristic([_, _, yes, _, _], grab) :- !.

% take_action([_, _, _, yes, _], A) :-
%   hunter(_, _, east),  A = turnleft;
%   hunter(_, _, north), A = turnleft;
%   !.
%
% take_action([yes, _, _, _, _], A) :- % I don't know were is Wumpus
%   heuristic(avoid_wumpus),
%   next(X, Y), \+ has_wumpus(X, Y),
%   A = [turnleft, turnleft, forward],
%   !.
%
% take_action([yes, _, _, _, _], A) :- % I don't know were is Wumpus
%   heuristic(avoid_wumpus),
%   next(X, Y), has_wumpus(X, Y),
%   A = [turnleft, turnleft, forward],
%   !.
%
% take_action([_, yes, _, _, _], A) :-
%   heuristic(avoid_pit),
%   A = [turnleft, turnleft, forward],
%   !.

take_action([_, _, _, _, _], forward)   :- !.
