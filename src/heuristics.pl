% ---------------------------- %
% Define heuristics            %
% ---------------------------- %
heuristic(avoid_pit) :-
  write('Avoiding pit. '),
  hunter(X, Y, _), assertz( breeze_at(X, Y) ).

heuristic(avoid_wumpus) :-
  write('Avoiding wumpus. '),
  hunter(X, Y, _), assertz( stench_at(X, Y) ).

heuristic(get_back) :- write('Get back. ').

% perceptions([Stench, Breeze, Glitter, Bump, Scream])
take_action([_, _, _, _, _], exit)   :-
  hunter(1, 1, _), has_gold(yes), !.

% take_action([_, _, _, _, _],  A) :-
%   hunter(X, Y, _), has_gold(G), X \== 1, Y \== 1, G == gold,
%   heuristic(get_back),
%   A = forward,
%   !.

take_action([_, _, yes, _, _], grab) :- !.

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
