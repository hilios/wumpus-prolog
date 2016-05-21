:- abolish(hunter/3).
:- abolish(wumpus/2).
:- abolish(pit/2).
:- abolish(gold/2).
:- abolish(grab/2).
:- abolish(actions/1).
:- abolish(visited/2).
:- abolish(runloop/1).

:- dynamic([
  hunter/3,
  wumpus/2,
  pit/2,
  gold/2,
  grab/2,
  actions/1,
  visited/2
]).

% Defines the world NxM matrix.
world(4, 4).

%     +---+---+---+---+
%   4 |   |   |   | P |
%     +---+---+---+---+
%   3 | W | G | P |   |
%     +---+---+---+---+
%   2 |   |   |   |   |
%     +---+---+---+---+
%   1 | H |   | P |   |
%     +---+---+---+---+
%       1   2   3   4
% Database.
% hunter(1, 1, east).
% wumpus(1, 3).
% pit(3, 1).
% pit(3, 3).
% pit(4, 4).
% gold(2, 3).

% Test database
%     +---+---+---+---+
%   4 |   |   |   | G |
%     +---+---+---+---+
%   3 |   |   |   |   |
%     +---+---+---+---+
%   2 |   |   |   |   |
%     +---+---+---+---+
%   1 | H |   |   |   |
%     +---+---+---+---+
%       1   2   3   4
hunter(1, 1, east).
gold(4, 4).

visited(1, 1).

% ---------------------------- %
% Environment predicates       %
% ---------------------------- %
has_gold(yes) :- grab(X, Y), gold(X, Y), !.
has_gold(no).

has_arrows(yes) :- shoot_at(_, _), !.
has_arrows(no).

% Perceptions
% ===========
% If has gold it has glitter.
has_glitter(yes) :- has_gold(G), G == no, hunter(X, Y, _), gold(X, Y), !.
has_glitter(no).

% Senses breeze if adjacent block has a pit.
has_breeze(yes) :-
  hunter(X, Y, _), N is Y + 1, pit(X, N), !;
  hunter(X, Y, _), S is Y - 1, pit(X, S), !;
  hunter(X, Y, _), E is X + 1, pit(E, Y), !;
  hunter(X, Y, _), W is X - 1, pit(W, Y), !.
has_breeze(no).

% Senses stench if adjacent block has the wumpus.
has_stench(yes) :-
  hunter(X, Y, _), N is Y + 1, wumpus(X, N), !;
  hunter(X, Y, _), S is Y - 1, wumpus(X, S), !;
  hunter(X, Y, _), E is X + 1, wumpus(E, Y), !;
  hunter(X, Y, _), W is X - 1, wumpus(W, Y), !.
has_stench(no).

% Senses bump if is facing a wall
has_bump(yes) :-
  world(W, _), hunter(W, _, east),  !;
  world(_, H), hunter(_, H, north), !;
  hunter(1, _, west),  !;
  hunter(_, 1, south), !.
has_bump(no).

% Senses screm if wumpus have died
has_scream(yes) :- is_wumpus(alive), !.
has_scream(no).

% Check player's condition
is_player(dead) :-
  hunter(X, Y, _), wumpus(X, Y), !;
  hunter(X, Y, _), pit(X, Y),    !.
is_player(alive).

% Check Wumpus condition
is_wumpus(alive) :- wumpus(X, Y), shoot_at(X, Y), !.
is_wumpus(dead).

% Returns the current percetions
perceptions([Stench, Breeze, Glitter, Bump, Scream]) :-
  has_stench(Stench), has_breeze(Breeze), has_glitter(Glitter),
  has_bump(Bump), has_scream(Scream), !.

% Check if position is into map bounds.
in_bounds(X, Y) :-
  world(W, H),
  X > 0, X =< W,
  Y > 0, Y =< H.

% Moves the Player to a new position.
move(X, Y) :-
  assertz(actions(move)),
  in_bounds(X, Y),
  format("- Moving to ~dx~d~n", [X,Y]),
  assertz(visited(X, Y)),
  hunter(_, _, D),
  retractall(hunter(_, _, D)), % Reset the hunter pos then reassign.
  asserta(hunter(X, Y, D)),
  !.
move(X, Y) :- format('- Cannot move to ~dx~d~n', [X, Y]).

% Shoot at position and kill wumpus if its there
shoot(X, Y) :-
  assertz(actions(shoot)),
  has_arrows(yes),
  assertz(shoot_at(X, Y)),
  wumpus(X, Y),
  !.
shoot(_, _) :- write('I don not have arrows anymore.').

% Get all adjacent blocks
neighbors(L) :- findall(N, neighbor(N), L).
neighbor(B) :- hunter(X, Y, _), E is X+1, in_bounds(E, Y), B = [E, Y].
neighbor(B) :- hunter(X, Y, _), N is Y+1, in_bounds(X, N), B = [X, N].
neighbor(B) :- hunter(X, Y, _), W is X-1, in_bounds(W, Y), B = [W, Y].
neighbor(B) :- hunter(X, Y, _), S is Y-1, in_bounds(X, S), B = [X, S].

% Player's actions
action(exit) :- write('- Bye, bye!'), nl, print_result, nl, halt.

action([move,  X, Y]) :- move(X, Y).
action([shoot, X, Y]) :- shoot(X, Y).

action(grab) :-
  assertz(actions(grab)),
  hunter(X, Y, _), assertz( grab(X, Y) ),
  (gold(X, Y), has_gold(no)) ->
    write('- Found gold!'), nl;
    write('- Nothing to grab'), nl.

% A naive random move
action(random) :-
  neighbors(N), length(N, L), random_between(1, L, R), nth1(R, N, [X, Y]),
  move(X, Y).

action(noop).

% Score
score(S) :- findall(A, actions(A), As), length(As, S).

% Print
print_result :-
  score(S),
  format('~n~tResult~t~40|~n'),
  format('Score: ~`.t ~d~40|', [S]), nl,
  (has_gold(yes), hunter(1, 1, _)) ->
    format('Outcome: ~`.t ~p~40|', [win]), nl;
    format('Outcome: ~`.t ~p~40|', [loose]), nl.

% Run the game
run :- runloop(0).

runloop(100) :- write('100: Reached max allowed moves.'), nl, action(exit), !.
runloop(T) :-
  hunter(X, Y, D), perceptions(P),
  format('~d: At ~dx~d facing ~p, senses ~p. ', [T, X, Y, D, P]),
  heuristic(P, A),
  format('I\'m doing ~p.~n', [A]),
  action(A),
  % Iterate
  is_player(alive) -> (
    Ti is T + 1,
    runloop(Ti)
  );
  write('- You have deceased'), nl,
  action(exit),
  !.
