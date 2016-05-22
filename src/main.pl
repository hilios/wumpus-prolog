:- abolish(hunter/3).
:- abolish(wumpus/2).
:- abolish(pit/2).
:- abolish(gold/2).
:- abolish(grab/2).
:- abolish(actions/1).
:- abolish(visited/2).
:- abolish(shooted/2).
:- abolish(runloop/1).

:- dynamic([
  hunter/3,
  wumpus/2,
  pit/2,
  gold/2,
  grab/2,
  actions/1,
  shooted/2,
  visited/2
]).

% Defines the world NxM matrix.
world(4, 4).

% Initial player position
hunter(1, 1, east).
visited(1, 1).

% ---------------------------- %
% Environment predicates       %
% ---------------------------- %
has_gold(yes) :- grab(X, Y), gold(X, Y), !.
has_gold(no).

has_arrows(no) :- shooted(_, _), !.
has_arrows(yes).

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
has_scream(yes) :- is_wumpus(dead), !.
has_scream(no).

% Check player's condition
is_player(dead) :- hunter(X, Y, _), wumpus(X, Y), !.
is_player(dead) :- hunter(X, Y, _), pit(X, Y),    !.
is_player(alive).

% Check Wumpus condition
is_wumpus(dead) :- shooted(X, Y), wumpus(X, Y), !.
is_wumpus(alive).

% Check if position is into map bounds.
in_bounds(X, Y) :-
  world(W, H),
  X > 0, X =< W,
  Y > 0, Y =< H.

% Returns the current percetions
perceptions([Stench, Breeze, Glitter, Bump, Scream]) :-
  has_stench(Stench), has_breeze(Breeze), has_glitter(Glitter),
  has_bump(Bump), has_scream(Scream), !.

% Moves the Player to a new position.
move(X, Y) :-
  assertz(actions(move)),
  in_bounds(X, Y),
  % format("- Moving to ~dx~d~n", [X, Y]),
  direction(X, Y, D),
  retractall(hunter(_, _, _)),
  asserta(hunter(X, Y, D)),
  assertz(visited(X, Y)),
  !.
move(X, Y) :- format('!: Cannot move to ~dx~d~n', [X, Y]).

% Get the direction
direction(X, Y, D) :- hunter(Xi, Yi, _), X > Xi, Y == Yi, D = east,   !.
direction(X, Y, D) :- hunter(Xi, Yi, _), X == Xi, Y < Yi, D = north,  !.
direction(X, Y, D) :- hunter(Xi, Yi, _), X < Xi, Y == Yi, D = west,   !.
direction(X, Y, D) :- hunter(Xi, Yi, _), X == Xi, Y > Yi, D = south,  !.
direction(_, _, D) :- hunter(_, _, D).

% Shoot at given position
shoot(_, _) :- has_arrows(no), write('!: I do not have arrows anymore.'), !.
shoot(X, Y) :-
  assertz(actions(shoot)),
  has_arrows(yes),
  assertz(shooted(X, Y)).

% Get all adjacent blocks
neighbors(N) :- findall([X, Y], neighbors(X, Y), N).

% Define the adjacents blocks
neighbors(X, Y) :- hunter(Xi, Yi, _), E is Xi+1, in_bounds(E, Yi), X is E,  Y is Yi.
neighbors(X, Y) :- hunter(Xi, Yi, _), N is Yi+1, in_bounds(Xi, N), X is Xi, Y is N.
neighbors(X, Y) :- hunter(Xi, Yi, _), W is Xi-1, in_bounds(W, Yi), X is W,  Y is Yi.
neighbors(X, Y) :- hunter(Xi, Yi, _), S is Yi-1, in_bounds(Xi, S), X is Xi, Y is S.

% Player's actions
action(exit) :- write('Bye, bye!'), nl, print_result, nl, print_world, nl, halt.

action([move,  X, Y]) :- move(X, Y).
action([shoot, X, Y]) :- shoot(X, Y).

action(grab) :-
  assertz(actions(grab)),
  hunter(X, Y, _), assertz(grab(X, Y)),
  (gold(X, Y), has_gold(no)) ->
    write('!: Found gold! '), nl;
    true.

% A naive random move
action(random) :-
  neighbors(N), length(N, L), random_between(1, L, R), nth1(R, N, [X, Y]),
  move(X, Y).

action(noop).

% Score
score(S) :- findall(P, points(P), Ps), sum_list(Ps, S).

points(P) :- steps(T),        P is -T.
points(P) :- is_player(dead), P is -1000.
points(P) :- has_gold(yes),   P is +1000.

steps(S) :- findall(A, actions(A), As), length(As, S).

% Print
print_result :-
  format('~n~tResult~t~40|~n'),
  score(S), steps(T),
  format('Steps: ~`.t ~d~40|', [T]), nl,
  format('Score: ~`.t ~d~40|', [S]), nl,
  (has_gold(yes), hunter(1, 1, _)) ->
    format('Outcome: ~`.t ~p~40|', [win]), nl;
    format('Outcome: ~`.t ~p~40|', [loose]), nl.

print_world :-
  is_player(P), hunter(Hx, Hy, _),
  format('~n~tWorld~t~40|~n'),
  format('Player: ~`.t ~p at ~dx~d~40|', [P, Hx, Hy]), nl,
  is_wumpus(W), wumpus(Wx, Wy),
  format('Wumpus: ~`.t ~p at ~dx~d~40|', [W, Wx, Wy]), nl,
  has_gold(G), gold(Gx, Gy),
  format('Gold: ~`.t~p at ~dx~d~40|', [G, Gx, Gy]), nl,
  findall([Px, Py], pit(Px, Py), Ps),
  format('Pit: ~`.t ~p~40|', [Ps]), nl.

% Run the game
run(random) :-
  random_between(2, 5, X1), random_between(2, 5, Y1), assertz(gold(X1, Y1)),
  random_between(2, 5, X2), random_between(2, 5, Y2), assertz(wumpus(X2, Y2)),
  random_between(2, 5, X3), random_between(2, 5, Y3), assertz(pit(X3, Y3)),
  random_between(2, 5, X4), random_between(2, 5, Y4), assertz(pit(X4, Y4)),
  random_between(2, 5, X5), random_between(2, 5, Y5), assertz(pit(X5, Y5)),
  run.

run([Gx, Gy], [Wx], [Wy], [P1x, P1y], [P2x, P2y], [P3x, P3y]) :-
  assertz(gold(Gx, Gy)),
  assertz(wumpus(Wx, Wy)),
  assertz(pit(P1x, P1y)),
  assertz(pit(P2x, P2y)),
  assertz(pit(P3x, P3y)),
  run.

run :- runloop(0).

runloop(200) :- write('!: Reached max allowed moves.'), nl, action(exit), !.
runloop(T) :-
  hunter(X, Y, D), perceptions(P),
  format('~d: At ~dx~d facing ~p, senses ~p. ', [T, X, Y, D, P]),
  heuristic(P, A),
  format('I\'m doing ~p.~n', [A]),
  action(A),
  % Iterate
  is_player(dead) -> (
    write('You have deceased.'), nl,
    action(exit),
    !);
  Ti is T + 1,
  runloop(Ti).
