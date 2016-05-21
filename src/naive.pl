% Executes a naive random move
heuristic([_, _, _, _, _], noop) :-
  neighbors(N), length(N, L), random_between(1, L, R), nth1(R, N, [X, Y]),
  move(X, Y).
