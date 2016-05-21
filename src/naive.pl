% ---------------------------- %
% Define heuristics            %
% ---------------------------- %

% Grab the gold if sees it shining
heuristic([_, _, yes, _, _], grab) :- !.

% Apply a random move
heuristic([_, _, _, _, _], random) :- !.
