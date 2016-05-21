% ---------------------------- %
% Define heuristics            %
% ---------------------------- %

% [Stench, Breeze, Glitter, Bump, Scream]
% Grab the gold if sees it shining
heuristic([_, _, yes, _, _], grab) :- !.

% Exit when reaches gold
heuristic([_, _, _, _, _], exit) :- hunter(1, 1, _), has_gold(yes), !.

% Apply a random move
heuristic([_, _, _, _, _], random) :- !.
