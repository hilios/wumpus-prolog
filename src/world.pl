% ---------------------------- %
% World predicates             %
% ---------------------------- %

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
% Test world
% wumpus(1, 3).
% pit(4, 1).
% pit(3, 3).
% pit(4, 4).
% gold(2, 3).

% Test world
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
% gold(4, 4).

% Random world
:- random_between(2, 5, X), random_between(2, 5, Y), assertz(gold(X, Y)).
:- random_between(2, 5, X), random_between(2, 5, Y), assertz(wumpus(X, Y)).
:- random_between(2, 5, X), random_between(2, 5, Y), assertz(pit(X, Y)).
:- random_between(2, 5, X), random_between(2, 5, Y), assertz(pit(X, Y)).
:- random_between(2, 5, X), random_between(2, 5, Y), assertz(pit(X, Y)).
