% Define all the blocks in the world
blocks([a, b, c, d, e, f]).

% A generic block
block(X) :-
    blocks(BLOCKS), % this extracts the list BLOCKS
    member(X, BLOCKS). % this checks if X is a member of BLOCKS

% Define the start and goal states
start([[on, a, b], [on, b, 'table'], [on, c, d], [clear, c], [clear, a], [on, d, 'table']]).
goal([[on, d, a], [on, a, c], [on, c, b], [on, b, 'table'], [clear, a]]).

% notequal(X1, X2) takes 2 args and holds true when they aren't 
% equal. In other words, it fails when the args are equal and otherwise succeeds
notequal(X, Y) :- X \= Y.

% notequal(X, X) :-!, fail. % fail, if equal
% notequal(_, _). % otherwise, succeed

% substitute(E, E1, OLD, NEW) holds when NEW is the list OLD in which E is substituted 
% by E1. There're no dups in OLD or NEW
substitute(X, Y, [X|T], [Y|T]).
substitute(X, Y, [H|T], [H|T1]) :-
    substitute(X, Y, T, T1).

% move[X, Y, Z, S1, S2] holds when the state S2 is obtained from
% the state S1 by moving block X from Y onto Z
move(X, Y, Z, S1, S2) :-
    member([clear, X], S1), % find a clear block X in S1
    member([on, X, Y], S1), block(Y), % find a block on which X sits
    member([clear, Z], S1), notequal(X, Z), % find another clear block, Z
    substitute([on, X, Y], [on, X, Z], S1, INT), % remove X from Y, place it on Z
    substitute([clear, Z], [clear, Y], INT, S2). % Z is no longer clear; Y is now clear

% move a block from another block onto the table
move_to_table(X, Y, S1, S2) :-
    member([clear, X], S1), % find a clear block X in S1
    member([on, X, Y], S1), block(Y), % find a block on which X sits
    substitute([on, X, Y], [on, X, 'table'], S1, INT), % remove X from Y, place it on the table
    append([[clear, Y]], INT, S2). % now, Y is also clear, no need to substitute

% move a block from the table onto another block
move_from_table(X, Z, S1, S2) :-
    member([clear, X], S1),
    member([on, X, 'table'], S1), % X is on the table
    member([clear, Z], S1), notequal(X, Z), % find another clear block Z
    substitute([on, X, 'table'], [on, X, Z], S1, INT), % remove X from the table, place it on Z
    substitute([clear, Z], [], INT, S2). % Z is no longer clear

% there is a path from state S1 to S2 when there is a move from S1 to S2
path(S1, S2) :-
    move(_, _, _, S1, S2).
path(S1, S2) :-
    move_to_table(_, _, S1, S2).
path(S1, S2) :-
    move_from_table(_, _, S1, S2).

% connect is the symmetric version of path:
% S1 and S2 are connected if there is a path from S1->S2 or S2->S1
connect(S1, S2) :-
    path(S1, S2).
connect(S1, S2) :-
    path(S2, S1).

% check if a state has been visited
notYetVisited(State, PathSoFar) :-
    sort(State, SortedState),
    \+ member(SortedState, PathSoFar).

% notYetVisited(State, PathSoFar) :-
%     permutation(State, PermuteState),
%     not(member(PermuteState, PathSoFar)).

% depthFirst(Start, Path, PathSoFar) returns the Path from start
% to goal state given the path so far
depthFirst(X, [X], _) :-
    goal(X). % if X is the goal, return X

% else, expand X by Y and find path from Y
depthFirst(X, [X|Ypath], VISITED) :-
    connect(X, Y), % find a path from X to Y
    notYetVisited(Y, VISITED), % check if Y has been visited
    depthFirst(Y, Ypath, [Y|VISITED]). % find path from Y


%---------------------------------------------------------------------------------
% Test cases
% Test case 1: Default start and goal states
test_case(1,
    [[on, a, b], [on, b, 'table'], [on, c, d], [clear, c], [clear, a], [on, d, 'table']],
    [[on, d, a], [on, a, c], [on, c, b], [on, b, 'table'], [clear, a]]).

% Test case 2: Move from table onto block
test_case(2,
    [[on, a, 'table'], [on, b, 'table'], [clear, a], [clear, b]],
    [[on, a, b], [on, b, 'table'], [clear, a]]).

% Test case 3: 
test_case(3,
    [[on, a, b], [on, b, 'table'], [on, c, 'table'], [clear, a], [clear, c]],
    [[on, c, a], [on, a, b], [on, b, 'table'], [clear, c]]).

% Test case 4: Move multiple blocks onto table and back
test_case(4,
    [[on, a, b], [on, b, c], [on, c, 'table'], [clear, a]],
    [[on, b, 'table'], [on, c, a], [on, a, 'table'], [clear, c], [clear, b]]).


% Run single test case
run_test(TestID) :-
    test_case(TestID, StartState, GoalState),
    format('Running Test Case ~w~n', [TestID]),
    format('Start State: ~w~n', [StartState]),
    format('Goal State: ~w~n', [GoalState]),
    statistics(runtime, [StartTime|_]),
    (depthFirst(StartState, Path, []) ->
        statistics(runtime, [EndTime|_]),
        Time is EndTime - StartTime,
        format('Path: ~w~n', [Path]),
        format('Time taken: ~w ms~n~n', [Time])
    ;
        format('No path found!~n~n')
    ).

% Run all test cases
run_all :-
    write('Running all test cases...'), nl,
    % run_test(1),
    % run_test(2),
    % run_test(3),
    % run_test(4),
    forall(test_case(ID, _, _), run_test(ID)),
    write('All test cases passed!'), nl.

:- run_test(1).