% Define the road network as facts: road(City1, City2, Distance).
road(arad, zerind, 75).
road(arad, sibiu, 140).
road(arad, timisoara, 118).
road(zerind, arad, 75).
road(zerind, oradea, 71).
road(oradea, zerind, 71).
road(oradea, sibiu, 151).
road(sibiu, arad, 140).
road(sibiu, oradea, 151).
road(sibiu, fagaras, 99).
road(sibiu, rimnicu_vilcea, 80).
road(timisoara, arad, 118).
road(timisoara, lugoj, 111).
road(lugoj, timisoara, 111).
road(lugoj, mehadia, 70).
road(mehadia, lugoj, 70).
road(mehadia, drobeta, 75).
road(drobeta, mehadia, 75).
road(drobeta, craiova, 120).
road(craiova, drobeta, 120).
road(craiova, rimnicu_vilcea, 146).
road(craiova, pitesti, 138).
road(rimnicu_vilcea, sibiu, 80).
road(rimnicu_vilcea, craiova, 146).
road(rimnicu_vilcea, pitesti, 97).
road(fagaras, sibiu, 99).
road(fagaras, bucharest, 211).
road(pitesti, rimnicu_vilcea, 97).
road(pitesti, craiova, 138).
road(pitesti, bucharest, 101).
road(bucharest, fagaras, 211).
road(bucharest, pitesti, 101).
road(bucharest, giurgiu, 90).
road(bucharest, urziceni, 85).
road(giurgiu, bucharest, 90).
road(urziceni, bucharest, 85).
road(urziceni, hirsova, 98).
road(urziceni, vaslui, 142).
road(hirsova, urziceni, 98).
road(hirsova, eforie, 86).
road(eforie, hirsova, 86).
road(vaslui, urziceni, 142).
road(vaslui, iasi, 92).
road(iasi, vaslui, 92).
road(iasi, neamt, 87).
road(neamt, iasi, 87).

% Breadth-First Search (BFS) Algorithm
% bfs(Start, Goal, Path, Cost, VisitedCount)
bfs(Start, Goal, Path, Cost, VisitedCount) :-
    % Initialize the queue with the starting node
    bfs([[Start, [Start], 0]], Goal, Path, Cost, 0, VisitedCount).

% If the Goal is found at the head of the queue
bfs([[Goal, PathSoFar, CostSoFar] | _], Goal, Path, Cost, Visited, VisitedCount) :-
    % Reverse the path to get the correct order
    reverse(PathSoFar, Path),
    Cost = CostSoFar,
    VisitedCount is Visited + 1.

% Continue BFS search
bfs([[Current, PathSoFar, CostSoFar] | Rest], Goal, Path, Cost, Visited, VisitedCount) :-
    % Find all adjacent nodes not already in PathSoFar
    findall(
        [Next, [Next | PathSoFar], NewCost],
        (
            road(Current, Next, StepCost),
            \+ member(Next, PathSoFar),
            NewCost is CostSoFar + StepCost
        ),
        NextPaths
    ),
    % Append new paths to the end of the queue
    append(Rest, NextPaths, NewQueue),
    % Increment Visited count and continue search
    bfs(NewQueue, Goal, Path, Cost, Visited + 1, VisitedCount).

% Depth-First Search (DFS) Algorithm
% dfs(Start, Goal, Path, Cost, VisitedCount)
dfs(Start, Goal, Path, Cost, VisitedCount) :-
    dfs_helper(Start, Goal, [Start], Path, 0, Cost, 0, VisitedCount).

% If the Goal is found
dfs_helper(Goal, Goal, PathSoFar, Path, CostSoFar, Cost, Visited, VisitedCount) :-
    reverse(PathSoFar, Path),
    Cost = CostSoFar,
    VisitedCount is Visited + 1.

% Continue DFS search
dfs_helper(Current, Goal, PathSoFar, Path, CostSoFar, Cost, Visited, VisitedCount) :-
    % Explore adjacent nodes
    road(Current, Next, StepCost),
    \+ member(Next, PathSoFar),
    NewCost is CostSoFar + StepCost,
    dfs_helper(Next, Goal, [Next | PathSoFar], Path, NewCost, Cost, Visited + 1, VisitedCount).

% Straight Line Distance Heuristic (SLD) for A* search
% sld(City, DistanceToGoal)
sld(arad, 366).
sld(zerind, 374).
sld(oradea, 380).
sld(timisoara, 329).
sld(lugoj, 244).
sld(mehadia, 241).
sld(drobeta, 242).
sld(craiova, 160).
sld(sibiu, 253).
sld(rimnicu_vilcea, 193).
sld(fagaras, 176).
sld(pitesti, 100).
sld(bucharest, 0).
sld(giurgiu, 77).
sld(neamt, 234).
sld(iasi, 226).
sld(vaslui, 199).
sld(urziceni, 80).
sld(hirsova, 151).
sld(eforie, 161).

% A* Search Algorithm
% Load the heaps library for priority queue implementation
:- use_module(library(heaps)).

% a_star(Start, Goal, Path, Cost, VisitedCount)
a_star(Start, Goal, Path, Cost, VisitedCount) :-
    % Get the heuristic estimate from Start to Goal
    sld(Start, H),
    % Initialize an empty priority queue (OpenSet)
    empty_heap(OpenSet),
    % Add the starting node to the OpenSet
    add_to_heap(OpenSet, H, [Start, [Start], 0], InitialQueue),
    % Begin the A* search
    a_star_search(InitialQueue, Goal, [], Path, Cost, 0, VisitedCount).

% If the Goal is found
a_star_search(Queue, Goal, _, Path, Cost, Visited, VisitedCount) :-
    % Get the node with the lowest F value
    get_from_heap(Queue, _, [Goal, PathReversed, Cost], _),
    reverse(PathReversed, Path),
    VisitedCount is Visited + 1.

% Continue A* search
a_star_search(Queue, Goal, VisitedList, Path, Cost, Visited, VisitedCount) :-
    % Get the node with the lowest F value
    get_from_heap(Queue, _, [Current, PathSoFar, CostSoFar], RestQueue),
    % Generate successors
    findall(
        [F, [Next, [Next | PathSoFar], NewCost]],
        (
            road(Current, Next, StepCost),
            \+ member(Next, VisitedList),
            NewCost is CostSoFar + StepCost,
            sld(Next, H),
            F is NewCost + H
        ),
        Children
    ),
    % Add successors to the priority queue
    add_children_to_queue(Children, RestQueue, NewQueue),
    NewVisited is Visited + 1,
    % Continue the search with the updated queue and visited list
    a_star_search(NewQueue, Goal, [Current | VisitedList], Path, Cost, NewVisited, VisitedCount).

% Helper predicate to add multiple children to the priority queue
add_children_to_queue([], Queue, Queue).
add_children_to_queue([[F, Element] | Children], Queue, NewQueue) :-
    add_to_heap(Queue, F, Element, UpdatedQueue),
    add_children_to_queue(Children, UpdatedQueue, NewQueue).

/*------------------------------------------------------------------------------*/
% Timing and Running the Search Algorithms

% time_alg(Algorithm, StartCity, GoalCity)
% Times the execution of a search algorithm and prints the results
time_alg(Alg, Start, Goal):-
    % Start timing
    statistics(cputime, T0),

    % Call the specified algorithm
    call_alg(Alg, Start, Goal, Path, Cost, VisitedCount),

    % Stop timing
    statistics(cputime, T1),
    Time is (T1 - T0) * 1000,  % Convert time to milliseconds

    % Print the results
    format("~nSearch Algorithm: ~w~n", [Alg]),
    format("Path: ~w~n", [Path]),
    format("Cost: ~w~n", [Cost]),
    format("Cities Visited: ~w~n", [VisitedCount]),
    format("Time: ~3f ms~n", [Time]).

% Helper predicate to call the appropriate search algorithm
call_alg(bfs, Start, Goal, Path, Cost, VisitedCount) :-
    bfs(Start, Goal, Path, Cost, VisitedCount).
call_alg(dfs, Start, Goal, Path, Cost, VisitedCount) :-
    dfs(Start, Goal, Path, Cost, VisitedCount).
call_alg(a_star, Start, Goal, Path, Cost, VisitedCount) :-
    a_star(Start, Goal, Path, Cost, VisitedCount).

% run_all
% Runs all algorithms for specified cities and prints the timing results
run_all :-
    % Define the list of start cities
    Cities = [oradea, neamt, timisoara],
    % Define the list of algorithms to test
    Algorithms = [bfs, dfs, a_star],
    % For each city and each algorithm, time the algorithm
    forall(
        member(City, Cities),
        forall(
            member(Alg, Algorithms),
            time_alg(Alg, City, bucharest)
        )
    ).
