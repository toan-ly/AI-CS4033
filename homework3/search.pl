% Romanian road map data (distance in km)

% road(arad, zerind, 75).
% road(arad, sibiu, 140).
% road(arad, timisoara, 118).
% road(zerind, arad, 75).
% road(zerind, oradea, 71).
% road(oradea, zerind, 71).
% road(oradea, sibiu, 151).
% road(timisoara, arad, 118).
% road(timisoara, lugoj, 111).
% road(lugoj, timisoara, 111).
% road(lugoj, mehadia, 70).
% road(mehadia, lugoj, 70).
% road(mehadia, drobeta, 75).
% road(drobeta, mehadia, 75).
% road(drobeta, craiova, 120).
% road(craiova, drobeta, 120).
% road(craiova, rimnicu_vilcea, 146).
% road(craiova, pitesti, 138).
% road(sibiu, arad, 140).
% road(sibiu, oradea, 151).
% road(sibiu, fagaras, 99).
% road(sibiu, rimnicu_vilcea, 80).
% road(rimnicu_vilcea, sibiu, 80).
% road(rimnicu_vilcea, craiova, 146).
% road(rimnicu_vilcea, pitesti, 97).
% road(fagaras, sibiu, 99).
% road(fagaras, bucharest, 211).
% road(pitesti, rimnicu_vilcea, 97).
% road(pitesti, craiova, 138).
% road(pitesti, bucharest, 101).
% road(bucharest, fagaras, 211).
% road(bucharest, pitesti, 101).
% road(bucharest, giurgiu, 90).
% road(giurgiu, bucharest, 90).
% road(neamt, iasi, 87).
% road(iasi, neamt, 87).
% road(iasi, vaslui, 92).
% road(vaslui, iasi, 92).
% road(vaslui, urziceni, 142).
% road(urziceni, vaslui, 142).
% road(urziceni, hirsova, 98).
% road(urziceni, bucharest, 85).
% road(hirsova, urziceni, 98).
% road(hirsova, eforie, 86).
% road(eforie, hirsova, 86).
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


% Breadth-First Search
% bfs(Start, Goal, Path, Cost, VisitedCount) :-
%     bfs([[Start]], Goal, [], Path, 0, Cost, 0, VisitedCount).

% bfs([[Goal | PathSoFar] | _], Goal, PathSoFar, Path, CostSoFar, Cost, Visited, VisitedCount) :-
%     reverse([Goal | PathSoFar], Path),
%     Cost = CostSoFar,
%     VisitedCount is Visited + 1.

% bfs([[Current | PathSoFar] | Rest], Goal, VisitedList, Path, CostSoFar, Cost, Visited, VisitedCount) :-
%     findall([Next, Current | PathSoFar],
%             (road(Current, Next, StepCost), \+ member(Next, VisitedList)),
%             NextPaths),
%     append(Rest, NextPaths, NewQueue),
%     NewVisited is Visited + 1,
%     bfs(NewQueue, Goal, [Current | VisitedList], Path, CostSoFar, Cost, NewVisited, VisitedCount).

bfs(Start, Goal, Path, Cost, VisitedCount) :-
    bfs([[Start, [Start], 0]], Goal, Path, Cost, 0, VisitedCount).

bfs([[Goal, PathSoFar, CostSoFar] | _], Goal, Path, Cost, Visited, VisitedCount) :-
    reverse(PathSoFar, Path),
    Cost = CostSoFar,
    VisitedCount is Visited + 1.

bfs([[Current, PathSoFar, CostSoFar] | Rest], Goal, Path, Cost, Visited, VisitedCount) :-
    findall([Next, [Next | PathSoFar], NewCost],
            (road(Current, Next, StepCost), \+ member(Next, PathSoFar), NewCost is CostSoFar + StepCost),
            NextPaths),
    append(Rest, NextPaths, NewQueue),
    bfs(NewQueue, Goal, Path, Cost, Visited + 1, VisitedCount).


% Depth-First Search
dfs(Start, Goal, Path, Cost, VisitedCount) :-
    dfs_helper(Start, Goal, [Start], Path, 0, Cost, 0, VisitedCount).

dfs_helper(Goal, Goal, PathSoFar, Path, CostSoFar, Cost, Visited, VisitedCount) :-
    reverse(PathSoFar, Path),
    Cost = CostSoFar,
    VisitedCount is Visited + 1.

dfs_helper(Current, Goal, PathSoFar, Path, CostSoFar, Cost, Visited, VisitedCount) :-
    road(Current, Next, StepCost),
    \+ member(Next, PathSoFar),
    NewCost is CostSoFar + StepCost,
    dfs_helper(Next, Goal, [Next | PathSoFar], Path, NewCost, Cost, Visited + 1, VisitedCount).

% Straight Line Distance Heuristic (SLD)
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

% A* Search
:- use_module(library(heaps)).

a_star(Start, Goal, Path, Cost, VisitedCount) :-
    sld(Start, H),
    empty_heap(OpenSet),
    add_to_heap(OpenSet, H, [Start, [Start], 0], InitialQueue),
    a_star_search(InitialQueue, Goal, [], Path, Cost, 0, VisitedCount).

a_star_search(Queue, Goal, _, Path, Cost, Visited, VisitedCount) :-
    get_from_heap(Queue, _, [Goal, PathReversed, Cost], _),
    reverse(PathReversed, Path),
    VisitedCount is Visited + 1.

a_star_search(Queue, Goal, VisitedList, Path, Cost, Visited, VisitedCount) :-
    get_from_heap(Queue, _, [Current, PathSoFar, CostSoFar], RestQueue),
    findall([F, [Next, [Next | PathSoFar], NewCost]],
            (road(Current, Next, StepCost),
             \+ member(Next, VisitedList),
             NewCost is CostSoFar + StepCost,
             sld(Next, H),
             F is NewCost + H),
            Children),
    add_children_to_queue(Children, RestQueue, NewQueue),
    NewVisited is Visited + 1,
    a_star_search(NewQueue, Goal, [Current | VisitedList], Path, Cost, NewVisited, VisitedCount).

add_children_to_queue([], Queue, Queue).
add_children_to_queue([[F, Element] | Children], Queue, NewQueue) :-
    add_to_heap(Queue, F, Element, UpdatedQueue),
    add_children_to_queue(Children, UpdatedQueue, NewQueue).


/*------------------------------------------------------------------------------*/
% Time each search algorithm
time_alg(Alg, Start, Goal):-
    % Start timing
    statistics(cputime, T0),

    % Call the algorithm
    call_alg(Alg, Start, Goal, Path, Cost, VisitedCount),

    % Stop timing
    statistics(cputime, T1),
    Time is (T1 - T0) * 1000,

    format("~nSearch Algorithm: ~w~n", [Alg]),
    format("Path: ~w~n", [Path]),
    format("Cost: ~w~n", [Cost]),
    format("Cities Visited: ~w~n", [VisitedCount]),
    format("Time: ~3f ms~n", [Time]).

call_alg(bfs, Start, Goal, Path, Cost, VisitedCount) :-
    bfs(Start, Goal, Path, Cost, VisitedCount).
call_alg(dfs, Start, Goal, Path, Cost, VisitedCount) :-
    dfs(Start, Goal, Path, Cost, VisitedCount).
call_alg(a_star, Start, Goal, Path, Cost, VisitedCount) :-
    a_star(Start, Goal, Path, Cost, VisitedCount).

% Test all algorithms
run_all :-
    Cities = [sibiu, zerind, neamt, timisoara],
    Algorithms = [bfs, dfs, a_star],
    forall(member(City, Cities),
        forall(member(Alg, Algorithms),
            time_alg(Alg, City, bucharest))).
