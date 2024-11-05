% Romanian road map data (distance in km)

road(arad, zerind, 75).
road(arad, timisoara, 118).
road(arad, sibiu, 140).
road(zerind, oradea, 71).
road(zerind, arad, 75).
road(oradea, zerind, 71).
road(oradea, sibiu, 151).
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
road(sibiu, arad, 140).
road(sibiu, oradea, 151).
road(sibiu, fagaras, 99).
road(sibiu, rimnicu_vilcea, 80).
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
road(giurgiu, bucharest, 90).
road(neamt, iasi, 87).
road(iasi, neamt, 87).
road(iasi, vaslui, 92).
road(vaslui, iasi, 92).
road(vaslui, urziceni, 142).
road(urziceni, vaslui, 142).
road(urziceni, hirsova, 98).
road(urziceni, bucharest, 85).
road(hirsova, urziceni, 98).
road(hirsova, eforie, 86).
road(eforie, hirsova, 86).

% Breadth-First Search
bfs(Start, Goal, Path, Cost) :-
    bfs([[Start]], Goal, [], Path, 0, Cost).

bfs([[Goal | PathSoFar] | _], Goal, _, Path, CostSoFar, Cost) :-
    reverse([Goal | PathSoFar], Path).

bfs([[Current | PathSoFar] | Rest], Goal, Visited, Path, CostSoFar, Cost) :-
    findall([Next, Current | PathSoFar],
            (road(Current, Next, _), \+ member(Next, Visited)),
            NextPaths),
    append(Rest, NextPaths, NewQueue),
    bfs(NewQueue, Goal, [Current | Visited], Path, CostSoFar, Cost).

% Depth-First Search
dfs(Start, Goal, Path, Cost) :-
    dfs_helper(Start, Goal, [Start], Path, 0, Cost).

dfs_helper(Goal, Goal, PathSoFar, Path, CostSoFar, Cost) :-
    reverse(PathSoFar, Path).

dfs_helper(Current, Goal, PathSoFar, Path, CostSoFar, Cost) :-
    road(Current, Next, StepCost),
    \+ member(Next, PathSoFar),
    NewCost is CostSoFar + StepCost,
    dfs_helper(Next, Goal, [Next | PathSoFar], Path, NewCost, Cost).

% Straight Line Distance Heuristic
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

a_star(Start, Goal, Path, Cost) :-
    sld(Start, H),
    empty_heap(OpenSet),
    add_to_heap(OpenSet, H, [Start, [Start], 0], InitialQueue),
    a_star_search(InitialQueue, Goal, [], Path, Cost).

a_star_search(Queue, Goal, _, Path, Cost) :-
    get_from_heap(Queue, _, [Goal, PathReversed, Cost], _),
    reverse(PathReversed, Path).

a_star_search(Queue, Goal, Visited, Path, Cost) :-
    get_from_heap(Queue, _, [Current, PathSoFar, CostSoFar], RestQueue),
    findall([F, [Next, [Next | PathSoFar], NewCost]],
            (road(Current, Next, StepCost),
             \+ member(Next, Visited),
             NewCost is CostSoFar + StepCost,
             sld(Next, H),
             F is NewCost + H),
            Children),
    add_children_to_queue(Children, RestQueue, NewQueue),
    a_star_search(NewQueue, Goal, [Current | Visited], Path, Cost).

add_children_to_queue([], Queue, Queue).
add_children_to_queue([[F, Element] | Children], Queue, NewQueue) :-
    add_to_heap(Queue, F, Element, UpdatedQueue),
    add_children_to_queue(Children, UpdatedQueue, NewQueue).