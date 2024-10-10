/* Swap the first two elements if they are not in order*/
swap([X, Y | T], [Y, X | T]):-
    Y =< X.

/* Swap elements in the tail*/
swap([H | T], [H | T1]):-
    swap(T, T1).


/*
Bubble Sort Implementation:
    Repeatedly check adjacent elements and swap them if
    they are in the wrong order until the list is sorted
*/
bubbleSort(L, SL):-
    swap(L, L1), % at least one swap is needed
    !,
    bubbleSort(L1, SL).
bubbleSort(L, L). % here, the list is already sorted


/*
ordered determines if the list is sorted by checking if
every element is <= the following element
*/
ordered([]). % base case empty list
ordered([_X]). % base case single element list

/*
Recursively check if the first two elements are in order
Then check the rest of the list
*/
ordered([H1, H2 | T]):-
    H1 =< H2,
    ordered([H2 | T]).


/*
Insertion Sort Implementation:
    insert(E, SL, SLE): Take 1 element E at a time and insert
    into the appropriate location in the sorted sublist
*/

/* Handle inserting E into a sorted list if E <= the head*/
insert(X, [], [X]). % base case: insert X into an empty list
insert(E, [H | T], [E, H | T]):-
    ordered(T),
    E =< H,
    !.

/*
If E > H, then E is not in H. Therefore, E is recursively
inserted into the tail T and H remains unchanged
*/
insert(E, [H | T], [H | T1]):-
    ordered(T),
    insert(E, T, T1).

/*
Insertion Sort Implementation: 
	COMMENT!!!!!
*/
insertionSort([], []). % base case: empty list is already sorted
insertionSort([H | T], SORTED) :-
    insertionSort(T, T1),
    insert(H, T1, SORTED).


/*
Merge Sort Implementation:
    Split the list into 2 halves, recursively sort each of them, and merge
*/
mergeSort([], []). % The empty list is sorted
mergeSort([X], [X]):-!.
mergeSort(L, SL):-
    split_in_half(L, L1, L2),
    mergeSort(L1, S1),
    mergeSort(L2, S2),
    merge(S1, S2, SL).

/* Split the list L1 into 2 halves L1 and L2 */
intDiv(N, N1, R):- R is div(N, N1). % return integer division of N/N1
split_in_half([], _, _):-!, fail.
split_in_half([X], [], [X]). % split single element list
split_in_half(L, L1, L2):-
    length(L, N),
    intDiv(N, 2, N1), % find midpoint
    length(L1, N1),
    append(L1, L2, L).

/* Merge 2 sorted lists into a single sorted list */
merge([], L, L). % base case: result is second half if the first half is empty
merge(L, [], L). % base case: result is first half if the second half is empty

/*
Place H1 at the beginning of the list if H1 <= H2
Then recursively merge the rest of T1 with the second half [H2 | T2]
*/
merge([H1 | T1], [H2 | T2], [H1 | T]):-
    H1 =< H2,
    merge(T1, [H2 | T2], T).

/*
Place H2 at the beginning of the list if H1 >= H2 otherwise
Then recursively merge the first half [H1 | T1] with the rest of T2
*/
merge([H1|T1], [H2|T2], [H2|T]):-
    H2 =< H1,
    merge([H1 | T1], T2, T).


/*
Quick Sort Implementation:
    Choose a pivot element and split the list into 2 parts
*/
split(_, [], [], []). % base case: split empty list

% Handle case where elements are <= the pivot
split(X, [H | T], [H | SMALL], BIG):-
    H =< X,
    split(X, T, SMALL, BIG).

% Handle cases where elements are >= the pivot
split(X, [H | T], SMALL, [H | BIG]):-
    X =< H,
    split(X, T, SMALL, BIG).

/*
Divide the list into SMALL and BIG parts based on the pivot
Recursively sort each part and recombine them with pivot in between
*/
quickSort([], []). % base case: empty list is already sorted
quickSort([H | T], LS):-
    split(H, T, SMALL, BIG), % split the list based on the pivot
    quickSort(SMALL, S), % recursively sort the SMALL part
    quickSort(BIG, B), % recursively sort the BIG part
    append(S, [H | B], LS). % combine sorted SMALL, pivot, and sorted BIG


/*
Hybrid Sort Implementation:
    Use a small sort algorithm (Bubble Sort, Insertion Sort) for small lists (size < threshold)
    Use a big sort algorithm (Merge Sort, Quick Sort) for large lists (size >= threshold)
*/
% When list is small
hybridSort(LIST, bubbleSort, BIGALG, THRESHOLD, SLIST):-
    length(LIST, N), N =< THRESHOLD,
    bubbleSort(LIST, SLIST).

hybridSort(LIST, insertionSort, BIGALG, THRESHOLD, SLIST):-
    length(LIST, N), N =< THRESHOLD,
    insertionSort(LIST, SLIST).

% When list is large and mergeSort is used
hybridSort(LIST, SMALLALG, mergeSort, THRESHOLD, SLIST):-
    length(LIST, N), N > THRESHOLD,
    split_in_half(LIST, L1, L2),
    hybridSort(L1, SMALLALG, mergeSort, THRESHOLD, S1),
    hybridSort(L2, SMALLALG, mergeSort, THRESHOLD, S2),
    merge(S1, S2, SLIST).

% When list is large and quickSort is used
hybridSort([H | T], SMALLALG, quickSort, THRESHOLD, SLIST):-
    length(LIST, N), N > THRESHOLD,
    split(H, T, L1, L2),
    hybridSort(L1, SMALLALG, quickSort, THRESHOLD, S1),
    hybridSort(L2, SMALLALG, quickSort, THRESHOLD, S2),
    append(S1, [H | S2], SLIST).

hybridSort([H | T], SMALLALG, quickSort, THRESHOLD, SLIST):-
    length([H |T], N), N > THRESHOLD,
    split(H, T, L1, L2),
    hybridSort(L1, SMALLALG, quickSort, THRESHOLD, S1),
    hybridSort(L2, SMALLALG, quickSort, THRESHOLD, S2),
    append(S1, [H | S2], SLIST).


/*------------------------------------------------------------------------------*/
% Create a random list of N elements
randomList(N, LIST):-
    length(LIST, N),
    maplist(random(0, 1000), LIST).

% Create N random lists of different lengths
createRandomLists(N):-
    random(10, 100, SIZE),
    randomList(SIZE, L),
    assertz(random_list(N, L)),
    N1 is N - 1,
    createRandomLists(N1).

create50Lists:-
    createRandomLists(50).

% Time a sorting algorithm
timeAlg(Name, Alg, TIME):-
    statistics(cputime, T0),
    Alg,
    statistics(cputime, T1),
    TIME is T1 - T0,
    format('~w time: ~w seconds~n', [Name, TIME]).  % Print the algorithm name and the time

runAll:-
    randomList(10, SmallList),
    timeAlg("Bubble Sort", bubbleSort(SmallList, SortedBubble), TimeBubble),
    writeln(SortedBubble), nl,
    timeAlg("Insertion Sort", insertionSort(SmallList, SortedInsert), TimeInsertion),
    writeln(SortedInsert), nl,
    randomList(50, LargeList),
    timeAlg("Merge Sort", mergeSort(LargeList, SortedMerge), TimeMerge),
    writeln(SortedMerge), nl,
    timeAlg("Quick Sort", quickSort(LargeList, SortedQuick), TimeQuick),
    writeln(SortedQuick), nl,
    timeAlg("Bubble + Merge Sort", hybridSort(LargeList, bubbleSort, mergeSort, 10, SortedHybridBubbleMerge), TimeHybridBubbleMerge),
    writeln(SortedHybridBubbleMerge), nl,
    timeAlg("Insertion + Merge Sort", hybridSort(LargeList, insertionSort, mergeSort, 10, SortedHybridInsertionMerge), TimeHybridInsertionMerge),
    writeln(SortedHybridInsertionMerge), nl,
    timeAlg("Bubble + Quick Sort", hybridSort(LargeList, bubbleSort, quickSort, 10, SortedHybridBubbleQuick), TimeHybridBubbleQuick),
    writeln(SortedHybridBubbleQuick), nl,
    timeAlg("Insertion + Quick Sort", hybridSort(LargeList, insertionSort, quickSort, 10, SortedHybridInsertionQuick), TimeHybridInsertionQuick),
    writeln(SortedHybridInsertionQuick), nl.
