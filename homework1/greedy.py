from utils.utils import *
import heapq

@timeit(iterations=1000)
def best_first_search(start, goal='Bucharest'):
    visited = set()
    pq = [(heuristic(start), start)]  # (h(n), node)
    parent = {start: None}
    nodes_visited = 0
    while pq:
        _, city = heapq.heappop(pq)
        nodes_visited += 1
        if city == goal:
            return retrieve_path(parent, start, goal), nodes_visited
        
        visited.add(city)
        for neighbor, _ in get_neighbors(city):
            if neighbor not in visited:
                heapq.heappush(pq, (heuristic(neighbor), neighbor))
                parent[neighbor] = city
                
    return [], nodes_visited  # No path found