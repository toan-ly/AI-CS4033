from utils.utils import *
import heapq

@timeit(iterations=100)
def best_first_search(start, goal='Bucharest'):
    visited = set()
    pq = [(heuristic(start), start)]  # (h(n), node)
    parent = {start: None}

    while pq:
        _, city = heapq.heappop(pq)
        if city == goal:
            return retrieve_path(parent, start, goal)
        
        visited.add(city)
        for neighbor, _ in get_neighbors(city):
            if neighbor not in visited:
                heapq.heappush(pq, (heuristic(neighbor), neighbor))
                parent[neighbor] = city
                
    return []  # No path found