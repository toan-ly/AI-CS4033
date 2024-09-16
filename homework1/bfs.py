from utils.utils import *
from collections import deque

@timeit(iterations=100)
def breadth_first_search(start, goal='Bucharest'):
    visited = set()
    queue = deque([start])
    parent = {start: None}

    while queue:
        city = queue.popleft()
        if city == goal:
            return retrieve_path(parent, start, goal)
        visited.add(city)
        
        for neighbor, _ in get_neighbors(city):
            if neighbor not in visited and neighbor not in queue:
                parent[neighbor] = city
                queue.append(neighbor)
                
    return [] # No path found
    
