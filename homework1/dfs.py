from utils.utils import *

@timeit(iterations=100)
def depth_first_search(start, goal='Bucharest'):
    visited = set()
    stack = [start]
    parent = {start: None}

    while stack:
        city = stack.pop()
        if city == goal:
            return retrieve_path(parent, start, goal)
        visited.add(city)

        for neighbor, _ in reversed(get_neighbors(city)):
            if neighbor not in visited: # and neighbor not in stack:
                parent[neighbor] = city
                stack.append(neighbor)

    return [] # No path found
        