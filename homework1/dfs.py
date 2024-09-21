from utils.utils import *

@timeit(iterations=100)
def depth_first_search(start, goal='Bucharest'):
    visited = set()
    stack = [start]
    parent = {start: None}
    nodes_visited = 0
    while stack:
        city = stack.pop()
        nodes_visited += 1
        if city == goal:
            return retrieve_path(parent, start, goal), nodes_visited
        visited.add(city)

        for neighbor, _ in reversed(get_neighbors(city)):
            if neighbor not in visited: # if neighbor not in stack:
                parent[neighbor] = city
                stack.append(neighbor)

    return [], nodes_visited # Return empty path when no path is found