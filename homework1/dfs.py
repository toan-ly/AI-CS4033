from utils.utils import *

@timeit(iterations=1000)
def depth_first_search(start, goal='Bucharest'):
    """
    Implements the Depth-First Search (DFS) algorithm to find a path from the start city to the goal city.

    Parameters:
    - start (str): The starting city.
    - goal (str): The goal city. Defaults to 'Bucharest'.

    Returns:
    - tuple: A tuple containing the path as a list of cities and the number of nodes visited.
             If no path is found, returns an empty list and the number of nodes visited.
    """
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
