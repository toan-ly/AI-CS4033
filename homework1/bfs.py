from utils.utils import *
from collections import deque

@timeit(iterations=1000)
def breadth_first_search(start, goal='Bucharest'):
    """
    Implements the Breadth-First Search (BFS) algorithm to find the shortest path from the start city to the goal city.

    Parameters:
    - start (str): The starting city.
    - goal (str): The goal city. Defaults to 'Bucharest'.

    Returns:
    - tuple: A tuple containing the path as a list of cities and the number of nodes visited.
             If no path is found, returns an empty list and the number of nodes visited.
    """
    visited = set()
    queue = deque([start])
    parent = {start: None}
    nodes_visited = 0

    while queue:
        city = queue.popleft()
        nodes_visited += 1 # Count nodes visited
        if city == goal:
            return retrieve_path(parent, start, goal), nodes_visited
        visited.add(city)
        
        for neighbor, _ in get_neighbors(city):
            if neighbor not in visited and neighbor not in queue:
                parent[neighbor] = city
                queue.append(neighbor)
                
    return [], nodes_visited # No path found
