from utils.utils import *
import heapq

@timeit(iterations=1000)
def a_star(start, goal='Bucharest'):
    visited = set()
    pq = [(0 + heuristic(start), 0, start)]  # priority queue (f(n), g(n), node)
    parent = {start: None}
    g_scores = {start: 0}  # Distance from start
    nodes_visited = 0
    while pq: # while not empty
        f, g, city = heapq.heappop(pq)
        nodes_visited += 1
        if city == goal:
            return retrieve_path(parent, start, goal), nodes_visited
        
        visited.add(city)
        for neighbor, cost in get_neighbors(city):
            temp_g_score = g + cost
            if neighbor not in visited or temp_g_score < g_scores.get(neighbor, float('inf')):
                g_scores[neighbor] = temp_g_score
                f_score = temp_g_score + heuristic(neighbor)
                heapq.heappush(pq, (f_score, temp_g_score, neighbor))
                parent[neighbor] = city
                
    return [], nodes_visited  # Return empty path if no path found