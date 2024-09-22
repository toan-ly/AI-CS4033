import time

# Romania road map as adjacency list
romania_map = {
    'Arad': [('Zerind', 75), ('Sibiu', 140), ('Timisoara', 118)],
    'Zerind': [('Arad', 75), ('Oradea', 71)],
    'Oradea': [('Zerind', 71), ('Sibiu', 151)],
    'Sibiu': [('Arad', 140), ('Oradea', 151), ('Fagaras', 99), ('Rimnicu Vilcea', 80)],
    'Timisoara': [('Arad', 118), ('Lugoj', 111)],
    'Lugoj': [('Timisoara', 111), ('Mehadia', 70)],
    'Mehadia': [('Lugoj', 70), ('Drobeta', 75)],
    'Drobeta': [('Mehadia', 75), ('Craiova', 120)],
    'Craiova': [('Drobeta', 120), ('Rimnicu Vilcea', 146), ('Pitesti', 138)],
    'Rimnicu Vilcea': [('Sibiu', 80), ('Craiova', 146), ('Pitesti', 97)],
    'Fagaras': [('Sibiu', 99), ('Bucharest', 211)],
    'Pitesti': [('Rimnicu Vilcea', 97), ('Craiova', 138), ('Bucharest', 101)],
    'Bucharest': [('Fagaras', 211), ('Pitesti', 101), ('Giurgiu', 90), ('Urziceni', 85)],
    'Giurgiu': [('Bucharest', 90)],
    'Urziceni': [('Bucharest', 85), ('Hirsova', 98), ('Vaslui', 142)],
    'Hirsova': [('Urziceni', 98), ('Eforie', 86)],
    'Eforie': [('Hirsova', 86)],
    'Vaslui': [('Urziceni', 142), ('Iasi', 92)],
    'Iasi': [('Vaslui', 92), ('Neamt', 87)],
    'Neamt': [('Iasi', 87)]
}

# Heuristic function: straight-line distance to Bucharest h(n)
straight_line_dist = {
    'Arad': 366,
    'Bucharest': 0,
    'Craiova': 160,
    'Drobeta': 242,
    'Eforie': 161,
    'Fagaras': 176,
    'Giurgiu': 77,
    'Hirsova': 151,
    'Iasi': 226,
    'Lugoj': 244,
    'Mehadia': 241,
    'Neamt': 234,
    'Oradea': 380,
    'Pitesti': 100,
    'Rimnicu Vilcea': 193,
    'Sibiu': 253,
    'Timisoara': 329,
    'Urziceni': 80,
    'Vaslui': 199,
    'Zerind': 374
}

def retrieve_path(parent, start, goal):
    """
    Reconstruct the path from the start city to the goal city after the search is done
    Arguments include:
        parent (dict): A dictionary mapping each city to its previous city in the path.
        start (str): Starting city.
        goal (str): Goal city.
    Returns:
        list: A list of cities representing the reconstructed path from the start city to the goal city.
    """
    path = []
    total_cost = 0
    current = goal
    while current != start:
        path.append(current)
        prev = parent[current]
        for neighbor, cost in get_neighbors(prev):
            if neighbor == current:
                total_cost += cost
                break
        current = prev

    path.append(start)
    return path[::-1], total_cost

def get_neighbors(city):
    return romania_map[city]

def heuristic(city):
    return straight_line_dist[city]

def timeit(iterations=1000):
    def decorator(algorithm):
        def wrapper(*args, **kwargs):
            start_time = time.time()
            for _ in range(iterations):
                (path, cost), visited = algorithm(*args, **kwargs)
            end_time = time.time()
            print(f'{algorithm.__name__} took {(end_time - start_time):.6f} seconds')
            return path, cost, visited
        # Bypass timeit if needed
        wrapper.__wrapped__ = algorithm
        return wrapper
    return decorator
