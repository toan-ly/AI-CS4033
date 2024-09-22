from bfs import breadth_first_search
from dfs import depth_first_search
from greedy import best_first_search
from a_star import a_star
from utils.utils import romania_map

def test_completeness(algorithm, algorithm_name, goal='Bucharest'):
    cities = romania_map.keys()
    failed_cities = []
    
    print(f'Testing Completeness for {algorithm_name}')
    for city in cities:
        print(f'From {city} to {goal}...')
        path, _ = algorithm(city, goal)
        
        if not path:
            print(f'Failed to find a path from {city} to {goal}')
            failed_cities.append(city)
    
    if failed_cities:
        print(f'\nThe following cities failed to reach {goal}:')
        print(city for city in failed_cities)
    else:
        print('=> All successfully reached the goal!!!\n')

def test(start_city, goal_city='Bucharest'):
    print(f'From {start_city} to {goal_city}')
    print('DFS:')
    dfs_path, dfs_cost, visited = depth_first_search(start_city, goal_city)
    print(f'Path: {dfs_path}')
    print(f'Cost: {dfs_cost}')
    print(f'Node Visited: {visited}')
    
    print('\nBFS:')
    bfs_path, bfs_cost, visited = breadth_first_search(start_city, goal_city)
    print(f'Path: {bfs_path}')
    print(f'Cost: {bfs_cost}')
    print(f'Node Visited: {visited}')

    print('\nBest-First Search:')
    greedy_path, greedy_cost, visited = best_first_search(start_city, goal_city)
    print(f'Path: {greedy_path}')
    print(f'Cost: {greedy_cost}')
    print(f'Node Visited: {visited}')

    print('\nA* Search:')
    a_star_path, a_star_cost, visited = a_star(start_city, goal_city)
    print(f'Path: {a_star_path}')
    print(f'Cost: {a_star_cost}')
    print(f'Node Visited: {visited}')
    
    print(f'\n{"-"*50}')

def main():
    test_completeness(depth_first_search.__wrapped__, 'DFS')
    test_completeness(breadth_first_search.__wrapped__, 'BFS') 
    test_completeness(best_first_search.__wrapped__, 'Best-First Search')
    test_completeness(a_star.__wrapped__, 'A* Search')
    
    test('Sibiu')
    test('Zerind')
    test('Neamt')
    test('Timisoara')


if __name__ == '__main__':
    main()