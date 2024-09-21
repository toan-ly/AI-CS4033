from bfs import breadth_first_search
from dfs import depth_first_search
from greedy import best_first_search
from a_star import a_star

def test(start_city, goal_city='Bucharest'):
    print(f'From {start_city} to {goal_city}')
    print('DFS:')
    dfs_path, dfs_cost = depth_first_search(start_city, goal_city)
    print(f'Path: {dfs_path}')
    print(f'Cost: {dfs_cost}')
    
    print('\nBFS:')
    bfs_path, bfs_cost = breadth_first_search(start_city, goal_city)
    print(f'Path: {bfs_path}')
    print(f'Cost: {bfs_cost}')
    
    print('\nBest-First Search:')
    greedy_path, greedy_cost = best_first_search(start_city, goal_city)
    print(f'Path: {greedy_path}')
    print(f'Cost: {greedy_cost}')

    print('\nA* Search:')
    a_star_path, a_star_cost = a_star(start_city, goal_city)
    print(f'Path: {a_star_path}')
    print(f'Cost: {a_star_cost}')
    
    print(f'\n{"-"*50}')

def main():
    test('Sibiu')
    test('Zerind')
    test('Neamt')
    test('Timisoara')

if __name__ == '__main__':
    main()