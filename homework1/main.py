
from bfs import breadth_first_search
from dfs import depth_first_search
from greedy import best_first_search
from a_star import a_star

def test(start_city, goal_city='Bucharest'):
    print(f'From {start_city} to {goal_city}')
    print('DFS:')
    dfs_path = depth_first_search(start_city, goal_city)
    
    print('\nBFS:')
    bfs_path = breadth_first_search(start_city, goal_city)
    
    print('\nBest-First Search:')
    greedy_path = best_first_search(start_city, goal_city)

    print('\nA* Search:')
    a_star_path = a_star(start_city, goal_city)
    
    print(f'\n{"-"*50}')

def main():
    test('Sibiu')
    test('Zerind')
    test('Neamt')
    test('Timisoara')
 




if __name__ == '__main__':
    main()