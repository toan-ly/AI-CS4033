from bfs import breadth_first_search
from dfs import depth_first_search
from greedy import best_first_search
from a_star import a_star
from utils.utils import romania_map
import time
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

def test_performance(algorithm, algorithm_name, goal='Bucharest'):
    cities = romania_map.keys()
    failed_cities = []
    total_time = 0
    total_nodes_visited = 0
    total_cost = 0
    
    print(f'Testing Performance for {algorithm_name}')
    for city in cities:
        print(f'From {city} to {goal}...')

        start_time = time.time()
        for _ in range(1000):
            (path, cost), nodes_visited = algorithm(city, goal)
        end_time = time.time()

        elapsed_time = end_time - start_time
        
        if not path:
            print(f'Failed to find a path from {city} to {goal}')
            failed_cities.append(city)
        else:
            total_time += elapsed_time
            total_nodes_visited += nodes_visited
            total_cost += cost
    
    if failed_cities:
        print(f'\nThe following cities failed to reach {goal}:')
        print(city for city in failed_cities)
    else:
        print('=> All successfully reached the goal!!!\n')
    
    avg_time = total_time / len(cities)
    avg_nodes_visited = total_nodes_visited / len(cities)
    avg_cost = total_cost / len(cities)
    return avg_time, avg_nodes_visited, avg_cost

def store_performance():
    algorithms = {
        'DFS': depth_first_search.__wrapped__,
        'BFS': breadth_first_search.__wrapped__,
        'Best-First Search': best_first_search.__wrapped__,
        'A*': a_star.__wrapped__
    }

    avg_times = []
    avg_nodes_visited = []
    avg_costs = []

    for algorithm_name, algorithm in algorithms.items():
        time, nodes_visited, cost = test_performance(algorithm, algorithm_name)
        avg_times.append(time)
        avg_nodes_visited.append(nodes_visited)
        avg_costs.append(cost)
    
    df = pd.DataFrame({
        'Algorithm': list(algorithms.keys()),
        'Average Time': avg_times,
        'Average Nodes Visited': avg_nodes_visited,
        'Average Cost': avg_costs
    })
    
    return df

def plot_performance(data):
    # Plot Average Time
    plt.figure(figsize=(10, 6))
    sns.barplot(x='Algorithm', 
                y='Average Time',
                data=data,
                palette='tab10')
    plt.title('Average Time to Bucharest', fontsize=18)
    plt.ylabel('Time (seconds)', fontsize=14)
    plt.xlabel('Search Algorithm', fontsize=14)
    plt.tight_layout()
    plt.show()

    # Plot Average Cities Visited
    plt.figure(figsize=(10, 6))
    sns.barplot(x='Algorithm',
                y='Average Nodes Visited',
                data=data,
                palette='tab10')
    plt.title('Average Cities Visited to Reach Bucharest', fontsize=18)
    plt.xlabel('Search Algorithm', fontsize=14)
    plt.ylabel('Number of Cities Visited', fontsize=14)
    plt.tight_layout()
    plt.show()

    # Plot Average Cost
    plt.figure(figsize=(10, 6))
    sns.barplot(x='Algorithm',
                y='Average Cost',
                data=data,
                palette='tab10')
    plt.title('Average Cost to Reach Bucharest', fontsize=18)
    plt.xlabel('Search Algorithm', fontsize=14)
    plt.ylabel('Cost', fontsize=14)
    plt.tight_layout()
    plt.show()
    

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
    test_performance(depth_first_search.__wrapped__, 'DFS')
    test_performance(breadth_first_search.__wrapped__, 'BFS') 
    test_performance(best_first_search.__wrapped__, 'Best-First Search')
    test_performance(a_star.__wrapped__, 'A* Search')
    
    test('Sibiu')
    test('Zerind')
    test('Neamt')
    test('Timisoara')
    
    performance_data = store_performance()
    plot_performance(performance_data)


if __name__ == '__main__':
    main()