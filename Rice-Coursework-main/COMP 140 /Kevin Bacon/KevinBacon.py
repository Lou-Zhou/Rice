"""
The Kevin Bacon Game.

Replace "pass" with your code.
"""
import simpleplot
import comp140_module4 as movies
import math
#import user308_0UcVAj8UjH_3 as graphs
class Queue:
    """
    A simple implementation of a FIFO queue.
    """

    def __init__(self):
        """
        Initialize the queue.
        """
        self._queue_values = []

    def __len__(self):
        """
        Returns: an integer representing the number of items in the queue.
        """
        return len(self._queue_values)

    def __str__(self):
        """
        Returns: a string representation of the queue.
        """
        return str(self._queue_values)

    def push(self, item):
        """
        Add item to the queue.

        input:
            - item: any data type that's valid in a list
        """
        self._queue_values.append(item)

    def pop(self):
        """
        Remove the least recently added item.

        Assumes that there is at least one element in the queue.  It
        is an error if there is not.  You do not need to check for
        this condition.

        Returns: the least recently added item.
        """
        return self._queue_values.pop(0)

    def clear(self):
        """
        Remove all items from the queue.
        """
        self._queue_values = []

def bfs(graph, start_node):
    """
    Performs a breadth-first search on graph starting at the
    start_node.

    inputs:
        - graph: a graph object
        - start_node: a node in graph representing the start node

    Returns: a two-element tuple containing a dictionary
    associating each visited node with the order in which it
    was visited and a dictionary associating each visited node
    with its parent node.
    """
    #initialize empty queue and  dist, and parent maps
    queue = Queue()
    dist = {}
    parent = {}
    for node in graph.nodes():
        #set baseline values for dist and parent
        dist[node] = math.inf
        parent[node] = None
    #distance from start_node to start_node is 0
    dist[start_node] = 0
    #push start_node into queue
    queue.push(start_node)
    while len(queue) != 0:
        node = queue.pop()
        #go through neighbors of node's neighbors
        for nbr in graph.get_neighbors(node):
            if dist[nbr] == math.inf:
                #if the distance from start_node is inf, increment one
                #to distance then add to respective dictionaries
                dist[nbr] = dist[node] + 1
                parent[nbr] = node
                #push new neighbor into queue
                queue.push(nbr)
    return dist, parent
#graph = graphs.Graph()
#graph.add_node(0)
#graph.add_node(1)
#graph.add_node(2)
#graph.add_node(3)
#graph.add_node(4)
#print(graph.nodes())

#graph.add_edge(0,1)
#graph.add_edge(0,2)
#graph.add_edge(0,3)
#graph.add_edge(0,4)
#graph.add_edge(1,2)
#graph.add_edge(1,3)
#graph.add_edge(1,4)
#graph.add_edge(2,3)
#graph.add_edge(2,4)
#graph.add_edge(3,4)

def distance_histogram(graph, node):
    """
    Computes the distance between the given node and all other
    nodes in that graph and creates a histogram of those distances.

    inputs:
        - graph: a graph object
        - node: a node in graph

    returns: a dictionary mapping each distance with the number of
    nodes that are that distance from node.
    """
    distances = {}
    neighbor_dist = bfs(graph, node)[0]
    #get dictionary of distances from bfs
    print(neighbor_dist)
    for keys in neighbor_dist:
        #loops through all keys, if not in dictionary, set value to 1
        #if in dictionary, increment by 1
        if neighbor_dist[keys] not in distances:
            distances[neighbor_dist[keys]] = 1
        else:
            distances[neighbor_dist[keys]] = distances[neighbor_dist[keys]] + 1
    return distances
print(distance_histogram(movies.load_test_graph('line'), 'A'))
def find_path(graph, start_person, end_person, parents):
    """
    Computes the path from start_person to end_person in the graph.

    inputs:
        - graph: a graph oject with edges representing the connections between people
        - start_person: a node in graph representing the starting node
        - end_person: a node in graph representing the ending node
        - parents: a dictionary representing the parents in the graph

    returns a list of tuples of the path in the form:
        [(actor1, {movie1a, ...}), (actor2, {movie2a, ...}), ...]
    """
    
    path = []
    dead_end = []
    #two empty lists -- path to store path, and dead_end to check
    #if path goes to a dead end(not to ending node)
    person = start_person
    if start_person == end_person:
        path.append((start_person, set([])))
        #if start_person is end_person -- done
    while person != end_person:
        #print(person)
        #print(path)
        if person not in parents.values() and person not in dead_end:
            #if person is a dead end, add to dead end and reset path
            dead_end.append(person)
            #print(dead_end)
            person = start_person
            path = []
        all_matches = []
        #finds all potential paths, if no potential paths,
        #then no valid path
        for key in parents.keys():
            if parents[key] == person and key not in dead_end:
                all_matches.append(key)
                print(all_matches)
                #finds all potential paths
        for key in parents.keys():
            if parents[key] == person and key not in dead_end:
                path.append((person, graph.get_attrs(key, person)))
                #path.append((person, key))
                person = key
                #takes a potential path and follows it
                break
        
        if person == end_person:
            path.append((end_person, set([])))
            #if reached end, add ending value
            return path
        if len(all_matches) == 0:
            #if no potential paths, no path is possible, return
            #empty list
            print(dead_end)
            return []
        
    print("output")
    return path

#print(bfs(graph, 0)[0])
#print(find_path(graph, 0, 4, bfs(graph, 0)[1]))
def play_kevin_bacon_game(graph, start_person, end_people):
    """
    Play the "Kevin Bacon Game" on the actors in the given
    graph.

    inputs:
        - graph: a a graph oject with edges representing the connections between people
        - start_person: a node in graph representing the node from which the search will start
        - end_people: a list of nodes in graph to which the search will be performed

    Prints the results out.
    """
    #run find_path, using the bfs function for the parents parameter
    path =  find_path(graph, start_person, end_people, bfs(graph, start_person)[1])
    movies.print_path(path)

def run():
    """
    Load a graph and play the Kevin Bacon Game.
    """
    graph5000 = movies.load_graph('subgraph5000')

    if len(graph5000.nodes()) > 0:
        # You can/should use smaller graphs and other actors while
        # developing and testing your code.
        play_kevin_bacon_game(graph5000, 'Kevin Bacon',
            ['Amy Adams', 'Andrew Garfield', 'Anne Hathaway', 'Barack Obama', \
             'Benedict Cumberbatch', 'Chris Pine', 'Daniel Radcliffe', \
             'Jennifer Aniston', 'Joseph Gordon-Levitt', 'Morgan Freeman', \
             'Sandra Bullock', 'Tina Fey'])

        # Plot distance histograms
        for person in ['Kevin Bacon', 'Stephanie Fratus']:
            hist = distance_histogram(graph5000, person)
            simpleplot.plot_bars(person, 400, 300, 'Distance', \
                'Frequency', [hist], ["distance frequency"])

# Uncomment the call to run below when you have completed your code.

run()
