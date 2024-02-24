"""
Map Search
"""
import math
import comp140_module7 as maps
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


class Stack:
    """
    A simple implementation of a FILO stack.
    """

    def __init__(self):
        """
        Initialize the stack.
        """
        self._stack_values = []

    def __len__(self):
        """
        Returns: an integer representing the number of items in the stack.
        """
        return len(self._stack_values)

    def __str__(self):
        """
        Returns: a string representation of the stack.
        """
        return str(self._stack_values)

    def push(self, item):
        """
        Add item to the stack.

        input:
            - item: any data type that's valid in a list
        """
        self._stack_values.append(item)

    def pop(self):
        """
        Remove the most recently added item.

        Returns: the most recently added item.
        """
        return self._stack_values.pop(-1)

    def clear(self):
        """
        Remove all items from the stack.
        """
        self._stack_values = []


def bfs_dfs(graph, rac_class, start_node, end_node):
    """
    Performs a breadth-first search or a depth-first search on graph
    starting at the start_node.  The rac_class should either be a
    Queue class or a Stack class to select BFS or DFS.

    Completes when end_node is found or entire graph has been
    searched.

    inputs:
        - graph: a directed Graph object representing a street map
        - rac_class: a restricted access container (Queue or Stack) class to
          use for the search
        - start_node: a node in graph representing the start
        - end_node: a node in graph representing the end

    Returns: a dictionary associating each visited node with its parent
    node.
    """
    #initialize empty queue or stack and dist, and parent maps
    if rac_class == Queue:
        rac = Queue()
    else:
        rac = Stack()
    dist = {}
    parent = {}
    for node in graph.nodes():
        #set baseline values for dist and parent
        dist[node] = math.inf
        parent[node] = None
    #distance from start_node to start_node is 0
    dist[start_node] = 0
    #push start_node into queue / stack
    rac.push(start_node)
    while len(rac) != 0:
        node = rac.pop()
        #go through neighbors of node's neighbors
        for nbr in graph.get_neighbors(node):
            if dist[nbr] == math.inf:
                #if the distance from start_node is inf, increment one
                #to distance then add to respective dictionaries
                dist[nbr] = dist[node] + 1
                parent[nbr] = node
                if nbr == end_node:
                    return parent
                #push new neighbor into queue
                rac.push(nbr)
    return parent

def dfs(graph, start_node, end_node, parent):
    """
    Performs a recursive depth-first search on graph starting at the
    start_node.

    Completes when end_node is found or entire graph has been
    searched.

    inputs:
        - graph: a directed Graph object representing a street map
        - start_node: a node in graph representing the start
        - end_node: a node in graph representing the end
        - parent: a dictionary that initially has one entry associating
                  the original start_node with None

    Modifies the input parent dictionary to associate each visited node
    with its parent node
    """
    
    #base case, when the start_node = end_node, the value is found
    if start_node == end_node:
        return parent
    #recursive case:
    for node in graph.get_neighbors(start_node):
        #goes through each neighbor of the node
        if node not in parent:
            #if node has not been seen yet, set value in parent
            parent[node] = start_node
            #run dfs from node to end_node, making the graph smaller
            dfs(graph, node, end_node, parent)
    return parent #exit condition incase end_node isn't found


        
        

def astar(graph, start_node, end_node,
          edge_distance, straight_line_distance):
    """
    Performs an A* search on graph starting at start_node.

    Completes when end_node is found or entire graph has been
    searched.

    inputs:
        - graph: a directed Graph object representing a street map
        - start_node: a node in graph representing the start
        - end_node: a node in graph representing the end
        - edge_distance: a function which takes two nodes and a graph
                         and returns the actual distance between two
                         neighboring nodes
        - straight_line_distance: a function which takes two nodes and
                         a graph and returns the straight line distance 
                         between two nodes

    Returns: a dictionary associating each visited node with its parent
    node.
    """
    #initialize maps and lists
    closedset = []
    openset = [start_node]
    fcosts = {start_node: straight_line_distance(start_node, end_node, graph)}
    gcosts = {start_node: 0}
    parent = {start_node:None}
    while len(openset) > 0:
        print("New Iteration")
        fcost = math.inf
        node = openset[0]
        #loop which gets lowest fcost from openset
        for open_node in openset:
            if open_node not in fcosts:
                fcosts[open_node] = math.inf
            if fcosts[open_node] < fcost:
                fcost = fcosts[open_node]
                node = open_node
        openset.remove(node)
        closedset.append(node)
        #pop from openset, add to closedset
        if node == end_node:
            #if the node is end_node, found end_node, return parent
            return parent
        for nbr in graph.get_neighbors(node):
            #loop through neighbors, calculating g,h, and fcosts
            nbr_gcost = gcosts[node] + edge_distance(node, nbr, graph)
            nbr_hcost = straight_line_distance(nbr, end_node, graph)
            nbr_fcost = nbr_gcost + nbr_hcost
            if nbr not in gcosts:
                #if we don't have a current gcost for nbr, set to inf
                gcosts[nbr] = math.inf
            if nbr_gcost < gcosts[nbr] and nbr not in closedset:
                #if the new gcost is less than the old gcost and nbr isn't in closedset
                #update gcosts, fcosts, and parent
                fcosts[nbr] = nbr_fcost
                gcosts[nbr] = nbr_gcost
                parent[nbr] = node
            if nbr not in closedset and nbr not in openset:
                #if nbr is not in either open or closedset, add nbr in openset
                openset.append(nbr)
    return parent #return in case node isn't found
#print(maps.load_test_graph('clique'))

# You can replace functions/classes you have not yet implemented with
# None in the call to "maps.start" below and the other elements will
# work.

maps.start(bfs_dfs, Queue, Stack, dfs, astar)
