# Lou Zhou
# lz80
# COMP 182 Spring 2021 - Homework 4, Problem 3

# You can import any standard library, as well as Numpy and Matplotlib.
# You can use helper functions from comp182.py, provided.py, and autograder.py,
# but they have to be copied over here.
import matplotlib.pyplot as plt
import pylab
import types
import time
import math
import copy
import numpy
import random
# Your code here...
def show():
    """
    Do not use this function unless you have trouble with figures.

    It may be necessary to call this function after drawing/plotting
    all figures.  If so, it should only be called once at the end.

    Arguments:
    None

    Returns:
    None
    """
    plt.show()
def read_graph(filename):

    """
    Read a graph from a file.  The file is assumed to hold a graph
    that was written via the write_graph function.

    Arguments:
    filename -- name of file that contains the graph

    Returns:
    The graph that was stored in the input file.
    """
    with open(filename) as f:
        g = eval(f.read())
    return g
def copy_graph(g):
    """
    Return a copy of the input graph, g

    Arguments:
    g -- a graph

    Returns:
    A copy of the input graph that does not share any objects.
    """
    return copy.deepcopy(g)
def upa(n, m):
    """
    Generate an undirected graph with n node and m edges per node
    using the preferential attachment algorithm.

    Arguments:
    n -- number of nodes
    m -- number of edges per node

    Returns:
    undirected random graph in UPAG(n, m)
    """
    g = {}
    if m <= n:
        g = make_complete_graph(m)
        for new_node in range(m, n):
            # Find <=m nodes to attach to new_node
            totdeg = float(total_degree(g))
            nodes = list(g.keys())
            probs = []
            for node in nodes:
                probs.append(len(g[node]) / totdeg)
            mult = distinct_multinomial(m, probs)

            # Add new_node and its random neighbors
            g[new_node] = set()
            for idx in mult:
                node = nodes[idx]
                g[new_node].add(node)
                g[node].add(new_node)
    return g            
def erdos_renyi(n, p):
    """
    Generate a random Erdos-Renyi graph with n nodes and edge probability p.

    Arguments:
    n -- number of nodes
    p -- probability of an edge between any pair of nodes

    Returns:
    undirected random graph in G(n, p)
    """
    g = {}

    ### Add n nodes to the graph
    for node in range(n):
        g[node] = set()

    ### Iterate through each possible edge and add it with 
    ### probability p.
    for u in range(n):
        for v in range(u+1, n):
            r = random.random()
            if r < p:
                g[u].add(v)
                g[v].add(u)

    return g
def total_degree(g):
    """
    Compute total degree of the undirected graph g.

    Arguments:
    g -- undirected graph

    Returns:
    Total degree of all nodes in g
    """
    return sum(map(len, g.values()))
def make_complete_graph(num_nodes):
    """
    Returns a complete graph containing num_nodes nodes.
 
    The nodes of the returned graph will be 0...(num_nodes-1) if num_nodes-1 is positive.
    An empty graph will be returned in all other cases.
 
    Arguments:
    num_nodes -- The number of nodes in the returned graph.
 
    Returns:
    A complete graph in dictionary form.
    """
    result = {}
         
    for node_key in range(num_nodes):
        result[node_key] = set()
        for node_value in range(num_nodes):
            if node_key != node_value: 
                result[node_key].add(node_value)
 
    return result
def distinct_multinomial(ntrials, probs):
    """
    Draw ntrials samples from a multinomial distribution given by
    probs.  Return a list of indices into probs for all distinct
    elements that were selected.  Always returns a list with between 1
    and ntrials elements.

    Arguments:
    ntrials -- number of trials
    probs   -- probability vector for the multinomial, must sum to 1

    Returns: 
    A list of indices into probs for each element that was chosen one
    or more times.  If an element was chosen more than once, it will
    only appear once in the result.  
    """
    ### select ntrials elements randomly
    mult = numpy.random.multinomial(ntrials, probs)

    ### turn the results into a list of indices without duplicates
    result = [i for i, v in enumerate(mult) if v > 0]
    return result
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
def dfs(input_node, graph, visited):
    #print("NEW DFS")
    stack = Stack()
    cc = []
    stack.push(input_node)
    while len(stack) > 0:
        node = stack.pop()
        cc.append(node)
        #print(node)
        for neigh in graph[node]:
            #print(stack)
            if neigh not in visited:
                visited.append(neigh)
                stack.push(neigh)
    return cc, visited
def compute_largest_cc_size(g: dict) -> int:
    ccseq = []
    visited = []
    largest = -1
    for node in g:
        if node not in visited:
            visited.append(node)
            cc, visited = dfs(node, g, visited)
            if len(cc) > largest:
                largest = len(cc)
    return largest
def _plot_dict_line(d, label=None):
    """
    Plot data in the dictionary d on the current plot as a line.

    Arguments:
    d     -- dictionary
    label -- optional legend label

    Returns:
    None
    """
    xvals, yvals = _dict2lists(d)
    if label:
        pylab.plot(xvals, yvals, label=label)
    else:
        pylab.plot(xvals, yvals)
def _dict2lists(data):
    """
    Convert a dictionary into a list of keys and values, sorted by
    key.  

    Arguments:
    data -- dictionary

    Returns:
    A tuple of two lists: the first is the keys, the second is the values
    """
    xvals = list(data.keys())
    xvals.sort()
    yvals = []
    for x in xvals:
        yvals.append(data[x])
    return xvals, yvals
def plot_lines(data, title, xlabel, ylabel, labels=None, filename=None):
    """
    Plot a line graph with the provided data.

    Arguments: 
    data     -- a list of dictionaries, each of which will be plotted 
                as a line with the keys on the x axis and the values on
                the y axis.
    title    -- title label for the plot
    xlabel   -- x axis label for the plot
    ylabel   -- y axis label for the plot
    labels   -- optional list of strings that will be used for a legend
                this list must correspond to the data list
    filename -- optional name of file to which plot will be
                saved (in png format)

    Returns:
    None
    """
    ### Check that the data is a list
    if not isinstance(data, list):
        msg = "data must be a list, not {0}".format(type(data).__name__)
        raise TypeError(msg)

    ### Create a new figure
    fig = pylab.figure()

    ### Plot the data
    if labels:
        mylabels = labels[:]
        for _ in range(len(data)-len(labels)):
            mylabels.append("")
        for d, l in zip(data, mylabels):
            _plot_dict_line(d, l)
        # Add legend
        pylab.legend(loc='best')
        gca = pylab.gca()
        legend = gca.get_legend()
        pylab.setp(legend.get_texts(), fontsize='medium')
    else:
        for d in data:
            _plot_dict_line(d)

    ### Set the lower y limit to 0 or the lowest number in the values
    mins = [min(l.values()) for l in data]
    ymin = min(0, min(mins))
    pylab.ylim(ymin=ymin)

    ### Label the plot
    pylab.title(title)
    pylab.xlabel(xlabel)
    pylab.ylabel(ylabel)

    ### Draw grid lines
    pylab.grid(True)

    ### Show the plot
    fig.show()

    ### Save to file
    if filename:
        pylab.savefig(filename)
given_graph = read_graph("rf7.repr")

possible_nodes = (len(given_graph) * (len(given_graph) - 1)) / 2 #find all possible paths
numberofedges = total_degree(given_graph) / 2 

#can create similar ey graph by using the same number of nodes and 
#the probability of a path existing between two nodes as the given graph
ey_graph = erdos_renyi(len(given_graph), numberofedges / possible_nodes)
#can create similar upa graph by using the same number of nodes and 
#the average degree of each node as the given graph, rounded down
upa_graph = upa(len(given_graph), int(numberofedges / len(given_graph)))

def remove_node(graph, del_node):
    """
    Removes the node of interest from a graph
    Inputs: graph, the graph of interest
            del_node, a node to be deleted
    Output: the graph without del_node
    """
    graph.pop(del_node)
    for node in graph:
        if del_node in graph[node]: #removes node and the node as a neighbor
            graph[node].remove(del_node)
    return graph
def random_attack(graph):
    """
    Removes 20% of nodes from graphs in a random fashion, simulating a random attack
    Input: graph, the graph of interest
    Output: cc_data, a count of the connected components during the attack
    """
    nodes_to_take = int(0.2 * len(graph)) + 2 #calculates number of nodes to take
    cc_data = {}
    for idx in range(nodes_to_take):
        cc_data[idx] = compute_largest_cc_size(graph) #for each node to take, randomly choose a node and remove it
        random_node = random.choice(list(graph.keys()))
        output = remove_node(graph, random_node) 
    return cc_data
def targeted_attack(graph):
    """
    Removes 20% of nodes from graphs in order of highest degree, 
    simulating a targeted attack
    Input: graph, the graph of interest
    Output: cc_data, a count of the connected components during the attack
    """
    nodes_to_take = int(0.2 * len(graph)) + 2
    graph_neighbors = {}
    cc_data = {}
    for key in graph:
        graph_neighbors[key] = len(graph[key]) 
    keysorted = sorted(graph_neighbors, key=graph_neighbors.get, reverse = True)#gets degree of each node and sorts it
    for idx in range(nodes_to_take):
        cc_data[idx] = compute_largest_cc_size(graph)#go down list sorted by most degree and remove nodes one by one
        remove_node(graph, keysorted[idx])
    return cc_data
#test_graph_1 = {1: [2, 5,8], 2: [1, 3, 5,8], 3:[2,4, 7], 4:[3, 5, 6, 7], 5:[4,2], 6:[4], 7:[3, 4], 8:[1,2]}
#test_graph_2 = {0: [1, 2,3], 1: [0, 3], 3:[0,1], 2:[0], 5:[6,7], 6:[5], 7:[5]}
#print(random_attack(test_graph_1))
#print(random_attack(test_graph_2))
#print(targeted_attack(test_graph_1))
#print(targeted_attack(test_graph_2))


data = []
#append all data to plot
random_upa = random_attack(copy_graph(upa_graph))
data.append(random_upa)
random_ey = random_attack(copy_graph(ey_graph))
data.append(random_ey)
random_rf7 = random_attack(copy_graph(given_graph))
data.append(random_rf7)
targeted_upa = targeted_attack(copy_graph(upa_graph))
data.append(targeted_upa)
targeted_ey = targeted_attack(copy_graph(ey_graph))
data.append(targeted_ey)
targeted_rf7 = targeted_attack(copy_graph(given_graph))
data.append(targeted_rf7)
#find labels
labels = ["Random on UPA", "Random on EY", "Random on ISP"
          , "Targeted on UPA", "Targeted on EY", "Targeted on ISP"
          ]
#print(cc_data)
plot_lines(data, "Targeted vs Random Attacks", "Nodes Taken", "Connected Components", labels=labels, filename="compoutput")
show()


