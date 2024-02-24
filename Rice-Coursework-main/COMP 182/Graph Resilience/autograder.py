# Lou Zhou
# lz80
# COMP 182 Spring 2021 - Homework 4, Problem 3

# You may NOT import anything apart from already imported libraries.
# You can use helper functions from comp182.py and provided.py, but they have
# to be copied over here.

from collections import *
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
# test_graph_1 = {1: [2, 5], 2: [1, 3, 5], 3:[2,4], 4:[3, 5, 6], 5:[4,2], 6:[4]
# }

# test_graph_2 = {0: [1, 2,3 ], 1: [0, 3], 3:[0,1], 2:[0], 5:[6,7], 6:[5], 7:[5]
# }

def dfs(input_node, graph, visited):
    """
    Implements a modified DFS search algorithm which finds the connected component a node is in
    Input: input_node, a node of interest
           graph, the graph that input_node is in
           visited, a list which documents which nodes have been visited
    Output: cc, a list describing the connected component
            visited, an updated version of the inputed visited list
    """
    stack = Stack()
    cc = []
    stack.push(input_node) #initialize stack and push input_node
    while len(stack) > 0:
        node = stack.pop()
        cc.append(node) #pop out and add node to cc
        for neigh in graph[node]:
            if neigh not in visited: #go through neighbors of node, pushing to stack
                visited.append(neigh)
                stack.push(neigh)
    return cc, visited 
def compute_largest_cc_size(g: dict) -> int:
    """
    Finds the size of the largest connected component within a graph
    Input: graph, the graph of interest
    Output: largest, an integer that describes the size of the largest connected component
    """
    ccseq = []
    visited = []
    largest = -1
    for node in g:
        if node not in visited:
            visited.append(node) #go through unvisited nodes, finding connected component
            cc, visited = dfs(node, g, visited)
            if len(cc) > largest:
                largest = len(cc) #if larger than largest component, save larger value
    return largest
#print(compute_largest_cc_size(test_graph_1))
#print(compute_largest_cc_size(test_graph_2))