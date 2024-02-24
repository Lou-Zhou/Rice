"""
Code to implement the game of Spot it!

http://www.blueorangegames.com/spotit/

For each function, replace the return statement with your code.  Add
whatever helper functions you deem necessary.
"""

import comp140_module2 as spotit

def equivalent(point1, point2, mod):
    """
    Determines if the two given points are equivalent in the projective
    geometric space in the finite field with the given modulus.

    Each input point, point1 and point2, must be valid within the
    finite field with the given modulus.

    inputs:
        - point1: a tuple of 3 integers representing the first point
        - point2: a tuple of 3 integers representing the second point
        - mod: an integer representing the modulus

    returns: a boolean indicating whether or not the points are equivalent
    """
    crossproduct = (point1[1] * point2[2] - point1[2] * point2[1], 
                    point1[2] * point2[0] - point1[0] * point2[2], point1[0] * point2[1] 
                    - point1[1] * point2[0])
    if(crossproduct[0] % mod == 0 and crossproduct[1] % mod == 0 
        and crossproduct[2] % mod == 0):
        return True
    return False
print(equivalent((3,4,1),(3,4,1),5))

def incident(point, line, mod):
    """
    Determines if a point lies on a line in the projective
    geometric space in the finite field with the given modulus.

    The inputs point and line must be valid within the finite field
    with the given modulus.

    inputs:
        - point: a tuple of 3 integers representing a point
        - line: a tuple of 3 integers representing a line
        - mod: an integer representing the modulus

    returns: a boolean indicating whether or not the point lies on the line
    """
    if (point[0] * line[0] + point[1] * line[1] + point[2] * line[2]) % mod == 0:
        return True
    
    return False

def generate_all_nonunique_points(mod):
    """
    Generate all points in the projective geometric space in
    the finite field with the given modulus.

    inputs:
        - mod: an integer representing the modulus

    Returns: a list of all points, each is a tuple of 3 elements
    """
    output = []
    for x_point in range(mod):
        for y_point in range(mod):
            for z_point in range(mod):
                point = (x_point,y_point,z_point)
                output.append(point)
    output.remove((0,0,0))
    return output

def generate_all_points(mod):
    """
    Generate all unique points in the projective geometric space in
    the finite field with the given modulus.

    inputs:
        - mod: an integer representing the modulus

    Returns: a list of unique points, each is a tuple of 3 elements
    """
    nonunique = generate_all_nonunique_points(mod)
    unique = nonunique
    print(len(nonunique))
    
    for point in nonunique:
        for other_point in nonunique[nonunique.index(point) + 1:]:
            if equivalent(point, other_point, mod):
                unique.remove(other_point)      
    return unique

def create_cards(points, lines, mod):
    """
    Create a list of unique cards.

    Each point and line within the inputs, points and lines, must be
    valid within the finite field with the given modulus.

    inputs:
        - points: a list of unique points, each represented as a tuple of 3 integers
        - lines: a list of unique lines, each represented as a tuple of 3 integers
        - mod: an integer representing the modulus

    returns: a list of lists of integers, where each nested list represents a card.
    """
    deck = []
    for line_iterated in lines:
        card = []
        for point_iterated in points:
            if incident(line_iterated, point_iterated, mod):
                card.append(points.index(point_iterated))
        deck.append(card)
    return deck
print(create_cards(generate_all_points(3), generate_all_points(3), 3))
def run():
    """
    Create the deck and play the game.
    """
    # Prime modulus
    # Set to 2 or 3 during development
    # Set to 7 for the actual game
    modulus = 2

    # Generate all unique points for the given modulus
    points = generate_all_points(modulus)

    # Lines are the same as points, so make a copy
    lines = points[:]

    # Generate a deck of cards given the points and lines
    deck = create_cards(points, lines, modulus)

    # Run GUI - uncomment the line below after you have implemented
    #           everything and you can play your game.  The GUI does
    #           not work if the modulus is larger than 7.

    spotit.start(deck)

# Uncomment the following line to run your game (once you have
# implemented the run function.)

#run()
