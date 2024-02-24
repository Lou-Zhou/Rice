"""
Code to calculate the circle that passes through three given points.

Fill in each function with your code (including fixing the return
statement).
"""

import math
import comp140_module1 as circles

def distance(point0x, point0y, point1x, point1y):
    """
    Computes the distance between two points.

    inputs:
        -point0x: a float representing the x-coordinate of the first point
        -point0y: a float representing the y-coordinate of the first point
        -point1x: a float representing the x-coordinate of the second point
        -point1y: a float representing the y-coordinate of the second point

    returns: a float that is the distance between the two points
    """
    dis = math.sqrt((point1x-point0x)**2 + (point1y-point0y)**2)
    return dis

def midpoint(point0x, point0y, point1x, point1y):
    """
    Computes the midpoint between two points.

    inputs:
        -point0x: a float representing the x-coordinate of the first point
        -point0y: a float representing the y-coordinate of the first point
        -point1x: a float representing the x-coordinate of the second point
        -point1y: a float representing the y-coordinate of the second point

    returns: two floats that are the x- and y-coordinates of the midpoint
    """
    x_midpoint = (point0x + point1x)/2
    y_midpoint = (point0y + point1y)/2
    return x_midpoint, y_midpoint

def slope(point0x, point0y, point1x, point1y):
    """
    Computes the slope of the line that connects two given points.

    The x-values of the two points, point0x and poin1x, must be different.

    inputs:
        -point0x: a float representing the x-coordinate of the first point.
        -point0y: a float representing the y-coordinate of the first point
        -point1x: a float representing the x-coordinate of the second point.
        -point1y: a float representing the y-coordinate of the second point

    returns: a float that is the slope between the points
    """
    newslope = (point1y - point0y) / (point1x - point0x)
    return newslope

def perp(lineslope):
    """
    Computes the slope of a line perpendicular to a given slope.

    input:
        -lineslope: a float representing the slope of a line.
                    Must be non-zero

    returns: a float that is the perpendicular slope
    """
    return -1 / lineslope

def intersect(slope0, point0x, point0y, slope1, point1x, point1y):
    """
    Computes the intersection point of two lines.

    The two slopes, slope0 and slope1, must be different.

    inputs:
        -slope0: a float representing the slope of the first line.
        -point0x: a float representing the x-coordinate of the first point
        -point0y: a float representing the y-coordinate of the first point
        -slope1: a float representing the slope of the second line.
        -point1x: a float representing the x-coordinate of the second point
        -point1y: a float representing the y-coordinate of the second point

    returns: two floats that are the x- and y-coordinates of the intersection
    point
    """
    intercept_x = ((slope0 * point0x) - (slope1 * point1x) + (point1y-point0y))/(slope0 - slope1)
    intercept_y = slope0 * (intercept_x - point0x) + point0y
    return intercept_x, intercept_y

def make_circle(point0x, point0y, point1x, point1y, point2x, point2y):
    """
    Computes the center and radius of a circle that passes through
    thre given points.

    The points must not be co-linear and no two points can have the
    same x or y values.

    inputs:
        -point0x: a float representing the x-coordinate of the first point
        -point0y: a float representing the y-coordinate of the first point
        -point1x: a float representing the x-coordinate of the second point
        -point1y: a float representing the y-coordinate of the second point
        -point2x: a float representing the x-coordinate of the third point
        -point2y: a float representing the y-coordinate of the third point

    returns: three floats that are the x- and y-coordinates of the center
    and the radius
    """
    midpoint_1_x, midpoint_1_y = midpoint(point0x, point0y, point1x, point1y)
    slope0 = slope(point0x, point0y, point1x, point1y)
    slope0 = perp(slope0)
    
    
    midpoint_2_x, midpoint_2_y = midpoint(point1x, point1y, point2x, point2y)
    slope1 = slope(point1x, point1y, point2x, point2y)
    slope1 = perp(slope1)
    center_x, center_y = intersect(slope0, midpoint_1_x, midpoint_1_y, slope1, 
                                   midpoint_2_x, midpoint_2_y)
    radius = distance(center_x, center_y, point0x, point0y) 
    return center_x, center_y, radius

# Run GUI - uncomment the line below after you have
#           implemented make_circle
circles.start(make_circle)
