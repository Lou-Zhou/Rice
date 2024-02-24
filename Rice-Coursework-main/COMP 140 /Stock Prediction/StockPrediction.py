"""
Stock market prediction using Markov chains.

For each function, replace the return statement with your code.  Add
whatever helper functions you deem necessary.
"""

import comp140_module3 as stocks
import random
### Model 
    
def markov_chain(data, order):
    """
    Create a Markov chain with the given order from the given data.

    inputs:
        - data: a list of ints or floats representing previously collected data
        - order: an integer repesenting the desired order of the markov chain

    returns: markov, a dictionary that represents the Markov chain
    """
    #Create starting dictionary for markov and empty list containing
    # every possible key
    markov = {}
    all_keys = []
    #Find all possible keys, loop through entire list
    for dataindex in range(len(data) - order):
        markov_keys = []
        #For each point, get next order - 1 points to get key
        # e.g. if order is 3, get next two points to get total key
        for subdataindex in range(order):
            markov_keys.append(data[dataindex + subdataindex])
        #If new key is not in all possible keys, add to all_keys
        if tuple(markov_keys) not in all_keys:
            all_keys.append(tuple(markov_keys))
            markov[tuple(markov_keys)] = {} #Set value to an empty dict
    
    #Now with every possible key, we get counts for each situation
    for index in range(len(data) - order):
        test_key = []
        subindex = 0
        #Get all possible keys(again) 
        while subindex < order:
            test_key.append(data[index + subindex])
            subindex = subindex + 1
        test_key = tuple(test_key)
        #Check if value right after a key is in the key's dictionary
        newindex = index + subindex
        if data[newindex] in markov[tuple(test_key)].keys():
            #If so, add one to that value
            markov[test_key][data[newindex]] = markov[test_key][data[newindex]] + 1
        else:
            #If not, set that value in key's dictionary to one
            markov[test_key][data[newindex]] = 1
            
    #Turn into probabilities       
    for key in markov:
        totalsum = 0
        #For each key, get the total sum of the key's dictionaries
        for subkey in markov[key].keys():
            totalsum = totalsum +markov[key][subkey]
        for subkey in markov[key].keys():
         # Then divide each value in the dictionary by the total sum
            markov[key][subkey] = markov[key][subkey] / totalsum
             
    return dict(markov)
        


### Predict
def get_random_choice(submodel):
    """ Generates a random choice based on the probabilities given in
    a dictionary
    
    Input: submodel, a dictionary describing the value to predict(the keys) 
    and the probability for each value(the value at the key)
    
    returns: random_choice, a randomly chosen key from submodel
    
    """

    #Get random number from 0 to 1
    choice = random.random()
    #Save key in case that all probabilities do not sum to 1
    submodelkey = 0
    choseninterval = 0
    for key in submodel.keys():
        #Iterate through all keys, add the probability to
        #choseninterval
        choseninterval = choseninterval + submodel[key]
        if choseninterval > choice:
            #If choice is in the interval, return the key
            return key
        submodelkey = key
    #return last key in case probabilities do not sum to 1
    return submodelkey

    
print(get_random_choice({1: .2, 2: .2, 3: .2, 4:.2, 5:.2}))
print("=")
def predict(model, last, num):
    """
    Predict the next num values given the model and the last values.

    inputs:
        - model: a dictionary representing a Markov chain
        - last: a list (with length of the order of the Markov chain)
                representing the previous states
        - num: an integer representing the number of desired future states

    returns: a list of integers that are the next num states
    """
    #Create output as empty list and copy of dictionary to prevent
    # mutation
    future_prediction = []
    history = last.copy()
    order = len(history)
    state = 0
    while state < num:
        #get last (order)th values of the whole history to get what situation
        # to predict off of
        present = history[-order:]
        if tuple(present) in model.keys():
            #if the situation is in model, choose randomly with weights from
            # model
            submodel = model[tuple(present)]
            current_prediction = get_random_choice(submodel)  
        else:
            #if not, pick randomly
            current_prediction = random.choice([0,1,2,3])
        future_prediction.append(current_prediction)
        history.append(current_prediction)
        #update the entire history and the output
        state = state + 1
    return future_prediction


#print(model.keys())
#model = predict({(0,): {1: 1}, (1,): {0: 1}}, [0], 3)
print(predict({(0,): {0: 0.3, 1: 0.5, 2: 0.2}}, [0], 1))

### Error

def mse(result, expected):
    """
    Calculate the mean squared error between two data sets.

    The length of the inputs, result and expected, must be the same.

    inputs:
        - result: a list of integers or floats representing the actual output
        - expected: a list of integers or floats representing the predicted output

    returns: a float that is the mean squared error between the two data sets
    """
    mean_square_error = 0
    for nums_index in range(len(result)):
        #for each value in result, take difference with coinciding value in
        # expected and square that result
        square_error = (result[nums_index] - expected[nums_index])**2
        #add to total error
        mean_square_error = mean_square_error + square_error
    #find mean by dividing total by the number of entires
    mean_square_error = mean_square_error / len(result)
    return mean_square_error


### Experiment

def run_experiment(train, order, test, future, actual, trials):
    """
    Run an experiment to predict the future of the test
    data given the training data.

    inputs:
        - train: a list of integers representing past stock price data
        - order: an integer representing the order of the markov chain
                 that will be used
        - test: a list of integers of length "order" representing past
                stock price data (different time period than "train")
        - future: an integer representing the number of future days to
                  predict
        - actual: a list representing the actual results for the next
                  "future" days
        - trials: an integer representing the number of trials to run

    returns: a float that is the mean squared error over the number of trials
    """
    totalerror = 0
    trialnum = 0
    while trialnum < trials:
        #for every trial, create a model and predict using that model
        model = markov_chain(train, order)
        predictions = predict(model, test, future)
        #find the mean square error for each trial and add to the total error
        error = mse(predictions, actual)
        totalerror = totalerror + error
        trialnum = trialnum + 1
    #output the average mean square error
    totalerror = totalerror / trials
    return totalerror


### Application

def run():
    """
    Run application.

    You do not need to modify any code in this function.  You should
    feel free to look it over and understand it, though.
    """
    # Get the supported stock symbols
    symbols = stocks.get_supported_symbols()

    # Get stock data and process it

    # Training data
    changes = {}
    bins = {}
    for symbol in symbols:
        prices = stocks.get_historical_prices(symbol)
        changes[symbol] = stocks.compute_daily_change(prices)
        bins[symbol] = stocks.bin_daily_changes(changes[symbol])

    # Test data
    testchanges = {}
    testbins = {}
    for symbol in symbols:
        testprices = stocks.get_test_prices(symbol)
        testchanges[symbol] = stocks.compute_daily_change(testprices)
        testbins[symbol] = stocks.bin_daily_changes(testchanges[symbol])

    # Display data
    #   Comment these 2 lines out if you don't want to see the plots
    stocks.plot_daily_change(changes)
    stocks.plot_bin_histogram(bins)

    # Run experiments
    orders = [1, 3, 5, 7, 9]
    ntrials = 500
    days = 5

    for symbol in symbols:
        print(symbol)
        print("====")
        print("Actual:", testbins[symbol][-days:])
        for order in orders:
            error = run_experiment(bins[symbol], order,
                                   testbins[symbol][-order-days:-days], days,
                                   testbins[symbol][-days:], ntrials)
            print("Order", order, ":", error)
        print()

# You might want to comment out the call to run while you are
# developing your code.  Uncomment it when you are ready to run your
# code on the provided data.

#run()
