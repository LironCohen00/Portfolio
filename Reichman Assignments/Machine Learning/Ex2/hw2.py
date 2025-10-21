import numpy as np
import matplotlib.pyplot as plt
import math

### Chi square table values ###
# The first key is the degree of freedom 
# The second key is the p-value cut-off
# The values are the chi-statistic that you need to use in the pruning

chi_table = {1: {0.5 : 0.45,
             0.25 : 1.32,
             0.1 : 2.71,
             0.05 : 3.84,
             0.0001 : 100000},
         2: {0.5 : 1.39,
             0.25 : 2.77,
             0.1 : 4.60,
             0.05 : 5.99,
             0.0001 : 100000},
         3: {0.5 : 2.37,
             0.25 : 4.11,
             0.1 : 6.25,
             0.05 : 7.82,
             0.0001 : 100000},
         4: {0.5 : 3.36,
             0.25 : 5.38,
             0.1 : 7.78,
             0.05 : 9.49,
             0.0001 : 100000},
         5: {0.5 : 4.35,
             0.25 : 6.63,
             0.1 : 9.24,
             0.05 : 11.07,
             0.0001 : 100000},
         6: {0.5 : 5.35,
             0.25 : 7.84,
             0.1 : 10.64,
             0.05 : 12.59,
             0.0001 : 100000},
         7: {0.5 : 6.35,
             0.25 : 9.04,
             0.1 : 12.01,
             0.05 : 14.07,
             0.0001 : 100000},
         8: {0.5 : 7.34,
             0.25 : 10.22,
             0.1 : 13.36,
             0.05 : 15.51,
             0.0001 : 100000},
         9: {0.5 : 8.34,
             0.25 : 11.39,
             0.1 : 14.68,
             0.05 : 16.92,
             0.0001 : 100000},
         10: {0.5 : 9.34,
              0.25 : 12.55,
              0.1 : 15.99,
              0.05 : 18.31,
              0.0001 : 100000},
         11: {0.5 : 10.34,
              0.25 : 13.7,
              0.1 : 17.27,
              0.05 : 19.68,
              0.0001 : 100000}}

def calc_gini(data):
    """
    Calculate gini impurity measure of a dataset.
 
    Input:
    - data: any dataset where the last column holds the labels.
 
    Returns:
    - gini: The gini impurity value.
    """
    labels = data[: , data.shape[1] - 1]
    numEdible, numPoisonous = calc_classSizes(labels)
    gini = 1 - (numEdible/len(labels))**2 - (numPoisonous/len(labels))**2
    return gini

def calc_classSizes(labels):
    numEdible = 0
    numPoisonous = 0
    for label in labels:
        if (label == 'e'):
            numEdible += 1
        else:
            numPoisonous += 1
    return numEdible, numPoisonous


def calc_entropy(data):
    """
    Calculate the entropy of a dataset.

    Input:
    - data: any dataset where the last column holds the labels.

    Returns:
    - entropy: The entropy value.
    """
    labels = data[: , data.shape[1] - 1]
    numEdible, numPoisonous = calc_classSizes(labels)
    if numEdible == 0:
        entropy = - (numPoisonous/len(labels))*math.log2(numPoisonous/len(labels))
    elif numPoisonous == 0:
        entropy = - (numEdible/len(labels))*math.log2(numEdible/len(labels))
    else:
        entropy = - (numEdible/len(labels))*math.log2(numEdible/len(labels)) - (numPoisonous/len(labels))*math.log2(numPoisonous/len(labels))
    return entropy

def goodness_of_split(data, feature, impurity_func, gain_ratio=False):
    """
    Calculate the goodness of split of a dataset given a feature and impurity function.
    Note: Python support passing a function as arguments to another function
    Input:
    - data: any dataset where the last column holds the labels.
    - feature: the feature index the split is being evaluated according to.
    - impurity_func: a function that calculates the impurity.
    - gain_ratio: goodness of split or gain ratio flag.

    Returns:
    - goodness: the goodness of split value
    - groups: a dictionary holding the data after splitting 
              according to the feature values.
    """
    groups = {}
    for instance in data:
            feature_value = instance[feature]
            if feature_value not in groups: 
                groups[feature_value] = data[data[:, feature] == feature_value] 
    if gain_ratio:
        split_info = calc_entropy(data)
        info_gain, a = goodness_of_split(data, feature, calc_entropy, False)
        goodness= info_gain/split_info
            
    else:
        total_impurity = 0
        old_impurity = impurity_func(data)
        for key in groups:
            total_impurity = total_impurity + (impurity_func(groups[key]) * (groups[key].shape[0] / data.shape[0]))
        goodness = old_impurity - total_impurity
    return goodness, groups

class DecisionNode:

    def __init__(self, data, feature=-1,depth=0, chi=1, max_depth=1000, gain_ratio=False):
        
        self.data = data # the relevant data for the node
        self.feature = feature # column index of criteria being tested
        self.pred = self.calc_node_pred() # the prediction of the node
        self.depth = depth # the current depth of the node
        self.children = [] # array that holds this nodes children
        self.children_values = []
        self.terminal = False # determines if the node is a leaf
        self.chi = chi 
        self.max_depth = max_depth # the maximum allowed depth of the tree
        self.gain_ratio = gain_ratio 
    
    def calc_node_pred(self):
        """
        Calculate the node prediction.

        Returns:
        - pred: the prediction of the node
        """
        pred = None
        
        return pred
        
    def add_child(self, node, val):
        """
        Adds a child node to self.children and updates self.children_values

        This function has no return value
        """
        self.children.append(node)
        self.children_values.append(val)
     
    def split(self, impurity_func):

        """
        Splits the current node according to the impurity_func. This function finds
        the best feature to split according to and create the corresponding children.
        This function should support pruning according to chi and max_depth.

        Input:
        - The impurity function that should be used as the splitting criteria

        This function has no return value
        """
        bestGoodnessOfSplit = 0
        feature = None
        FinalChildren = np.empty((0,0))
        val = ""
        for i in range(0, self.data.shape[1]):
            currentGoodnessOfSplit, currentChildren = goodness_of_split(self.data, i, impurity_func, self.gain_ratio)
            if (currentGoodnessOfSplit > bestGoodnessOfSplit):
                bestGoodnessOfSplit = currentGoodnessOfSplit
                FinalChildren = currentChildren
                feature = i
        for feature_value in FinalChildren:
            node = DecisionNode(FinalChildren[feature_value], -1, self.depth + 1, 1, self.max_depth, self.gain_ratio) 
            self.feature = feature
            self.add_child(node, feature_value)



def build_tree(data, impurity, gain_ratio=False, chi=1, max_depth=1000):
    """
    Build a tree using the given impurity measure and training dataset. 
    You are required to fully grow the tree until all leaves are pure unless
    you are using pruning

    Input:
    - data: the training dataset.
    - impurity: the chosen impurity measure. Notice that you can send a function
                as an argument in python.
    - gain_ratio: goodness of split or gain ratio flag

    Output: the root node of the tree.
    """
    root = DecisionNode(data, -1, 0, 1, max_depth, gain_ratio)
    build_tree_recursion(root, data, impurity, gain_ratio, chi, max_depth)
    return root

def build_tree_recursion(node, data, impurity, gain_ratio, chi, max_depth):
    if impurity(data) != 0:
        if node.depth != max_depth:
            node.split(impurity)
            for child in node.children:
                build_tree_recursion(child, child.data, impurity, gain_ratio, 1, max_depth)
        else:
         node.terminal = True   
    else:
        node.terminal = True

def predict(root, instance):
    """
    Predict a given instance using the decision tree
 
    Input:
    - root: the root of the decision tree.
    - instance: an row vector from the dataset. Note that the last element 
                of this vector is the label of the instance.
 
    Output: the prediction of the instance.
    """

    if root.terminal == True:
        return root.data[0][-1]
    else:
        featureTested = root.feature
        for i in range(0, len(root.children)):
            if (instance[featureTested] == root.children_values[i]):
                return predict(root.children[i], instance)

def calc_accuracy(node, dataset):
    """
    Predict a given dataset using the decision tree and calculate the accuracy
 
    Input:
    - node: a node in the decision tree.
    - dataset: the dataset on which the accuracy is evaluated
 
    Output: the accuracy of the decision tree on the given dataset (%).
    """
    NumAccurate = 0
    for instance in dataset:
        if (predict(node, instance) == instance[-1]):
            NumAccurate += 1
    return (NumAccurate/dataset.shape[0])

def depth_pruning(X_train, X_test):
    """
    Calculate the training and testing accuracies for different depths
    using the best impurity function and the gain_ratio flag you got
    previously.

    Input:
    - X_train: the training data where the last column holds the labels
    - X_test: the testing data where the last column holds the labels
 
    Output: the training and testing accuracies per max depth
    """
    training = []
    testing  = []
    ###########################################################################
    # TODO: Implement the function.                                           #
    ###########################################################################
    for max_depth in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
        prunned_tree = build_tree(X_train, calc_entropy, True, max_depth)
        training.append(calc_accuracy(prunned_tree, X_train))
        testing.append(calc_accuracy(prunned_tree, X_test))
    ###########################################################################
    #                             END OF YOUR CODE                            #
    ###########################################################################
    return training, testing


def chi_pruning(X_train, X_test):

    """
    Calculate the training and testing accuracies for different chi values
    using the best impurity function and the gain_ratio flag you got
    previously. 

    Input:
    - X_train: the training data where the last column holds the labels
    - X_test: the testing data where the last column holds the labels
 
    Output:
    - chi_training_acc: the training accuracy per chi value
    - chi_testing_acc: the testing accuracy per chi value
    - depths: the tree depth for each chi value
    """
    chi_training_acc = []
    chi_testing_acc  = []
    depth = []
    return chi_training_acc, chi_testing_acc, depth

def count_nodes(node):
    """
    Count the number of node in a given tree
 
    Input:
    - node: a node in the decision tree.
 
    Output: the number of nodes in the tree.
    """
    n_nodes = None
    n_nodes = 1
    for child in node.children:
        n_nodes += count_nodes(child)
    return n_nodes






