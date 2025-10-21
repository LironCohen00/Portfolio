###### 323929992 ######
# ID1: 323929992
# ID2: 806614
#####################

# imports 
import numpy as np
import pandas as pd

def preprocess(X,y):
    """
    Perform mean normalization on the features and true labels.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).

    Returns:
    - X: The mean normalized inputs.
    - y: The mean normalized labels.
    """
    X = (X - (np.mean(X)))/(np.max(X) - np.min(X))
    y = (y - (np.mean(y)))/(np.max(y) - np.min(y))
    return X, y

def apply_bias_trick(X):
    """
    Applies the bias trick to the input data.

    Input:
    - X: Input data (m instances over n features).

    Returns:
    - X: Input data with an additional column of ones in the
        zeroth position (m instances over n+1 features).
    """
    if len(X.shape) == 1: # if X is just a 1-d array, meaning there is only 1 feature
       X = np.reshape(X, (-1, 1)) #reshape into a 2-d array with 1 column
    X = np.insert(X, 0, 1, axis=1) 
    return X

def compute_cost(X, y, theta):
    """
    Computes the average squared difference between an observation's actual and
    predicted values for linear regression.  

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).
    - theta: the parameters (weights) of the model being learned.

    Returns:
    - J: the cost associated with the current set of parameters (single number).
    """
    #m = np.size(y)
    J = np.sum(((X @ theta.T) - y)**2)
    J = J/(2*np.size(y))
    return J

def HypothesisFunction(x_i, theta):
    return np.matmul(theta, x_i)

def gradient_descent(X, y, theta, alpha, num_iters):
    """
    Learn the parameters of the model using gradient descent using 
    the training set. Gradient descent is an optimization algorithm 
    used to minimize some (loss) function by iteratively moving in 
    the direction of steepest descent as defined by the negative of 
    the gradient. We use gradient descent to update the parameters
    (weights) of our model.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).
    - theta: The parameters (weights) of the model being learned.
    - alpha: The learning rate of your model.
    - num_iters: The number of updates performed.

    Returns:
    - theta: The learned parameters of your model.
    - J_history: the loss value for every iteration.
    """
    
    theta = theta.copy() # optional: theta outside the function will not change
    J_history = [] # Use a python list to save the cost value in every iteration
    NumInstances = np.shape(X)[0]
    for i in range(num_iters):
        errors = (X @ theta.T) - y
        temp_thetas = np.array([])
        for j in range(len(theta)):
            temp_theta = theta[j] - alpha*(1/NumInstances)*(np.sum(errors*X[:, j]))
            temp_thetas = np.append(temp_thetas, temp_theta)
        theta = temp_thetas
        J_history.append(compute_cost(X, y, theta)) 
    return theta, J_history

def compute_pinv(X, y):
    """
    Compute the optimal values of the parameters using the pseudoinverse
    approach as you saw in class using the training set.

    #########################################
    #### Note: DO NOT USE np.linalg.pinv ####
    #########################################

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).

    Returns:
    - pinv_theta: The optimal parameters of your model.
    """
    pinvX = np.matmul(np.linalg.inv(np.matmul(np.transpose(X), X)), np.transpose(X))
    pinv_theta = np.matmul(pinvX, y)
    return pinv_theta

def efficient_gradient_descent(X, y, theta, alpha, num_iters):
    """
    Learn the parameters of your model using the training set, but stop 
    the learning process once the improvement of the loss value is smaller 
    than 1e-8. This function is very similar to the gradient descent 
    function you already implemented.

    Input:
    - X: Input data (m instances over n features).
    - y: True labels (m instances).
    - theta: The parameters (weights) of the model being learned.
    - alpha: The learning rate of your model.
    - num_iters: The number of updates performed. ?????????????????????????????????

    Returns:
    - theta: The learned parameters of your model.
    - J_history: the loss value for every iteration.
    """
    
    theta = theta.copy() # optional: theta outside the function will not change
    J_history = [] # Use a python list to save the cost value in every iteration
    NumInstances = np.shape(X)[0]
    for i in range(num_iters):
        errors = (X @ theta.T) - y
        temp_thetas = np.array([])
        for j in range(len(theta)):
            temp_theta = theta[j] - alpha*(1/NumInstances)*(np.sum(errors*X[:, j]))
            temp_thetas = np.append(temp_thetas, temp_theta)
        theta = temp_thetas
        J_history.append(compute_cost(X, y, theta)) 
        if (len(J_history) >= 2) and (J_history[-2] - J_history[-1] < 1e-8):
            break
    return theta, J_history

def find_best_alpha(X_train, y_train, X_val, y_val, iterations):
    """
    Iterate over the provided values of alpha and train a model using 
    the training dataset. maintain a python dictionary with alpha as the 
    key and the loss on the validation set as the value.

    You should use the efficient version of gradient descent for this part. 

    Input:
    - X_train, y_train, X_val, y_val: the training and validation data
    - iterations: maximum number of iterations ??????????????????????

    Returns:
    - alpha_dict: A python dictionary - {alpha_value : validation_loss}
    """
    
    alphas = [0.00001, 0.00003, 0.0001, 0.0003, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1, 2, 3]
    alpha_dict = {} # {alpha_value: validation_loss}
    init_thetas = np.random.random(size=X_train.shape[1])
    for alpha in alphas:
        alpha_dict[alpha] = compute_cost(X_val, y_val, efficient_gradient_descent(X_train, y_train,init_thetas , alpha, 40000)[0]) 
    return alpha_dict

def forward_feature_selection(X_train, y_train, X_val, y_val, best_alpha, iterations):
    """
    Forward feature selection is a greedy, iterative algorithm used to 
    select the most relevant features for a predictive model. The objective 
    of this algorithm is to improve the model's performance by identifying 
    and using only the most relevant features, potentially reducing overfitting, 
    improving accuracy, and reducing computational cost.

    You should use the efficient version of gradient descent for this part. 

    Input:
    - X_train, y_train, X_val, y_val: the input data without bias trick
    - best_alpha: the best learning rate previously obtained
    - iterations: maximum number of iterations for gradient descent

    Returns:
    - selected_features: A list of selected top 5 feature indices
    """
    selected_features = []
    costs = {}
    while len(selected_features) < 5:
        for i in range(X_train.shape[1]): # for every column
            if i not in selected_features:
                selected_features.append(i)
                X_temp = X_train[: , selected_features] # create a table with only the selected features' values
                X_temp = apply_bias_trick(X_temp)
                theta = efficient_gradient_descent(X_temp, y_train, np.ones(len(selected_features) + 1), best_alpha, iterations)[0]
                costs[i] = compute_cost(apply_bias_trick(X_val[: , selected_features]), y_val, theta)
                selected_features.remove(i)
        selected_features.append(min(costs, key=costs.get))
        costs.clear()
    return selected_features


def create_square_features(df):
    """
    Create square features for the input data.

    Input:
    - df: Input data (m instances over n features) as a dataframe.

    Returns:
    - df_poly: The input data with polynomial features added as a dataframe
               with appropriate feature names
    """
    df_poly = df.copy()
    df_old = df.copy()
    for i in range(len(df_old.columns)):
        for j in range(i, len(df_old.columns)):
            col_name = df_old.columns[i] + '*' + df_old.columns[j]
            df_poly[col_name] = df_old.iloc[:, i] * df_old.iloc[:, j]
    df_poly = df.copy()
    return df_poly