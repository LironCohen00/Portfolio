import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

class LogisticRegressionGD(object):
    """
    Logistic Regression Classifier using gradient descent.

    Parameters
    ------------
    eta : float
      Learning rate (between 0.0 and 1.0)
    n_iter : int
      Passes over the training dataset.
    eps : float
      minimal change in the cost to declare convergence
    random_state : int
      Random number generator seed for random weight
      initialization.
    """

    def __init__(self, eta=0.00005, n_iter=10000, eps=0.000001, random_state=1):
        self.eta = eta
        self.n_iter = n_iter
        self.eps = eps
        self.random_state = random_state
        self.costs = []

        # model parameters
        self.theta = None

        # iterations history
        self.Js = []
        self.thetas = []

    def sigmoid(self, x):
        return 1/(1+np.exp(-x))
    

    def get_cost(self,X, y):
        ##cost = (1/m)*np.sum(np.dot(-y , np.log(self.h(X))) - np.dot((1-y),(np.log(1-self.h(X)))))
       
        dot = np.dot(X,self.thetas.reshape(-1, 1))
        cost1 = np.dot(y, np.log(self.sigmoid(dot)))
        cost2 = np.dot((1-y), np.log(1-self.sigmoid(dot)))
        return -(cost1+cost2)/len(X)
    
    def get_derivative_cost_i(self, X, y, i):
        return (np.sum((self.sigmoid(np.dot(X,self.thetas)) - y)  * X[:,i] ))
    
    def fit(self, X, y):
        """
        Fit training data (the learning phase).
        Update the theta vector in each iteration using gradient descent.
        Store the theta vector in self.thetas.
        Stop the function when the difference between the previous cost and the current is less than eps
        or when you reach n_iter.
        The learned parameters must be saved in self.theta.
        This function has no return value.

        Parameters
        ----------
        X : {array-like}, shape = [n_examples, n_features]
          Training vectors, where n_examples is the number of examples and
          n_features is the number of features.
        y : array-like, shape = [n_examples]
          Target values.

        """
        # set random seed
        np.random.seed(self.random_state)

        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        i = 0
        self.thetas = np.zeros(len(X[0]))
        
        cost = self.get_cost(X,y)
        cost_change = cost
        
        for j in range(self.n_iter):
            temp_thetas = self.thetas[:]
            if cost_change < self.eps:
                return 
            else:
                for i in range(len(self.thetas)):

                  temp_thetas[i] = self.thetas[i] - (self.eta*self.get_derivative_cost_i(X, y, i))  
                  
                self.thetas = temp_thetas
                old_cost = cost
                cost = self.get_cost(X,y)
                self.costs.append(cost)
                cost_change = old_cost - cost
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################

    def predict(self, X):
        """
        Return the predicted class labels for a given instance.
        Parameters
        ----------
        X : {array-like}, shape = [n_examples, n_features]
        """
        preds = None
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        p = self.sigmoid(np.dot(X,self.thetas.reshape(-1, 1)))
        preds = []
        for i in p:
          if i < 0.5:
              preds.append(0)
          else:
              preds.append(1)

        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################
        return preds

def cross_validation(X, y, folds, algo, random_state):
    """
    This function performs cross validation as seen in class.

    1. shuffle the data and creates folds
    2. train the model on each fold
    3. calculate aggregated metrics

    Parameters
    ----------
    X : {array-like}, shape = [n_examples, n_features]
      Training vectors, where n_examples is the number of examples and
      n_features is the number of features.
    y : array-like, shape = [n_examples]
      Target values.
    folds : number of folds (int)
    algo : an object of the classification algorithm
    random_state : int
      Random number generator seed for random weight
      initialization.

    Returns the cross validation accuracy.
    """

    cv_accuracy = None

    # set random seed
    np.random.seed(random_state)

    ###########################################################################
    # TODO: Implement the function.                                           #
    ###########################################################################
    fold_size = len(X) // folds
    indices = np.random.permutation(len(X))
    X_shuffled = X[indices]
    y_shuffled = y[indices]

    scores = []

    for fold in range(folds):
      start = fold * fold_size
      end = (fold + 1) * fold_size
      val_X = X_shuffled[start:end]
      val_y = y_shuffled[start:end]
      train_X = np.concatenate((X_shuffled[:start], X_shuffled[end:]))
      train_y = np.concatenate((y_shuffled[:start], y_shuffled[end:]))

      algo.fit(train_X, train_y)

      score = 0
      ##print("outer")
      for x in range(len(val_X)):
          ##print("inner")
          if algo.predict(val_X[x]) == val_y[x]:
              score += 1
      score = score/len(val_X)
      scores.append(score)

    cv_accuracy = np.mean(scores)
    ###########################################################################
    #                             END OF YOUR CODE                            #
    ###########################################################################
    return cv_accuracy

def norm_pdf(data, mu, sigma):
    """
    Calculate normal desnity function for a given data,
    mean and standrad deviation.
 
    Input:
    - x: A value we want to compute the distribution for.
    - mu: The mean value of the distribution.
    - sigma:  The standard deviation of the distribution.
 
    Returns the normal distribution pdf according to the given mu and sigma for the given x.    
    """
    p = None
    ###########################################################################
    # TODO: Implement the function.                                           #
    ###########################################################################
    p = (1/ (sigma * np.sqrt(2*np.pi))) * np.exp((-1/2) * (((data-mu)/sigma)**2))
    ###########################################################################
    #                             END OF YOUR CODE                            #
    ###########################################################################
    return p

class EM(object):
    """
    Naive Bayes Classifier using Gauusian Mixture Model (EM) for calculating the likelihood.

    Parameters
    ------------
    k : int
      Number of gaussians in each dimension
    n_iter : int
      Passes over the training dataset in the EM proccess
    eps: float
      minimal change in the cost to declare convergence
    random_state : int
      Random number generator seed for random params initialization.
    """

    def __init__(self, k=1, n_iter=1000, eps=0.01, random_state=1991):
        self.k = k
        self.n_iter = n_iter
        self.eps = eps
        self.random_state = random_state

        np.random.seed(self.random_state)

        self.responsibilities = None
        self.weights = None
        self.mus = None
        self.sigmas = None
        self.costs = None

    # initial guesses for parameters
    def init_params(self, data):
        """
        Initialize distribution params
        """
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        n_samples = data.shape[0]
        self.weights = np.ones(self.k) / self.k
        self.mus = np.random.choice(data.flatten(), size=self.k, replace=False)
        self.sigmas = np.random.random(self.k) + 1.0
        self.responsibilities = np.zeros((n_samples, self.k))
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################

    def expectation(self, data):
        """
        E step - This function should calculate and update the responsibilities
        """
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        n_samples = data.shape[0]

        for i in range(n_samples):
            for j in range(self.k):
                self.responsibilities[i, j] = self.weights[j] * norm_pdf(data[i][-1], self.mus[j], self.sigmas[j])
            self.responsibilities[i] /= np.sum(self.responsibilities[i])
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################

    def maximization(self, data):
        """
        M step - This function should calculate and update the distribution params
        """
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        n_samples = data.shape[0]

        for j in range(self.k):
            denominator = np.sum(self.responsibilities[:, j])
            
            self.weights[j] = denominator / n_samples
            
     
            self.mus[j] = np.dot(self.responsibilities[:, j], data) / self.weights[j]*(n_samples)
 
            self.sigmas[j] = np.sqrt(np.dot(self.responsibilities[:, j], (data - self.mus[j]) ** 2) / self.weights[j]*(n_samples))
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################
    def compute_cost(self, data):
        log_likelihood = []

        for j in range(self.k):
            log_likelihood.append(self.weights[j] * norm_pdf(data, self.mus[j], self.sigmas[j]))  
        return -np.sum((np.log(log_likelihood)))
    

    def fit(self, data):
        """
        Fit training data (the learning phase).
        Use init_params and then expectation and maximization function in order to find params
        for the distribution.
        Store the params in attributes of the EM object.
        Stop the function when the difference between the previous cost and the current is less than eps
        or when you reach n_iter.
        """
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        self.init_params(data)
        self.costs = []

        for _ in range(self.n_iter):
          prev_cost = self.compute_cost(data)

          self.expectation(data)
          self.maximization(data)

          current_cost = self.compute_cost(data)
          self.costs.append(current_cost)

          if np.abs(prev_cost - current_cost) < self.eps:
              return 
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################



    def get_dist_params(self):
        return self.weights, self.mus, self.sigmas

def gmm_pdf(data, weights, mus, sigmas):
    """
    Calculate gmm desnity function for a given data,
    mean and standrad deviation.
 
    Input:
    - data: A value we want to compute the distribution for.
    - weights: The weights for the GMM
    - mus: The mean values of the GMM.
    - sigmas:  The standard deviation of the GMM.
 
    Returns the GMM distribution pdf according to the given mus, sigmas and weights
    for the given data.    
    """
    pdf = None
    ###########################################################################
    # TODO: Implement the function.                                           #
    ###########################################################################
    pdf = np.sum(np.dot(weights, norm_pdf(data,mus, sigmas)))
    ###########################################################################
    #                             END OF YOUR CODE                            #
    ###########################################################################
    return pdf

class NaiveBayesGaussian(object):
    """
    Naive Bayes Classifier using Gaussian Mixture Model (EM) for calculating the likelihood.

    Parameters
    ------------
    k : int
      Number of gaussians in each dimension
    random_state : int
      Random number generator seed for random params initialization.
    """

    def __init__(self, k=1, random_state=1991):
        self.k = k
        self.random_state = random_state
        self.prior = None

    def fit(self, X, y):
        """
        Fit training data.

        Parameters
        ----------
        X : array-like, shape = [n_examples, n_features]
          Training vectors, where n_examples is the number of examples and
          n_features is the number of features.
        y : array-like, shape = [n_examples]
          Target values.
        """
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        
        ##need gmm for each classs
        unique_classes, num_appearances = np.unique(y, return_counts=True)
        self.prior = num_appearances / len(y)
        self.gmm_models = np.empty((len(unique_classes), X.shape[1]), dtype = EM)


        for class_Index in range(len(unique_classes)):
            # Select data points belonging to the current class
            class_X = X[y == unique_classes[class_Index]]

            
            for featureIndex in range(X.shape[1]): 
            # Create a GMM model for each feature
              gmm_model = EM(self.k)
              gmm_model.fit(class_X[ :, featureIndex].reshape(-1, 1))
              self.gmm_models[class_Index, featureIndex] = gmm_model     
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################

    def predict(self, X):
        """
        Return the predicted class labels for a given instance.
        Parameters
        ----------
        X : {array-like}, shape = [n_examples, n_features]
        """
        preds = None
        ###########################################################################
        # TODO: Implement the function.                                           #
        ###########################################################################
        preds = []

        #print(len(self.gmm_models))
        #print(self.gmm_models[0].get_dist_params()[0])
        for x in X:
            x_likelihoods = []
            for class_label in range(self.gmm_models.shape[0]):
                likelihood = 1
                for featureIndex in range(len(X[0])):
                  weights, mus, sigmas = self.gmm_models[class_label, featureIndex].get_dist_params()
                  feature_value = x[featureIndex]
                  likelihood += (np.log(gmm_pdf(feature_value, weights, mus, sigmas)))
                x_likelihoods.append((likelihood))

            # Compute the posterior probabilities
            posterior_probs = [prior * likelihood for prior, likelihood in zip(self.prior, x_likelihoods)]

            # Select the class label with the highest posterior probability
            predicted_class = np.argmax(posterior_probs)
            preds.append(predicted_class)
        ###########################################################################
        #                             END OF YOUR CODE                            #
        ###########################################################################
        return preds



def get_accuracy(model,X,y):
    ##num_correct = 0
    ##for i in range(len(X)):
    ##    pred = model.predict(X[i])
    ##    if pred == y[i]:
    ##      num_correct += 1
    ##return num_correct/len(X)
    num_correct = 0
    preds = model.predict(X)
    for p in range(len(preds)):
        if preds[p] == y[p]:
          num_correct += 1
    return num_correct/len(X) 

def model_evaluation(x_train, y_train, x_test, y_test, k, best_eta, best_eps):
    ''' 
    Read the full description of this function in the notebook.

    You should use visualization for self debugging using the provided
    visualization functions in the notebook.
    Make sure you return the accuracies according to the return dict.

    Parameters
    ----------
    x_train : array-like, shape = [n_train_examples, n_features]
      Training vectors, where n_examples is the number of examples and
      n_features is the number of features.
    y_train : array-like, shape = [n_train_examples]
      Target values.
    x_test : array-like, shape = [n_test_examples, n_features]
      Training vectors, where n_examples is the number of examples and
      n_features is the number of features.
    y_test : array-like, shape = [n_test_examples]
      Target values.
    k : Number of gaussians in each dimension
    best_eta : best eta from cv
    best_eps : best eta from cv
    ''' 

    lor_train_acc = None
    lor_test_acc = None
    bayes_train_acc = None
    bayes_test_acc = None

    ###########################################################################
    # TODO: Implement the function.                                           #
    ###########################################################################
    xtrain_1000 = x_train[:1000]
    ytrain_1000 = y_train[:1000]

    xtest_500 = x_test[:500]
    ytest_500 = y_test[:500]   

    logistic_reg = LogisticRegressionGD(best_eta,10000,best_eps) 
    naive_bayes = NaiveBayesGaussian(k)

    ModelEvaluation(xtrain_1000, ytrain_1000, xtest_500, ytest_500, logistic_reg, naive_bayes, "First 1000 training points and first 500 test points")
    ModelEvaluation(x_train, y_train, x_test, y_test, logistic_reg, naive_bayes, "All data points")
    print("Explanation: We are aware that our models are not ouputting the correct parameters and predictions but we are not sure where"
          + " our error in our code is. As a result our graphs ends up looking the same for both cases of data sets.")
    
    lor_train_acc = get_accuracy(logistic_reg, x_train, y_train)
    lor_test_acc  = get_accuracy(logistic_reg, x_test, y_test)

    bayes_train_acc = get_accuracy(naive_bayes, x_train, y_train)
    bayes_test_acc  = get_accuracy(naive_bayes, x_test, y_test)
    ###########################################################################
    #                             END OF YOUR CODE                            #
    ###########################################################################
    return {'lor_train_acc': lor_train_acc,
            'lor_test_acc': lor_test_acc,
            'bayes_train_acc': bayes_train_acc,
            'bayes_test_acc': bayes_test_acc}

def ModelEvaluation(x_train, y_train, x_test, y_test, LogisticRegressionModel, NaiveBayesModel, NumSetPoints):
    LogisticRegressionModel.fit(x_train, y_train)
    NaiveBayesModel.fit(x_train, y_train)
    iterations = np.arange(0 , len(LogisticRegressionModel.costs), 1)

    plot_decision_regions(x_train, y_train, LogisticRegressionModel, title = ("Logistic Regression - " + NumSetPoints))

    plot_decision_regions( x_train, y_train, NaiveBayesModel, title = ("Naive Bayes - " + NumSetPoints))
    plt.plot(iterations, LogisticRegressionModel.costs, color='r')
    plt.xlabel("iterations")
    plt.ylabel("costs")
    plt.title("Iterations Vs Costs of the Logistic Regression model - " + NumSetPoints)
    plt.show()
    

def plot_decision_regions(X, y, classifier, resolution=0.01, title=""):

    # setup marker generator and color map
    markers = ('.', '.')
    colors = ('blue', 'red')
    cmap = ListedColormap(colors[:len(np.unique(y))])
    # plot the decision surface
    x1_min, x1_max = X[:, 0].min() - 1, X[:, 0].max() + 1
    x2_min, x2_max = X[:, 1].min() - 1, X[:, 1].max() + 1
    xx1, xx2 = np.meshgrid(np.arange(x1_min, x1_max, resolution),
                           np.arange(x2_min, x2_max, resolution))
    Z = classifier.predict(np.array([xx1.ravel(), xx2.ravel()]).T)
    Z = np.reshape(Z, xx1.shape)
    plt.contourf(xx1, xx2, Z, alpha=0.3, cmap=cmap)
    plt.xlim(xx1.min(), xx1.max())
    plt.ylim(xx2.min(), xx2.max())

    for idx, cl in enumerate(np.unique(y)):
        plt.title(title)
        plt.scatter(x=X[y == cl, 0], 
                    y=X[y == cl, 1],
                    alpha=0.8, 
                    c=colors[idx],
                    marker=markers[idx], 
                    label=cl, 
                    edgecolor='black')
    plt.show()

def generate_datasets():
    from scipy.stats import multivariate_normal
    '''
    This function should have no input.
    It should generate the two dataset as described in the jupyter notebook,
    and return them according to the provided return dict.
    '''
    dataset_a_features = None
    dataset_a_labels = None
    dataset_b_features = None
    dataset_b_labels = None
    ###########################################################################
    # TODO: Implement the function.                                           #
    ###########################################################################
    mean1 = [2, 2, 2]
    cov1 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

    mean2 = [6, 6, 6]
    cov2 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

    samples_class1 = multivariate_normal.rvs(mean=mean1, cov=cov1, size=1000)
    samples_class2 = multivariate_normal.rvs(mean=mean2, cov=cov2, size=1000)


    label_class1 = np.zeros(1000)
    label_class2 = np.ones(1000)

    features = np.concatenate((samples_class1, samples_class2), axis=0)
    labels = np.concatenate((label_class1, label_class2), axis=0)

    shuffle_indices = np.random.permutation(len(features))

    dataset_a_features = features[shuffle_indices]
    dataset_a_labels = labels[shuffle_indices]
    plotGraph(dataset_a_features, dataset_a_labels, "Data Set A")


    #####################################



    mean1_2 = [2, 2, 2]
    cov1_2 = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

    mean2_2 = [6, 6, 6]
    cov2_2 = [[1, 0.5, 0], [0.5, 1, 0], [0, 0, 1]]


    samples_class1_2 = multivariate_normal.rvs(mean=mean1_2, cov=cov1_2, size=1000)

    samples_class2_2 = multivariate_normal.rvs(mean=mean2_2, cov=cov2_2, size=1000)

    label_class1_2 = np.zeros(1000)
    label_class2_2 = np.ones(1000)

    features_2 = np.concatenate((samples_class1_2, samples_class2_2), axis=0)
    labels_2 = np.concatenate((label_class1_2, label_class2_2), axis=0)

    shuffle_indices = np.random.permutation(len(features_2))
    dataset_b_features = features_2[shuffle_indices]
    dataset_b_labels = labels_2[shuffle_indices]
    plotGraph(dataset_b_features, dataset_b_labels, "Data Set B")
    ###########################################################################
    #                             END OF YOUR CODE                            #
    ###########################################################################
    return{'dataset_a_features': dataset_a_features,
           'dataset_a_labels': dataset_a_labels,
           'dataset_b_features': dataset_b_features,
           'dataset_b_labels': dataset_b_labels
           }
def plotGraph(data_set_features, data_set_labels, title):
    feature1 = data_set_features[:, 0]
    feature2 = data_set_features[:, 1]
    feature3 = data_set_features[:, 2]

    fig, axs = plt.subplots(1, 3, figsize=(15, 5))

    axs[0].scatter(feature1[data_set_labels==0], feature2[data_set_labels==0], label='Class 1')
    axs[0].scatter(feature1[data_set_labels==1], feature2[data_set_labels==1], label='Class 2')
    axs[0].set_xlabel('Feature 1')
    axs[0].set_ylabel('Feature 2')
    axs[0].set_title('Feature 1 vs Feature 2')
    axs[0].legend()
    
    axs[1].scatter(feature1[data_set_labels==0], feature3[data_set_labels==0], label='Class 1')
    axs[1].scatter(feature1[data_set_labels==1], feature3[data_set_labels==1], label='Class 2')
    axs[1].set_xlabel('Feature 1')
    axs[1].set_ylabel('Feature 3')
    axs[1].set_title('Feature 1 vs Feature 3')
    axs[1].legend()

    axs[2].scatter(feature2[data_set_labels==0], feature3[data_set_labels==0], label='Class 1')
    axs[2].scatter(feature2[data_set_labels==1], feature3[data_set_labels==1], label='Class 2')
    axs[2].set_xlabel('Feature 2')
    axs[2].set_ylabel('Feature 3')
    axs[2].set_title('Feature 2 vs Feature 3')
    axs[2].legend()

    fig.suptitle(title)
    fig.tight_layout()
    fig.show()

