Advanced Machine Learning — AdaBoost
=====================================

A Jupyter notebook implementing the AdaBoost (Adaptive Boosting) ensemble
learning algorithm from scratch, with empirical comparison against scikit-learn.

Algorithm:
- AdaBoost trains a sequence of weak learners (decision stumps — single-feature
  threshold classifiers) iteratively.
- After each round, misclassified samples receive higher weights so the next
  learner focuses on correcting those mistakes.
- The final prediction is a weighted majority vote across all weak learners,
  where each learner's vote weight is proportional to its accuracy.

Experiments:
- Experiment 1 (linearly separable data): generated two classes cleanly
  separable by a straight line; AdaBoost should converge quickly.
- Experiment 2 (non-linearly separable data): generated classes with overlapping
  or curved decision boundaries; tests the algorithm's ability to approximate
  complex frontiers by composing many simple stumps.

Both experiments compare accuracy curves (training and test error vs. number
of boosting rounds) against scikit-learn's AdaBoostClassifier.

Language: Python
Files: AdaBoost.ipynb
Dependencies: NumPy, scikit-learn, matplotlib
