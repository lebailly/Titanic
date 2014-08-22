# Predict survival on the Titanic

## About

The sinking of the RMS Titanic is one of the most infamous shipwrecks in history.  On April 15, 1912, during her maiden voyage, the Titanic sank after colliding with an iceberg, killing 1502 out of 2224 passengers and crew. This sensational tragedy shocked the international community and led to better safety regulations for ships.

One of the reasons that the shipwreck led to such loss of life was that there were not enough lifeboats for the passengers and crew. Although there was some element of luck involved in surviving the sinking, some groups of people were more likely to survive than others, such as women, children, and the upper-class.

In this challenge, I look at what sorts of people were likely to survive. In particular, I use the tools of machine learning to predict which passengers survived the tragedy.

This project stems form a Kaggle compitation.  Details are available [here][1].

## Models

My first thought was to use age, gender, and passanger class as factors in decided the outcome of survival.  Since many of the life boats were loaded with "women and children first," it seems reasonable that age and gender would be important factors.  The passanger class seemed relevant as first class passengers were closer to the lifeboats than third class.  Also, the movie *Titanic* seemed to indicated third class passengers were initially prevent from boarding while first class passengers boarded (though who knows if this is accurate).

### Building a Logisitic Model from scratch

I decided first to try a logistic regression.  Having just completed Andrew Ng Coursera Course on [Machine Learning][2], I manually programed the algorithm.  To see this, run `./ManualModel.R grad_descent`, which uses gradient descent to optimize the cost function.  I also added an option to use R's built in function optimizer to optimize the cost function.  Running `./ManualModel.R optim` will use the built in `optim` function.  

These two optimization methods produce different results.  The builtin optim function performs slightly better (accurately predicting 80.93% of the test data, where the gradent descent method got 77.67%).  I am not sure if there is an error in my gradient descent, or if optim simply finds a better solution.

### Regularization and Learning Curves

Next I played around with regularizatoin.  It was simple to add a regularization term to the cost function.  However increase lambada (the amount of regularization) seemed to make the performance worse.  This is not surprising since we are using a linear decision boundary, and hence it is likely that this model is under-fitting the data.  To verify this I wrote `LearningCurve.R`, which confirmed that this model is under-fitting that data (see `Graphs/LearningCurves.pdf`).  Hence regulization won't help (as it's only useful for a model overfitting data).

### Using builtin training packages (such as `caret`)

After this I decided to try some more builtin functions via `caret` (since I don't want to program all my machine learnign algorithms from scratch).  Running this against the test data set gave the same results using the manual method with the `optim` method.  To run this, simply run `./RModels.R logistic omit`.  The omit simply means any entries with missing data are ommitted.

### Missing Data - Imputing Age

Up to this point, I simply had ignored missing data.  Any entry that didn't have either the age, gender, or passenger was ignored.  Perhaps my model would function better if I imputed the age.

We do have more information - perhaps some will help predict age for the missing data.  We have names for all the passengers, and these names include a tiltle (such as "Mr.", "Miss", and so forth).  For the passengers with known ages, I grouped them by title and comptued the average age for each title group.  Then, for each passenger with a missing age, I replaced their age with the approriate title group's mean age. 

Running `./RModels.R logistic impute_age` will impute the age data.  However, in the end this did not make much a difference, having an accuracy of 79.1% on the test set.

## Future Work

* How should I handel missing data?  I use a basic imputation method.  Other options?

* Add K-fold cross-validation ability.

* Ability to Randomize test/cv/train break down.

[1]: http://www.kaggle.com/c/titanic-gettingStarted
[2]: https://www.coursera.org/course/ml