# Model 1

## Regularization

It was simple enough to add a regularization term to the cost function.  However increase lambada (the amount of regularization) seems to make the performance worse.  This is not surprising since we are using a linear decision boundary, and hence it is likely that this model is under-fitting the data.  To verify this I wrote a LearningCurves.R to show this.

`Model1a.R` does the same things as `Model.R`, but uses more built in functions.

## Future Work

* Add K-fold cross-validation ability.
* Ability to Randomize test/cv/train break down.