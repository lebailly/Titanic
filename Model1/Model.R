#!/usr/bin/env Rscript

main <- function()
{
	ImportData()

	theta <- TrainModel(train)
	print(theta)
	test <- MakePredictions(test,theta)

	ErrorAnalysis(test)
}

TrainModel <- function(train, lambda = 0)
{
	#scale is a vector containing scaling factors.
	scale <- c(Constant=1,Sex=max(train$Sex),Age=max(train$Age),
		Pclass=max(train$Pclass))
	train$Sex <- train$Sex/scale['Sex']
	train$Age <- train$Age/scale['Age']
	train$Pclass <- train$Pclass/scale['Pclass']

	X <- train[c('Constant','Sex','Age','Pclass')]
	y <- train$Survived
	cost <- MakeCostFunct(X,y,lambda)

	theta <- c(Constant=0,Sex=0,Age=0,Pclass=0)
	theta <- optim(theta,cost)$par #Find a way to cache this!
	
	theta/scale
}

MakePredictions <- function(test,theta)
{
	f <- function(z) 1/(1+exp(-(t(theta) %*% as.numeric(z))[1,1]))

	X <- test[c('Constant','Sex','Age','Pclass')]
	test$Psurvival <- apply(X,1,f)
	test$Prediction <-  as.numeric(test$Psurvival >= 0.5)

	test
}

ErrorAnalysis <- function(test)
{
	Incorrect <- test[test$Prediction != test$Survived,
		c('Psurvival','Prediction')]

	data <- merge(original_data, Incorrect, by='row.names')
	data$Row.names <- NULL
	
	write.csv(data,file='../Data/Incorrect.csv',row.names=F)

	accuracy <- 100*(1-sum(abs(test$Prediction-test$Survived))/nrow(test))
	cat(sprintf('This model accurately predicts %.2f%% of the data \n',accuracy))
}

ImportData <- function(source='../Data/train.csv',train_ratio=0.7)
#Pre-condition: source is a filename where the data is located
#Post-conditino: Reads data from source, labels males as 0 and females 1
{
	original_data <<- read.csv(source, stringsAsFactors=F)

	data <- original_data
	data <- data[c('Survived','Sex','Age','Pclass')]
	data <- na.omit(data) 

	data$Sex[data$Sex=='male'] <- 0
	data$Sex[data$Sex=='female'] <- 1
	data$Sex <- as.numeric(data$Sex)

	data$Constant <- 1

	train_max <- floor(nrow(data)*train_ratio)

	train <<- data[1:train_max,]
	test <<- data[(train_max+1):nrow(data),]
}

MakeCostFunct <- function(X,y,lambda=0)
{
	m <- nrow(X)

	function(theta)
	{
		h <- function(z) 1/(1+exp(-t(theta) %*% as.numeric(z)))[1,1]
		-sum(y*log(apply(X,1,h))+log(1-apply(X,1,h))*(1-y))/m+lambda*sum(theta[-1]^2)/(2*m)
	}
}

main()