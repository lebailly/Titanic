#!/usr/bin/Rscript

main <- function()
{
	ImportData(train_ratio=0.6,val_ratio=0.2)

	scale <- ComputeScale(train)
	train <- ScaleFeatures(train, scale)

	X <- train[c('Constant','Sex','Age','Pclass')]
	y <- train$Survived

	cost <- CreateCost(X,y)
	theta <- c(1.678795,  2.592295, -2.331269, -2.994373) #rep(0,4)
	#theta <- optim(theta,cost)$par
	#Find better way to cache this!
	cost(theta)

	#My gradiant decent solution found optimial theta to be 
	#c(-0.3093811,  2.3394740, -0.5074464, -1.2551855)
	
	names(theta) <- c('Constant','Sex','Age','Pclass')

	print(list(Theta=theta, Scale=scale))

	############  MAKES PREDICTIONS ##################

	test <- ScaleFeatures(test, scale)

	X2 <- test[c('Constant','Sex','Age','Pclass')]
	y2 <- test$Survived

	f <- function(z) (t(theta) %*% as.numeric(z))[1,1]
	g <- function(z) 1/(1+exp(-z))

	predictions <- apply(X2,1,f) #ATTEN - Clean up, make new data frame, don't use X2
	probs <- sapply(predictions,g) #Computes survival probabilites based on model
	predictions[predictions>=0] <- 1
	predictions[predictions<0] <- 0

	X2$Psurvival <- probs
	X2$Prediction <- predictions
	X2$Actual <- y2 #ATTEN - Double check this in previous versions.
	
	X2 <- UnscaleFeatures(X2, scale)
	X2$Constant <- NULL

	print(X2[X2$Prediction != X2$Actual,])

	pdf('Incorrect.pdf')
	hist(X2[X2$Prediction != X2$Actual,]$Psurvival)
	dev.off()

	pdf('Correct.pdf')
	hist(X2[X2$Prediction == X2$Actual,]$Psurvival)
	dev.off()

	pdf('All.pdf')
	hist(X2$Psurvival)
	dev.off()

	accuracy <- 100*(1-sum(abs(predictions-y2))/length(y2))
	cat(sprintf('This model accurately predicts %.2f%% of the data \n',accuracy))
}

ComputeScale <- function(data)
#Computes the scale for features.
{
	scale <- c(Constant=1,Sex=max(data[,'Sex']),Age=max(data[,'Age'] ),
			Pclass=max(data[,'Pclass']))
}

ScaleFeatures <- function(data, scale)
{
	data$Sex <- data$Sex/scale['Sex']
	data$Age <- data$Age/scale['Age']
	data$Pclass <- data$Pclass/scale['Pclass']

	data
}

UnscaleFeatures <- function(data, scale)
{
	data$Sex <- data$Sex*scale['Sex']
	data$Age <- data$Age*scale['Age']
	data$Pclass <- data$Pclass*scale['Pclass']

	data
}

ImportData <- function(source='../Data/train.csv',train_ratio=0.6,val_ratio=0.2)
#Pre-condition: source is a filename where the data is located
#Post-conditino: Reads data from source, labels males as 0 and females 1
{
	data <- read.csv(source) #ATTEN - Specify column type? Use stringsAsFactors to prevent sex as factor
	data <- data[c('Survived', 'Sex','Age','Pclass')]
	data <- na.omit(data) 

	#ATTEN - use complete.cases?

	#ATTEN - Use unclass on factor varable sex?
	data$Sex <- as.character(data$Sex)
	data$Sex[data$Sex=='male'] <- 0
	data$Sex[data$Sex=='female'] <- 1
	data$Sex <- as.numeric(data$Sex)

	data$Constant <- 1

	train_max <- floor(nrow(data)*train_ratio)
	test_max <- floor(nrow(data)*(train_ratio+val_ratio))

	train <<- data[1:train_max,]
	val <<- data[(train_max+1):test_max,]
	test <<- data[(test_max+1):nrow(data),] #ATTEN - something other than nrow?
}

CreateCost <- function(X,y)
{
	function(theta)
	{
		h <- function(z) 1/(1+exp(-t(theta) %*% as.numeric(z)))[1,1]
		m <- dim(X)[1]

		-sum(y*log(apply(X,1,h)) + log(1-apply(X,1,h))*(1-y))/m
	}

}

main()