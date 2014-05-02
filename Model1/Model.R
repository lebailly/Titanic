#!/usr/bin/Rscript

#ATTEN - Clean up!

main <- function()
{
	ImportData()

	#Feature Scaling (move to a function? Better implementatoin? ATTEN)
	scale <- c(1,1,max(train$Age), max(train$Pclass))
	names(scale) <- c('Constant','Sex','Age','Pclass') #DO this & above in one? ATTEN
	train$Age <- train$Age/scale['Age']
	train$Pclass <- train$Pclass/scale['Pclass']
	cat('Feature Scaling Factors:\n')
	print(scale)

	train$Constant <- 1
	X <- train[c('Constant','Sex','Age','Pclass')]
	y <- train$Survived

	cost <- CreateCost(X,y)
	theta <- c(1.678795,  2.592295, -2.331269, -2.994373) #Find better way to cache this! (should be rep(0,4))
	#My solution found optimial theta to be c(-0.3093811,  2.3394740, -0.5074464, -1.2551855)
	theta <- optim(theta,cost)$par

	names(theta) <- c('Constant','Sex','Age','Pclass')

	cat('Theta =\n')
	print(theta)

	############  MAKES PREDICTIONS ##################

	test$Age <- test$Age/scale['Age']
	test$Pclass <- test$Pclass/scale['Pclass']

	test$Constant <- 1
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
		J <- 0

		for(i in 1:m) J <- J - (y[i]*log(h(X[i,])) + (1-y[i])*log(1-h(X[i,])))/m

		return(J)
	}

}

main()