#!/usr/bin/Rscript

#ATTEN - Clean up!

main <- function(train_file = '../Data/split1.csv', test_file = '../Data/split2.csv')
{
	args <- commandArgs(trailingOnly = TRUE)
	if(length(args) == 2)
	{
		train_file <- args[1]
		test_file <- args[2]
	}

	train <- ImportData(train_file)
	test <- ImportData(test_file) #Move below? ATTEN

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

	theta <- c(0,0,0,0)
	theta <- GradiantDescent(X,y,theta,0.001,150)
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
	X2$Actual <- y
	
	print(head(X2[X2$Prediction != X2$Actual,]))

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
	cat(sprintf('This model accurately predicts %.2f%% of the data in %s.\n',
		accuracy, test_file))


}

ImportData <- function(source)
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

	return(data)
}

ComputeCost <- function(X,y,theta)
#Pre-condition:
#Post-condition:
{
	h <- function(z) 1/(1+exp(-t(theta) %*% as.numeric(z)))[1,1]

	m <- dim(X)[1]
	J <- 0

	for(i in 1:m) J <- J - (y[i]*log(h(X[i,])) + (1-y[i])*log(1-h(X[i,])))/m

	return(J)
}

GradiantDescent <- function(X,y,theta,alpha,num_iters)
#Pre-condition: data is data frame with
{
	J_history <- c()

	start <- Sys.time()

	for(i in 1:num_iters)
	{
		h <- function(z) 1/(1+exp(-t(theta) %*% as.numeric(z)))[1,1]

		K <- apply(X,1,h) - y

		temp <- c()
		for(j in 1:dim(X)[2]) temp[j] <- theta[j] - alpha*sum(K*X[,j])

		theta <- temp
		J_history[i] = ComputeCost(X,y,theta)
	}

	end <- Sys.time()

	pdf('Cost.pdf')
	plot(J_history)
	dev.off()

	cat(sprintf('Gradiant Descent with %i steps took %.2f seconds (n=%i & j=%i).\n',
		num_iters, end-start,dim(X)[1], dim(X)[2]))

	return(theta)
}


main()