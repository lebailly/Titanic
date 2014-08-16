#!/usr/bin/env Rscript

library(caret)

main <- function()
{
	ImportData()

	model <- train(Survived ~ ., method='glm', family='binomial', data = training)
	pred <- predict(model, testing)
	pred[pred < 0.5] <- 0
	pred[pred >= 0.5] <- 1

	print(confusionMatrix(pred, testing$Survived))
	which(pred != testing$Survived)
}

ImportData <- function(source='../Data/train.csv',train_ratio=0.7)
#Pre-condition: source is a filename where the data is located
#Post-conditino: Reads data from source, labels males as 0 and females 1
{
	original_data <<- read.csv(source, stringsAsFactors=F)

	data <- original_data
	data <- data[c('Survived','Sex','Age','Pclass')]
	data <- na.omit(data)

	train_max <- floor(nrow(data)*train_ratio)

	training <<- data[1:train_max,]
	testing <<- data[(train_max+1):nrow(data),]
}

main()