#!/usr/bin/env Rscript

library(caret)

main <- function()
{
  args <- commandArgs(trailingOnly = TRUE)
  model = args[1]
  imput = args[2]
  
	ImportData(method=imput)

  if(model == 'logistic')
  {
    model <- train(Survived ~ ., method='glm', family='binomial', data = training)
    pred <- predict(model, testing)
    pred[pred < 0.5] <- 0
    pred[pred >= 0.5] <- 1
  }
  
  if(model == 'nn')
  {
    mode <- train(Survived ~ ., method='neuralnet', data = training)
    pred <- predict(model, testing)
  }
  
	print(confusionMatrix(pred, testing$Survived))
	which(pred != testing$Survived)
}

ImportData <- function(source='../Data/train.csv',train_ratio=0.7,method='omit')
#Pre-condition: source is a filename where the data is located
#Post-conditino: Reads data from source, labels males as 0 and females 1
{
	original_data <<- read.csv(source, stringsAsFactors=F)

	data <- original_data
  
  if(method == 'omit')
  {
    data <- data[c('Survived','Sex','Age','Pclass')]
    data <- na.omit(data)
  }
  else if(method == 'impute_age') #other way to predict age? Using sibsp or parch?
  {
    master_vector = grepl("Master.",data$Name, fixed=TRUE)
    miss_vector = grepl("Miss.", data$Name, fixed=TRUE)
    mrs_vector = grepl("Mrs.", data$Name, fixed=TRUE)
    mr_vector = grepl("Mr.", data$Name, fixed=TRUE)
    dr_vector = grepl("Dr.", data$Name, fixed=TRUE)
    
    master_age = round(mean(data$Age[master_vector], na.rm = TRUE))
    miss_age = round(mean(data$Age[miss_vector], na.rm = TRUE))
    mrs_age = round(mean(data$Age[mrs_vector], na.rm = TRUE))
    mr_age = round(mean(data$Age[mr_vector], na.rm = TRUE))
    dr_age = round(mean(data$Age[dr_vector], na.rm = TRUE))
    
    data[is.na(data$Age) & master_vector,]$Age <- master_age
    data[is.na(data$Age) & miss_vector,]$Age <- miss_age
    data[is.na(data$Age) & mrs_vector,]$Age <- mrs_age
    data[is.na(data$Age) & mr_vector,]$Age <- mr_age
    data[is.na(data$Age) & dr_vector,]$Age <- dr_age
    
    data <- data[c('Survived','Sex','Age','Pclass')] 
  }
  
	train_max <- floor(nrow(data)*train_ratio)

	training <<- data[1:train_max,]
	testing <<- data[(train_max+1):nrow(data),]
  #Use built in caret funct to split data (look at DS Specilizaiotn for name)
}

main()