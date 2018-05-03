
install.package ("MASS")
install.package ("grid")
install.package ("neuralnet")

library ("MASS")
library ("neuralnet")

set.seed(1234)

n <- 500
IQ <- rnorm(n, mean = 115, sd = 15)
ACT <- IQ*.2261 + rnorm(n, mean = 0, sd = 5)
SAT <- IQ*5.2174 + rnorm(n, mean = 0, sd = 100)
group <- 1 
weight <- ((.3*(ACT/36))+(.3*(SAT/800))+(.3*(IQ/145)))

pt1 <- cbind (group, ACT, SAT, IQ, weight)

n <- 500
IQ <- rnorm(n, mean = 100, sd = 15)
ACT <- IQ*.21 + rnorm(n, mean = 0, sd = 5)
SAT <- IQ*5 + rnorm(n, mean = 0, sd = 100)
group <- 0
weight <- ((.3*(ACT/36))+(.3*(SAT/800))+(.3*(IQ/145)))

pt2  <- cbind (group, ACT, SAT, IQ, weight)

pt3 <- as.data.frame(rbind (pt1, pt2))

Passing <- rbinom (n = 1000, size = 1, prob = pt3$weight)

Data_f <- as.data.frame (cbind(pt3, Passing))
Data <- subset (Data_f, select = -weight)
View (Data)

data <- Data[, sapply(Data, is.numeric)]
maxValue <- as.numeric(apply (data, 2, max))
minValue <- as.numeric(apply (data, 2, min))

data_scaled <- as.data.frame(scale(data, center = minValue, 
scale = maxValue-minValue))

ind <- sample (1:nrow(data_scaled), 600)
train <- data_scaled[ind,]
test <- data_scaled[-ind,]


model <- glm (formula = 
	Passing ~ ACT + SAT + IQ, 
	family = "binomial", 
	data = train)

summary (model)
predicted_model <- predict(model, test)

neural_model <- neuralnet(formula = 
	Passing ~  ACT + SAT + IQ,
	hidden = c(2,2) ,
	threshold = 0.01, 
	stepmax = 1e+07,
	startweights = NULL,
	rep = 1,
	learningrate = NULL,
	algorithm = "rprop+",
	linear.output=FALSE, 
	data= train)

plot (neural_model)

plot (neural_model)
results <- compute (neural_model, test[2:4])


results <- results$net.result*(max(data$Passing)-
min(data$Passing))+ min(data$Passing)
Values <- (test$Passing)*(max(data$Passing)- 
min(data$Passing)) + min(data$Passing)


MSE_nueral_model <- sum((results - Values)^2)/nrow(test)
MSE_model <- sum((predicted_model - test$Passing)^2)/nrow(test)

print(MSE_model - MSE_nueral_model)

print(results)


library(boot)
model <- glm(formula = 
	Passing ~ ACT + SAT + IQ, 
	family = "binomial", 
	data = data)
MSE_boot_model <- cv.glm(data,model,K=10)$delta[1]

set.seed(1234)
MSE_nueral_model <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
	ind <- sample (1:nrow(data_scaled),600)
	train <- data_scaled[ind,]
	test <- data_scaled[-ind,]
	neural_model <- neuralnet(formula = 
		Passing ~  ACT + SAT + IQ,
		hidden = c(2,2) ,
		threshold = 0.01, 
		rep = 1,
		learningrate = NULL,
		algorithm = "rprop+",
		linear.output=FALSE, 
		data= train)
	results <- compute (neural_model, test[2:4])

	results <- results$net.result*(max(data$Passing)-
		min(data$Passing))+ min(data$Passing)
	Values <- (test$Passing)*(max(data$Passing)- 
		min(data$Passing)) + min(data$Passing)
	MSE_neural_model[i] <- sum((results - Values)^2)/nrow(test)
	
	pbar$step()
}


####################Annotated code of the above######################


#The necessary packages are neuralnet, MASS, and grid. Neuralnet relies on the 
framework of MASS and grid#

install.package ("MASS")
install.package ("grid")
install.package ("neuralnet")

library ("MASS")
library ("neuralnet")

#this command brings up the documentation for the neuralnet package#

?neuralnet

#Setting a seed allows replication between users. Neuralnets run on stochastic 
processes meaning that without a seed, the results will be different every time#

set.seed(123)


#Data simulation is done by creating two bins and three distributions in each bin. 
Bin 1 has a mean one standard deviation greater than Bin 2. The purpose of doing 
this is to increase time to convergence. In other words, the increase in mean and 
the use of probability weights will create an artificial linear association within
the dataset. This will shorten run time.#

The three variables are modeled after the reported distributions for the ACT, SAT, 
and IQ score. The probability weight is calculated by taking dividing each randomly
generated score by three standard deviations above the associated means for bin 2. 
Though it is true that the real SAT and ACT have ceilings, not including them 
in this simulation reduces run time. The weighting is structured in such a way 
that the probability of obtaining a weight > 1 is relatively low. This simulation 
will produce a probability weight greater than 1 in roughly every ~37,000 simulations. #

The commands used first create three variables for a bin, then the associated weight. 
The rnorm command samples from the normal distribution with mean and standard deviation
n times as specified by the user. #

The three variables are bound into a matrix together with the cbind command. #

This process is repeated for bin 2 and rbind is used to put bin 1 and bin 2 into 
the same matrix. Then the calculated weights are used to sample from a binomial 
distribution to produce the dependent variable (passing = 1, 
failing = 0). Finally cbind is used to bind the passing varible vector to the 
matrix with bin 1 and bin 2. The as.data.frame command is used to transform the 
matrix into a dataframe. Last, the subset command is used to remove the weighting 
vector from the dataframe as it will not be used in the analysis.#

ACT <- rnorm(n = 500, mean = 26, sd = 5)
SAT <- rnorm(n = 500, mean = 600, sd = 100)
IQ <- rnorm(n = 500, mean = 115, sd = 15)
group <- 1 
weight <- ((.3*(ACT/36))+(.3*(SAT/800))+(.3*(IQ/145)))

pt1 <- cbind (group, ACT, SAT, IQ, weight)

ACT <- rnorm(n = 500, mean = 21, sd = 5)
SAT <- rnorm(n = 500, mean = 500, sd = 100)
IQ <- rnorm(n = 500, mean = 100, sd = 15)
group <- 0
weight <- ((.3*(ACT/36))+(.3*(SAT/800))+(.3*(IQ/145)))

pt2  <- cbind (group, ACT, SAT, IQ, weight)

pt3 <- as.data.frame(rbind (pt1, pt2))

Passing <- rbinom (n = 1000, size = 1, prob = pt3$weight)

Data_f <- as.data.frame (cbind(pt3, Passing))
Data <- subset (Data_f, select = -weight)
View (Data)

#The section of code scales the data. The purpose of scaling is to concentrate 
the features of the dataset. This in turns speeds up time to convergence. In short, 
normalization helps the gradient algorithm move towards convergence (i.e. get to 
where it needs to be). The apply command tells R to do a given function on your 
dataframe. In this case, the max and min values of columns are obtained (if the 
2 is changed to a 1, it will take the max and min values of rows) to perform 
minmax scaling (i.e. normalization).#

data <- Data[, sapply(Data, is.numeric)]
maxValue <- as.numeric(apply (data, 2, max))
minValue <- as.numeric(apply (data, 2, min))

data_scaled <- as.data.frame(scale(data, center = minValue, 
scale = maxValue-minValue))

#This section of code seperates the data into a training and a test dataset. The
training dataset is set to 60% of the simulated sample with the testing set the remaining
40%. This is done through using the sample command.  600 samples are taken from the 
simulated data and allotted to the training dataset, the remaining are used for the
testing dataset.#

ind <- sample (1:nrow(data_scaled), 600)
train <- data_scaled[ind,]
test <- data_scaled[-ind,]


#This secion of code describes a binomial regression. This is used to compare the 
performance of the neural net. The datau uses the training data set and then based
on the calculated coefficients, makes prediction on the test dataset. #

model <- glm (formula = 
	Passing ~ ACT + SAT + IQ, 
	family = "binomial", 
	data = train)

summary (model)
predicted_model <- predict(model, test)


#This section of code describes the neural network. Important options-
1. hidden = the number of nodes. More layers can be added in the 
following manner: c[X, X, X] where each value corresponds to its associated layer. The
default layer is one. 
2. Threshold describes the threshold for the derivative of the error function. Increase 
this to gain a faster convergence speed but at the loss of predicative accuracy of your 
model. 
3. linear.output = FALSE. False option is used for classifiers, true is for regression. 
In this example, we are classifying students as passing or not passing, so the option 
is FALSE.
The compute command calculates the predicted values given by the neural net on the training
data. Test[2:4] is the independent variable column vectors in the data.frame. In this form, 
the data is still scaled so not interpretable#

nueral_model <- neuralnet(formula = 
	Passing ~  ACT + SAT + IQ,
	hidden = 4 ,
	threshold = 0.01, 
	linear.output=FALSE, 
	data= train)

plot (nueral_model)
results <- compute (nueral_model, test[2:4])


#This secion of the code rescale the data back up. Since the outcome variable is binary, 
this section of code is redundant. This reverses the scaling done above#

results <- results$net.result*(max(data$Passing)-
min(data$Passing))+ min(data$Passing)
Values <- (test$Passing)*(max(data$Passing)- 
min(data$Passing)) + min(data$Passing)

#Finally, we compare the accuracy of the neural net to the logistic regression. This
code calculates the mean square error (MSE) for both models and then prints them.#

MSE_nueral_model <- sum((results - Values)^2)/nrow(test)
MSE_model <- sum((predicted_model - test$Passing)^2)/nrow(test)

print(paste(MSE_nueral_model,MSE_model))

#If you wish to see what the neural net predicted, you can use this code#
print(results)




plot(test$Passing, results, col='blue', main= 'real vs predicted',
pch=1, cex= 0.9, type = "p", xlab = "actual", ylab= "predicted")
abline (0,1,col="black")


rm(list = ls())

