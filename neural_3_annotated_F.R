
#These packages are required to run this simulation. 

library ("MASS")
library ("neuralnet")

#The following function generates an arbitrary correlation matrix.
#It uses matrix factorization (i.e. QR decomposition) to construct a positive 
#definite matrix. The eigen values for the matrix are randomly generated from 
#a uniform distribution but can be manually specified if desired. To do so, 
#when using the function, use the ev = n option in the function call to specify 
#the desired eigen values where n are the eigenvalues.

Posdef <- function (n, ev = runif(n, 0, 10)) 
	{
	Z <- matrix(ncol=n, rnorm(n^2))
		decomp <- qr(Z)
		Q <- qr.Q(decomp) 
		R <- qr.R(decomp)
		d <- diag(R)
		ph <- d / abs(d)
		O <- Q %*% diag(ph)
		Z <- t(O) %*% diag(ev) %*% O
		return(Z)
	}


#This portion of the code creates a NA-filled list. The purpose of this is to create an 
#R object that will be populated with results from the simulation. time = n should 
#denote the number of times that the simulation will run. In this case, there are 4 
#runs of the simulation so time = 4.

sim_output <- rep (NA, time = 4)

for (sim in 1:4){

#The initial portion of the function sets the boundaries of the simulated data. 
#Sigma describes the size of the correlation matrix. The size of the matrix will 
#determine the number of variables in the simulated dataset. 

#mu generates the means for the simulated variables. runif generates a random value 
#from a uniform distribution with a specified minimum and maximum value. Here the 
#minimum value is 0 and the maximum value is 10. Running this code will then generate
#11 random numbers between 0 and 10.

#Finally, data is the final simulated dataset. the mvnorm command generates values from
#a specified normal multivariate normal distribution. n specifies the number of observations
#mu is the means and of each variable and sigma is the correlation between each variable
#in the multivariate normal distribution.

Sigma <- Posdef(n = 11)
mu <-  runif(11,0,10) 
data <- as.data.frame(mvrnorm(n=1000, mu, Sigma))

#This section of the function sets a floor and ceiling on the simulated dataset.
#All simulated values less than 0 are set to 0. All simulated values greater than 10 are set to 10.

data[data <  0] <-  0 
data[data >  10] <-  10 


#Here a set of names are given to each variable. The first name in the list corresponds to 
#the first column in the generated dataset. The names of the variables can be anything.#

names(data) = c('criteria_1', 'criteria_2', 'criteria_3', 'criteria_4', 'criteria_5', 
		    'criteria_6', 'criteria_7', 'criteria_8', 'criteria_9', 'criteria_10',
		    'outcome')

#Finally, an ifelse statement is used to create a binary variable. Here this command states 
#that if a value is greater than 5, transform it to a 1, else change it to 0. This effectively 
#turns the continuous variable into a binary variable. 

data$outcome <- ifelse(data$outcome > 5, 1, 0)

#The next portion of the script uses the apply command to extract the min and max values.
#The purpose of this is to standardize the data using min-max standardization. The code 
#extracts the minimum and maximum value from each column in the simulated dataset. 
#Note: in the apply command the option 1 represents rows and 2 represents columns in a data
#set. Here we are extracting the min and max value of a column so we use 2.

data <- data[, sapply(data, is.numeric)]
maxValue <- as.numeric(apply (data, 2, max))
minValue <- as.numeric(apply (data, 2, min))

#This code rescales our data using the minimum and maximum values we extracted using the 
#scale command. Each column in the dataset is scaled such that the center is the minimum 
#value of that column and the scale is the maximum minus this minimum value. In other words,
#this puts the simulated data on a 0 to 1 scale. 

data_scaled <- as.data.frame(scale(data, center = minValue, 
scale = maxValue-minValue))

#This portion of the code splits the data into two sets. One set is a exploratory set 
#(called train here) and the other is the validation set (test). Currently it is divided 
#into two portions of size 600 and 400.

ind <- sample (1:nrow(data_scaled), 600)
train <- data_scaled[ind,]
test <- data_scaled[-ind,]

#Here the logistic regression model is specified. The data used to in the model is the
#exploratory dataset portion, not the validation portion. Finally, the extracted model from
#the exploratory dataset is used on the validation set with the predict command. 

model <- glm (formula = 
	outcome ~ criteria_1 + criteria_2 + criteria_3 + criteria_4 + criteria_5 +
		    criteria_6 + criteria_7 + criteria_8 + criteria_9 + criteria_10, 
	family = "binomial", 
	data = train)
predicted_model <- predict(model, test)

#The neural network is specified in this section of the code. Details regarding model
#specification and a description of options are found within the associated manuscript. 

neural_model <- neuralnet(formula = 
	outcome ~ criteria_1 + criteria_2 + criteria_3 + criteria_4 + criteria_5 +
		    criteria_6 + criteria_7 + criteria_8 + criteria_9 + criteria_10,
	hidden = c(10) ,
	threshold = 0.01, 
	stepmax = 1e+07,
	startweights = NULL,
	rep = 1,
	learningrate = NULL,
	algorithm = "rprop+",
	linear.output=FALSE, 
	data= train)

#This portion of the code will rescale values back to their original values. Note, in this
#simulation, the dependent variable is already on a 0 to 1 scale. As such, the min max scaling did 
#not transform the dependent variable. This section of the code is not needed to run a 
#the simulation as written but was included for those wanting to compare a neural network 
#to a linear regression model with a continuous variable. 

results <- compute (neural_model, test[,1:10])
results <- results$net.result*(max(data$outcome)-
min(data$outcome))+ min(data$outcome)
Values <- (test$outcome)*(max(data$outcome)- 
min(data$outcome)) + min(data$outcome)

#Here the mean square error is calculated for the logistic regression and the neural network.

MSE_neural_model <- sum((results - Values)^2)/nrow(test)
MSE_model <- sum((predicted_model - test$outcome)^2)/nrow(test)

#The output is given for each iteration of the simulation and the sim_output list is 
#populated with differences in mean square error. Positive values indicate that the neural 
#network had a lower mean square error than logistic regression. 

print(MSE_model - MSE_neural_model)
R1 <- (MSE_model - MSE_neural_model)
sim_output [sim] <- R1

}

#Here we transform the output from the simulation into a dataframe. 
#An additional step can be the write.csv command to create a .csv file.

sim_output <- as.data.frame (sim_output)
sim_output
