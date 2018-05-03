library ("MASS")
library ("neuralnet")
set.seed(9001)

Variable1 <- rnorm(n = 500, mean = 26, sd = 5)
Variable2 <- rnorm(n = 500, mean = 600, sd = 100)
Variable3 <- rnorm(n = 500, mean = 115, sd = 15)
group <- 1 
weight <- ((.3*(Variable1/36))+(.3*(Variable2/800))+(.3*(Variable3/145)))
pt1 <- cbind (group, Variable1, Variable2, Variable3, weight)

Variable1 <- rnorm(n = 500, mean = 21, sd = 5)
Variable2 <- rnorm(n = 500, mean = 500, sd = 100)
Variable3 <- rnorm(n = 500, mean = 100, sd = 15)
group <- 0
weight <- ((.3*(Variable1/36))+(.3*(Variable2/800))+(.3*(Variable3/145)))
pt2  <- cbind (group, Variable1, Variable2, Variable3, weight)

pt3 <- as.data.frame(rbind (pt1, pt2))
Outcome <- rbinom (n = 1000, size = 1, prob = pt3$weight)

Data_f <- as.data.frame (cbind(pt3, Outcome))
Data <- subset (Data_f, select = -weight)

data <- Data[, sapply(Data, is.numeric)]
maxValue <- as.numeric(apply (data, 2, max))
minValue <- as.numeric(apply (data, 2, min))

data_scaled <- as.data.frame(scale(data, center = minValue, 
    scale = maxValue-minValue))

k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
    ind <- sample (1:nrow(data_scaled),600)
    train <- data_scaled[ind,]
    test <- data_scaled[-ind,]
    neural_model <- neuralnet(formula = 
        Outcome ~ Variable1 + Variable2 + Variable3,
        hidden = c(2,2),
        threshold = 0.01,
	  stepmax = 1e+07, 
        rep = 1,
        learningrate = NULL,
        algorithm = "rprop+",
        linear.output=FALSE, 
        data= train)
    results <- compute (neural_model, test[2:4])

    results <- results$net.result*(max(data$Outcome)-
        min(data$Outcome))+ min(data$Outcome)
    Values <- (test$Outcome)*(max(data$Outcome)- 
        min(data$Outcome)) + min(data$Outcome)
    MSE_neural_model[i] <- sum((results - Values)^2)/nrow(test)

    pbar$step()
}


print(mean(MSE_neural_model))
print(MSE_neural_model)