library(lme4)
library(ggplot2)

#create some levels
levs <- as.factor(c("A","B","C","D","E"))

#set the factor means
f_means <- c(5,2,16,12,6)

# set individual as a factor

ind <- as.factor(paste("i",1:9,sep=""))

#Set individual effects

i_eff <- seq(-4,4,length=9)

#now let's simulate a repeated measure for each individuals
idf <- data.frame(matrix(0,ncol=3,nrow=45))
colnames(idf) <- c("size","ind","levs")
counter <- 1
for(i in 1:length(levs)){
  for(j in 1:length(ind)){
    idf$size[counter] <- rnorm(1,f_means[i]+i_eff[j],.3)
    idf$ind[counter] <- ind[j] 
    idf$levs[counter] <- levs[i]
    counter <- counter + 1
  }
}
idf$ind <- rep(ind,5)
idf$levs <- sort(rep(levs,9))

ggplot(idf,aes(x=levs,y=size,group=ind,colour=ind))+geom_point()+geom_path()


m3 <-lmer(size~levs - 1 +(1|ind), data=idf)


## Now let's randomize the individuals
idf_rand <- idf
for(i in 1:5){
  idf_rand$ind[idf_rand$levs==levs[i]]  <- sample(idf$ind[idf$levs==levs[i]],9,replace=F)
  
}

# here we can visualize the data and examine individual effects
ggplot(idf_rand,aes(x=levs,y=size,group=ind,colour=ind))+geom_point()+geom_path()

#Fit the model and then check the variance term
m4 <-lmer(size~levs - 1 +(1|ind), data=idf_rand)

qqnorm(m4, ~ranef(.))