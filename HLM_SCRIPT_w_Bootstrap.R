
setwd ("C:\\Users\\jaret\\Desktop\\stat")
data1 = read.csv ("Hope_MATH.csv")
data2 = read.csv ("Hope_ENG.csv")
dat = read.csv ("lvl2.1.csv")

View (data2)

dis = unique (data1$id)
dis2 = unique (data2$id)

d1 = setdiff(dis,dis2)

d2 = setdiff(dis,dis2)
length(d2)

data3 = merge (data1, data2, by=c("name", "wave"), all = TRUE )

View (data3)

data4 =rbind (data1, data2)

write.csv (data3, file = "1234.csv")

data4 = merge (data3, dat, by=c("name"), all = TRUE )

write.csv (data4, file = "12345.csv")

install.packages ("nlme")
install.packages ("lme4")
install.packages ("optimx")
install.packages ("nloptr")
install.packages ("ICC")
install.packages ("lavaan")
install.packages ("boot")
install.packages ("MASS")
install.packages ("pscl")
_________________________________________________________________


setwd ("C:\\Users\\jaret\\Desktop\\stat")
data = read.csv ("HLM_STUDY_trimmed.csv")
View (data)
library ("lme4")
library ("lavaan")
require ("lmerTest")
library ("boot")
library ("MASS")
library ("pscl")

eth <- as.factor (data$eth)
DIST <- as.factor (data$DIST)
HOPE_N <- (data$HOPE - 1)

model_e = lm (formula = score ~ wave, data)
BIC (model_e)

model_1 = lm (formula = score ~ camp + nocamp + wave, data) 
BIC (model_1)

model_2 = lm (formula = score ~ camp + nocamp + GEND + wave, data)
BIC (model_2)

model_3 = lm (formula = score ~ camp + nocamp + GEND + Rural + wave, data)
BIC (model_3)

model_4 = model = lmer(formula = score ~ (camp + nocamp + GEND + wave) + 
(1 | name) + (0+ wave | name), data)
BIC (model_4)

model_5 = lmer(formula = score ~ (camp + nocamp + GEND + Rural + eth + wave 
+ camp:wave + nocamp:wave) + 
(1 | name) + (0 + wave | name), data)
BIC (model_5)

model_6 = lmer(formula = score ~ (camp + nocamp + GEND + Rural + eth + wave 
+ camp:wave + nocamp:wave) + ( wave | name), data)
BIC (model_6)

model_7 = lmer(formula = score ~ (camp + GEND + Rural + eth + wave + after.x
+ camp:wave) + (wave:camp |name), data, method = REML)
BIC (model_7)

anova (model_6, model_7)

summary (model_6)

View (data)


model_6 = lmer(formula = score ~ (camp + nocamp + GEND + Rural + eth + wave 
+ camp:wave + nocamp:wave) + ( camp| name), data)
BIC (model_6)


1-var(residuals(model_6))/(var(model.response(model.frame(model_6))))


##boot strategy 1##

boot.fn <- function(data, indices){
 data <- data[indices, ]
 mod <- lmer(formula = score ~ (camp + nocamp + GEND + Rural + eth + wave 
+ camp:wave + nocamp:wave) + ( wave | name), data)
 fixef(mod)
 }

set.seed(123456)
Out <- boot(data=data, statistic=boot.fn, R=1000)
Out

##boot strategy2##


original.estimates <- as.vector(t(do.call(rbind, coef(summary(model_6)))[, 1:8]))

n.sim <- 2000

store.matrix <- matrix(NA, nrow=n.sim, ncol=12)

set.seed(123)

for(i in 1:n.sim) {


  data.new <- data[sample(1:dim(data)[1], dim(data)[1], replace=TRUE),]


  m <- zeroinfl(formula = score ~ (camp + nocamp + GEND + Rural + eth + wave 
+ camp:wave + nocamp:wave) + ( wave | name),
                data = data.new, dist = "negbin",
                start = list(count = c(1.3711, -1.5152, 0.879),
                             zero = c(1.6028, -1.6663)))

  
  store.matrix[i, ] <- as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}



boot.means <- colMeans(store.matrix, na.rm=T)

boot.medians <- apply(store.matrix,2,median, na.rm=T)

boot.sds <- apply(store.matrix,2,sd, na.rm=T)


boot.bias <- colMeans(store.matrix, na.rm=T) - original.estimates


conf.mat <- matrix(apply(store.matrix, 2 ,quantile, c(0.025, 0.975), na.rm=T),
ncol=2, byrow=TRUE)
colnames(conf.mat) <- c("95%-CI Lower", "95%-CI Upper")


####################################################


View (data)

model = lmer(formula = score ~ (camp + nocamp + GEND + Rural + wave 
+ camp:wave + nocamp:wave) + 
(1 | name) + (0 + wave | name), data)
summary (model)

model = lmer(formula = score ~ (camp + nocamp + GEND + Rural + wave 
+ camp:wave + nocamp:wave + camp:Rural) + 
(1 | name) + (0 + wave | name), data)
summary (model)

BIC (model)