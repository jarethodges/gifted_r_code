install.packages ("polycor")


setwd ("C:\\Users\\jaret\\Desktop\\stat")

data = read.csv("MCA.csv")
csv 
data1 = na.omit(data)

data2 <- sapply(data1, as.factor)

library (polycor)

het.data = hetcor(data1)$cor

het.data

fa = factanal(covmat = het.data, factors = 2, rotation = "varimax")

fa

library(psych)
fa.2 <- fa(r = het.data, nfactors = 4, n.obs = nrow(data1), rotate = "varimax")

fa.2

library (lavaan)

View (data1)

model <- '
lv1 =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8
lv2 =~ I9 + I10 + I11 + I12 + I13 + I14 + I15 + I16
lv3 =~ I18 + I19 + I20 + I21 + I22 + I23 + I24
lv4 =~ I25 + I26 + I27 + I28 + I29 + I30 + I31
'


fit <-cfa (model, data= data1)

summary (fit, fit.measures = TRUE)


model <- '
lv1 =~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 +
I25 + I26 + I27 + I28 + I29 + I30 + I31
lv2 =~ I9 + I10 + I11 + I12 + I13 + I14 + I15 + I16 + I17
lv3 =~ I18 + I19 + I20 + I21 + I22 + I23 + I24
'

fit <-cfa (model, data= data1)
fit
summary (fit, fit.measures = TRUE)


mplus2lavaan("soo.inp")

