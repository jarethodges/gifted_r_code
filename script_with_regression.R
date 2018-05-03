## 1. Read in the data, set rule
money = read.csv("E:/PEIMS/stat/money.csv",header=TRUE)
names(money)[1]="DISTRICTNUMBER"
l = seq(from=2,to=2*length(money$DISTRICTNUMBER),by=2)
DIS = unlist(strsplit(as.character(money$DISTRICTNUMBER), split="'", fixed=TRUE))[l]
DIS = as.numeric(DIS)
money$DISTRICTNUMBER=DIS
money$rule = paste(money$DISTRICTNUMBER,money$year,sep="-")

race = read.csv("E:/PEIMS/stat/race.csv",header=TRUE)
names(race)[1]="DISTNAME"
names(race)[3]="DISTRICTNUMBER"
race$rule = paste(race$DISTRICTNUMBER,race$year,sep="-")

## 2. delete the cases that only money has.

dif1 = setdiff(money$rule,race$rule)
for(i in 1:length(dif1)){
  money = money[money$rule!=dif1[i],]
}


## 3. delete the cases that only race has.

dif2 = setdiff(race$rule,money$rule)
for(i in 1:length(dif2)){
  race = race[race$rule!=dif2[i],]
}

money = money[,1:249]
race = race[,1:23]
## 4. Merge the two dataset
money = money[order(money$DISTRICTNUMBER,money$year),]
race = race[order(race$DISTRICTNUMBER,race$year),]
row.names(money)=NULL
row.names(race)=NULL

master_count_money = cbind(money,race)

## 5. save the data.frame as a .csv file

write.csv(master_count_money,file="E:/PEIMS/stat/mcf.csv")

## set up regression for data analysis

setwd ("C:\\Users\\Jaret Hodges\\Desktop\\stat")

data = read.csv("Master_T_Tot.csv")

options(show.signif.stars=F) 
model1 = lm(formula =  G_T_Total ~. , data=data)
summary (model1)
summary.aov (model1)

fix (data)

str (data)

cor (data)

summary (data)

pairs (data)

data = read.csv("Master_T_Tot.csv")
options(show.signif.stars=F) 
model1 = lm(formula = G_T_Total ~ year + Total_Funds + T_Funds_Per_Stud + Tax_Revenue + State_Revenue + T_Capitol_Exp + Debt_Exp + T_Operating_Exp + T_GT_Exp + Funds_Per_St + gifted_AA + gifted_H + Total_Students + gifted_W + gifted_AZN + total_W + Percentage_Gifted + at_risk_AA + OR_NT_GIFTED + OR_NT_At_RISK, data=data)
summary (model1)
summary.aov (model1)

data = read.csv("Master_Trim.csv")
options(show.signif.stars=F) 
model1 = lm(formula =  T_GT_Exp ~ year + Total_Funds + GT_Fund_Perc + T_Funds_Per_Stud + Tax_Revenue + State_Revenue + T_Capitol_Exp + Debt_Exp + T_Operating_Exp + Funds_Per_St + gifted_AA + gifted_H + Total_Students + gifted_W + gifted_AZN + total_W + Percentage_Gifted + at_risk_AA + OR_NT_GIFTED + OR_NT_At_RISK, data=data)
summary (model1)
summary.aov (model1)


