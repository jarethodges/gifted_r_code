

1-(.5*pchisq(anova(model1,model2, refit=FALSE)$Chisq[[2]],df=2)+
   .5*pchisq(anova(model1,model2, refit=FALSE)$Chisq[[2]],df=1))