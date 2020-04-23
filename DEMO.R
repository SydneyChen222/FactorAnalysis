setwd("E:/591 Marketing Analysis/591marketingA")
project=read.csv("Airport_Quarterly_Passenger_Survey.csv", header=TRUE)
install.packages("GPArotation")
install.packages("nFactors")
library(nFactors)
library(psych)
library(GPArotation)
head(project)
dim(project)
summary(project)

df=project[,4:36]
df[is.na(df)] <- 0
head(df)
corr.test(as.matrix(project[,4:36]))
nScree(df) #estimate the number of factors from scree tests.
eigen(cor(df))
factanal(df, factors=11) 
factanal(df, factors=11,rotation="varimax")
(df_res<-factanal(df, factors=11, rotation="oblimin"))
factor_res=factanal(df, factors=11,rotation="varimax", scores="Bartlett")
dim(factor_res$scores)
head(factor_res$scores)
factor_res
X=as.matrix(df)
fX=as.matrix(factor_res$scores) #IVs from factor scores.
Y=project[,37]
summary(lm(Y~X))
summary(lm(Y~fX))
BIC(lm(Y~X))
BIC(lm(Y~fX))