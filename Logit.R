#----------------------------------------------------------------------------------------------
#install.packages("ResourceSelection")
#install.packages("lmtest")
#install.packages("pscl")
#install.packages("MKmisc")
#install.packages("aod")
#install.packages("caret")
#install.packages("ROCR")
#install.packages("SDMTools")

library(MASS)
library(ResourceSelection)
library(lmtest)
library(pscl)
library(MKmisc)
library(aod)

rm(list=ls())
data<-read.csv("C:\\Users\\cisadmin\\Documents\\Data\\simplestV2.csv",header=T)
reg<-data.frame(data)

reg$satCat<- factor(reg$satCat)

reg$satNurse<- factor(reg$satNurse)
reg$satDoc<- factor(reg$satDoc)
reg$doc1<- factor(reg$doc1)
reg$doc2<- factor(reg$doc2)
reg$nurse1<- factor(reg$nurse1)
reg$nurse2<- factor(reg$nurse2)
reg$nurse3<- factor(reg$nurse3)

#Getting a summary from the whole data
summary(reg)
#--------**-----------#
# In order to review all the relations between features and see any corellation between features

pairs(reg[, c("age", "doc1","nurse1","nurse2","nurse3","none")],reg$satCat)

#--------**-----------#
#contingency table 
xtabs(~doc1+age+satCat, data=reg)


#--------**-----------#
# SatCat feature is transformation of satisfaction measure to 0 and 1
# The satCat feature should be defined as categorical

#reg$satCat<- factor(reg$satCat)

#--------**-----------#
#devide data into train and test set

sub <- sample(nrow(reg), floor(nrow(reg) * 0.75))
train <- reg[sub, ]
test <- reg[-sub, ]

#--------**-----------#
#train the Logistic Regression Model according to the data

mylogit <- glm(satCat~doc1, data = train, family = "binomial",)


#--------**-----------#
#Test the model on the test data set

logitHat <- predict(mylogit, newdata=test, type="response")


#--------**-----------#
#AIC Measure: Akaike Information Criteria, Analogous of adjusted R squared(The Lower the better)
#Null Deviance (the lower the better) This measure does not change at all as the model changes
#Residual Deviance (The lower the better) This measure changes a little bit as the model changes.
#This measure improves as the model improves but not that much.
 
summary(mylogit)

#--------**-----------#
#ROC Curve ( Reciever Operating Characteristic) 
#The more the area under the curve the better the model
#Conclusion: The value for area under the curve does not change much as we change the model
#Not the best measurement for validation

library(ROCR)
ROCRpred <- prediction(logitHat, test$satCat)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#--------**-----------#
#Hosmer lemeshow test
#Hosmer Lameshow test of goodness
# H0: The models does not need interaction and non-linearity
# H1: The models needs interaction and non-linearity
#First of all should not change the satCat feature into categorical one when using this measurement.
#This measurment should have worked well but did not show any proof 
#One of the good measurments

HLgof.test(fit = fitted(mylogit), obs =train$satCat)
hl <- hoslem.test(train$satCat,fitted(mylogit) ,g=10)
hl

#--------**-----------#
#Psuedo R squared is
#This value should be near 1 for good models where this is no where near it
#It improves as the model improves but not any where close to one

pR2(mylogit)

#--------**-----------#
#Methods to compare two models
#The likelihood ratio test, comparing added features.
#H0 holds that the reduced model is true and does not need added complexity.
#A p-value for the overall model fit statistic that is less than 0.05 would compel us to reject the null hypothesis.
#Does not work very good for comparing 

mylogit1 <- glm(satCat~doc1+age, data = reg, family = "binomial")
mylogit2 <- glm(satCat~doc1, data = reg, family = "binomial")

anova(mylogit1,mylogit2, test ="Chisq")

lrtest(mylogit1,mylogit2)
#--------**-----------#
#Confusion matrix
library(caret)
#install.packages('e1071', dependencies=TRUE)

satCatTest<-test$satCat
LH<-ifelse(logitHat>=0.5,1,0)
LH1<-as.vector(LH)
confusionMatrix(LH1,satCatTest)
