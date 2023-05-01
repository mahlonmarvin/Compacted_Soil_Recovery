
library(ISLR)
library(neuralnet)
library(NeuralNetTools)
library(tidyverse)
library(broom)
library(caret)
library(olsrr)
library(car)

agxc <- read.csv("agx.csv", stringsAsFactors = TRUE)
df = subset(agxc, select = -c(Recovery))
View(df)

str(agxc)
View(agxc)

summary(agxc$ST)

pairs(agxc, upper.panel = NULL)


par = (mfrow = c(2,3))

scatter.smooth(agxc$ST, agxc$Y)
scatter.smooth(agxc$OMT, agxc$Y)
scatter.smooth(agxc$CPL, agxc$Y)
scatter.smooth(agxc$OML, agxc$Y)
scatter.smooth(agxc$MCL, agxc$Y)

#correlating the data to view their relationship
cor(agxc[C("ST","OMT","CPL","OML","MCL", "Y")])

omit()

f = formula(Y ~., ST+OMT+CPL+OML+MCL)
MODEL = lm(f, data = agxc)

MODEL

summary(MODEL)
View(MODEL$fitted.values)
scatter.smooth(MODEL$fitted.values)
residual = MODEL$residuals
plot(residual)
fitted_values = MODEL$fitted.values
hist(fitted_values)

Ypredicted = predict(MODEL,agxc)  #model prediction

Ymeasured = agxc$Y
scatter.smooth(Ymeasured,Ypredicted, )
data.frame(R2 = R2(Ymeasured, Ypredicted))


View(Ypredicted)

##########################################################################
#POLYNOMIAL REGRESSION FOR SECOND ORDER
########################################################################## 
agxc2 = read.csv("agx2.csv", header = TRUE)
View(agxc2)

pairs(agxc2, upper.panel = NULL)

df = subset(agxc2, select = -c(Y))
View(df)
f = formula(Y ~., X1+X2+X3+X4+X5+poly(X1,2)+poly(X2,2)+poly(X3,2)+poly(X4,2)+poly(X5,2)+
              
              poly(X1X2)+poly(X1X3)+poly(X1X4)+poly(X1X5)+poly(X2X3)+poly(X2X4)+poly(X2X5)+poly(X3X4)+
              
              poly(X3X5)+poly(X4X5), X1:X2:X3:X4:X5:X1X2:X1X5:X1X4:X1X5:X2X3:X2X4:X2X5:X3X4:X3X5:X4X5)

MODEL2 = lm(f, data = agxc2)

MODEL2

summary(MODEL2)

plot(MODEL2,6)

hist(MODEL2$residuals)

Ypredicted = predict(MODEL2, agxc2)   #model prediction for second order polynomial     
Ymeasured = agxc2$Y
scatter.smooth(Ymeasured, Ypredicted)
data.frame = (R2 = R2(Ypredicted, Ymeasured))
residual = MODEL2$residuals
fitted_model = MODEL2$residuals
hist(fitted_model)
plot(residual)

##########################################################################
#POLYNOMIAL REGRESSION FOR THIRD ORDER
########################################################################## 
agxc3 = read.csv("agx3.csv", header = TRUE)
View(agxc3)
pairs(agxc3, upper.panel = NULL)

f = formula(Y ~., X1+X2+X3+X4+X5+poly(X1,3)+poly(X2,3)+poly(X3,3)+poly(X4,3)+poly(X5,3)+poly(X1,2)+
              
              poly(X2,2)+poly(X3,2)+poly(X4,2)+poly(X5,2)+poly(X1X2)+poly(X1X3)+poly(X1X4)+poly(X1X5)+
              
              poly(X2X3)+poly(X2X4)+poly(X2X5)+poly(X3X4)+poly(X3X5)+poly(X4X5),poly(X1X2X3)+poly(X1X2X4)+
              
              poly(X1X2X5)+poly(X2X3X4)+poly(X2X3X5)+poly(X3X4X5), X1:X2:X3:X4:X5:X1X2:X1X5:X1X4:
              
              X1X5:X2X3:X2X4:X2X5:X3X4:X3X5:X4X5:X1X2X3:X1X2X4:X1X2X5:X2X3X4:X2X3X5:X3X4X5)

MODEL3 = lm(f, data = agxc3)

MODEL3

summary(MODEL3)

plot(MODEL3,3)
residual = MODEL3$residuals
plot(residual)

plot(MODEL3,1)
fitted_model = MODEL3$residuals
hist(fitted_model)
Ypredicted = predict(MODEL3, agxc3)
Ymeasured = agxc3$Y
scatter.smooth(Ymeasured, Ypredicted)
data.frame = (R2 = R2(Ypredicted, Ymeasured))

##########################################################################

##########################################################################           
# Next we use neural netwoks to predict the data, for the neural networks, we can
# create an additional table hhaving the cummulative average for each day

#First we normalize the given set of data
min <- apply(agxc[,1:6], 2, min)
max <- apply(agxc[1:6], 2, max)
scaled <- scale(agxc[1:6], center = min, scale = max - min)

tail(scaled)
View(scaled)

# next we divide the datasets into training and tsting datasest
index = sample(1:nrow(scaled), round(0.70*nrow(scaled)))
training = as.data.frame(scaled[index,])
testing = as.data.frame(scaled[-index,])

index2 = 

n = names(training)

f = formula(Y~., X1+X2+X3+X4+X5)

NNmodel = neuralnet(f, data = training, hidden = 5, linear.output = T)

plot(NNmodel)
View(NNmodel$net.result)
garson(NNmodel)

Y_NNpredicted = predict(NNmodel, testing)

View(Y_NNpredicted)
View(testing$Y)
Ymeasured = testing$Y
scatter.smooth(Ymeasured, Y_NNpredicted)
data.frame = (R2 = R2(Y_NNpredicted, Ymeasured))
View(NNpredict)


##########################################################################
#TIME SERIES ANALYSIS
##########################################################################  

agtime <- read.csv("TS.csv", stringsAsFactors = TRUE)

tail(agtime)
View(agtime)

summary(agxc$X1)
