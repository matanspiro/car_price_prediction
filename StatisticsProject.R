library(MASS)
library(fitdistrplus)
library(usdm)
library(lmtest)
library(car)
library(ggplot2)
library(GGally)
library(gdata)

set.seed(321)

fullData<-read.csv(file.choose(), header = T) # df
fullData<-subset(fullData, select = -c(1, 2, 3)) # dropping irrelevant columns' IDs
numericData<-fullData[, 10:22] # numeric data's df
ggpairs(numericData) # visual correlations
correlations_VS_Y<-round(cor(numericData[, 1:13], fullData$price), 2)
correlations_VS_Y # low cor to y: carheight (will be dealt with later), stroke, compressionratio, peakrpm 
# plotting the variables which are prone to be disposed of:
cor.test(numericData[,4], fullData$price) # height. indeed low cor
cor.test(numericData[,8], fullData$price) # stroke indeed low cor
cor.test(numericData[,9], fullData$price) # compressionratio indeed low cor
cor.test(numericData[,11], fullData$price) # peakrpm indeed low cor

par(mfrow = c(2, 3))
plot(y = fullData$price, x = fullData$stroke,
     xlab = "stroke", frame = TRUE, col = 'blue' , pch = 20, ylab = "price")
abline(lm(price ~ stroke, data = fullData), col = "red")

plot(y = fullData$price, x = fullData$compressionratio,
     xlab = "compressionratio", frame = TRUE, col = 'blue' , pch = 20, ylab = "price")
abline(lm(price ~ compressionratio, data = fullData), col = "red")

plot(y = fullData$price, x = fullData$peakrpm,
     xlab = "peakrpm", frame = TRUE, col = 'blue' , pch = 20, ylab = "price")
abline(lm(price ~ peakrpm, data = fullData), col = "red")

model1<-lm(price ~ stroke, data = fullData, x = TRUE, y = TRUE)
summary(model1) # Radj = 0.0014

model2<-lm(price ~ compressionratio, data = fullData, x = TRUE, y = TRUE)
summary(model2) # Radj =~ 0

model3<-lm(price ~ peakrpm, data = fullData, x = TRUE, y = TRUE)
summary(model3) # Radj = 0.002

# Y doesn't seem to be affected almost at all by stroke, compressionratio and peakrpm - i'll  drop them

# cor between variables
correlations<-round(cor(numericData[, 1:13]), 2)
correlations # cor helps us analyze the effect of changes made in one variable over the other variable
cor.test(numericData[,5], numericData[,2]) # curbweight-length. indeed high cor
cor.test(numericData[,1], numericData[,2]) # wheelbase-length. indeed high cor
cor.test(numericData[,12], numericData[,10]) # citympg-HP. indeed high cor
cor.test(numericData[,13], numericData[,10]) # highwaympg-HP. indeed high cor
cor.test(numericData[,6], numericData[,10]) # enginesize-HP. indeed high cor
cor.test(numericData[,3], numericData[,2]) # width-length. indeed high cor

fullData<-fullData[,-c(10, 12, 14:15, 17:18, 20:22)]
numericData<-numericData[, -c(1, 3, 5:6, 8:9, 11:13)] 
# dropping curbweight since it's a derivative of width & length, and also wheelbase because it related to size
# engine size is also correlated with car's length & width, and horsepower. size has negative cor with highwaympg, width & length
# thus i dropped size, engine size which is correlated with HP, citympg because of its correlation with highwaympg, and dropped highwhy because of HP
ggpairs(numericData) # visual correlations
cor.test(numericData[,1], numericData[,2]) # length-height. indeed high cor
cor.test(numericData[,1], numericData[,3]) # length-borratio. indeed high cor
cor.test(numericData[,1], numericData[,4]) # length-HP. indeed high cor
ggpairs(numericData)
fullData<-fullData[,-c(10)]
numericData<-numericData[, -c(1)] 

covariances<-round(cov(numericData), 2)
covariances # cov is a measure of the relation between two variables

# factorization. combination of level 2 will be good enough
fullData$fueltype<-factor(fullData$fueltype)
fullData$fuelsystem<-factor(fullData$fuelsystem)
fullData$aspiration<-factor(fullData$aspiration)
fullData$doornumber<-factor(fullData$doornumber)
fullData$carbody<-factor(fullData$carbody)
fullData$drivewheel<-factor(fullData$drivewheel)
fullData$enginelocation<-factor(fullData$enginelocation)
fullData$cylindernumber<-factor(fullData$cylindernumber)
fullData$enginetype<-factor(fullData$enginetype)

fueltypeMap<-mapLevels(x = fullData$fueltype)
str(fueltypeMap)
fuelsystemMap<-mapLevels(x = fullData$fuelsystem)
str(fuelsystemMap)
aspirationMap<-mapLevels(x = fullData$aspiration)
str(aspirationMap)
doornumbereMap<-mapLevels(x = fullData$doornumber)
str(doornumbereMap)
carbodyMap<-mapLevels(x = fullData$carbody)
str(carbodyMap)
drivewheelMap<-mapLevels(x = fullData$drivewheel)
str(drivewheelMap)
enginelocationMap<-mapLevels(x = fullData$enginelocation)
str(enginelocationMap)
cylindernumberMap<-mapLevels(x = fullData$cylindernumber)
str(cylindernumberMap)
enginetypeMap<-mapLevels(x = fullData$enginetype)
str(enginetypeMap)

fullData$fueltype<-as.numeric(fullData$fueltype)
fullData$fuelsystem<-as.numeric(fullData$fuelsystem)
fullData$aspiration<-as.numeric(fullData$aspiration)
fullData$doornumber<-as.numeric(fullData$doornumber)
fullData$carbody<-as.numeric(fullData$carbody)
fullData$drivewheel<-as.numeric(fullData$drivewheel)
fullData$enginelocation<-as.numeric(fullData$enginelocation)
fullData$cylindernumber<-as.numeric(fullData$cylindernumber)
fullData$enginetype<-as.numeric(fullData$enginetype)

# test if the categorical variables indeed affect Y
model4<-lm(price ~ fueltype, data = fullData, x = TRUE, y = TRUE)
summary(model4) # no effect, Radj = 0.006
cor.test(fullData$fueltype, fullData$price)

model5<-lm(price ~ fuelsystem, data = fullData, x = TRUE, y = TRUE)
summary(model5)
cor.test(fullData$fuelsystem, fullData$price)

model6<-lm(price ~ aspiration, data = fullData, x = TRUE, y = TRUE)
summary(model6)
cor.test(fullData$aspiration, fullData$price)

model7<-lm(price ~ doornumber, data = fullData, x = TRUE, y = TRUE)
summary(model7) # no effect, Radj =~ 0
cor.test(fullData$doornumber, fullData$price)

model8<-lm(price ~ carbody, data = fullData, x = TRUE, y = TRUE)
summary(model8) # no effect, Radj =~ 0
cor.test(fullData$carbody, fullData$price)

model9<-lm(price ~ drivewheel, data = fullData, x = TRUE, y = TRUE)
summary(model9)
cor.test(fullData$drivewheel, fullData$price)

model10<-lm(price ~ enginelocation, data = fullData, x = TRUE, y = TRUE)
summary(model10)
cor.test(fullData$enginelocation, fullData$price)

model11<-lm(price ~ cylindernumber, data = fullData, x = TRUE, y = TRUE)
summary(model11) # Radj =~ 0 # surprising that cylindernumber isn't needed
cor.test(fullData$cylindernumber, fullData$price)

model12<-lm(price ~ enginetype, data = fullData, x = TRUE, y = TRUE)
summary(model12)
cor.test(fullData$enginetype, fullData$price)

# will drop fueltype, doornumber, carbody, cylindernumber and enginetype
fullData<-fullData[,-c(1, 4:5, 8:9)]

model <- lm(price ~ . +
                fuelsystem:boreratio + fuelsystem:horsepower + fuelsystem:carheight +
                aspiration:boreratio + aspiration:horsepower + aspiration:carheight +
                drivewheel:boreratio + drivewheel:horsepower + drivewheel:carheight +
                enginelocation:boreratio + enginelocation:horsepower + enginelocation:carheight
                , data = fullData)
summary(model) # Radj = 0.755

# categorial variables' scatter plots
# fuelsystem
plot(fullData$fuelsystem, fullData$price)
for (i in 1:length(fullData[, 1])){
  if (fullData[i, 1] == 3){
    fullData[i, 1]<-2
  }
  if (fullData[i, 1] == 5){
    fullData[i, 1]<-4
  }
  if (fullData[i, 1] == 8){
    fullData[i, 1]<-7
  }
}
# !! run model again - Radj pretty much the same = 0.755

par(mfrow = c(1, 1))
# removal of outliers
# cook's distance
plot(model, 4)
# residuals vs leverage
plot(model, 5)
fullData<-fullData[-c(10, 75, 130), ] # dropping outliers
numericData<-numericData[-c(10, 75, 130), ] # dropping outliers
# !! run model again - pretty much the same = 0.767

# assumptions' full examination
# 1. the errors are independent
linearity<-plot(model, 1) # X axis is for predictions, Y is the error

# 2. linearity
linearity<-plot(model, 1) # X axis is for predictions, Y is the error

# 3. each e come from a normal distribution
normality<-plot(model, 2)
shapiro.test(fullData$price)

# 4. equal variances - variation around the regression line is constant
plot(model, 3)
n<-nrow(fullData)
vector1<-fullData$price[row.names(fullData) %in% 1:as.integer(n/3)]
vector2<-fullData$price[row.names(fullData) %in% (as.integer(n/3)+1):(2*(as.integer(n/3)+1) - 1)]
F_res<-var.test(vector1, vector1)
F_res

# choosing optimal transformation
hist(fullData$price) # original state
shapiro.test(fullData$price)
summary(model) # original Radj = 0.767
origY<-fullData$price # original Y vector
# log(y)
newYlog<-log(fullData$price)
hist(newYlog)
fullData$price<-newYlog
shapiro.test(newYlog)
# !! run model again. Radj = 0.81

# y ^ 0.5
newYroot<-origY ^ 0.5
fullData$price<-newYroot
hist(newYroot)
shapiro.test(newYroot)
# !! run model again. Radj = 0.796

# y ^ -0.5
newYnegroot<-origY ^ -0.5
fullData$price<-newYnegroot
hist(newYnegroot)
shapiro.test(newYnegroot)
# !! run model again. Radj = 0.819

# boxcox
fullData$price<-origY
bc<-boxcox(lm(fullData$price ~ 1))
lambda<-bc$x[which.max(bc$y)]
newYboxcox<-(fullData$price ^ lambda - 1) / lambda
hist(newYboxcox) # better looking dist
fullData$price<-newYboxcox
normality<-plot(model, 2) # better looking graph
shapiro.test(fullData$price) # best result: 0.003
# !! run model again. Radj = 0.819
# the transformation made the hist more similar to normal dist, but the data still fails shapiro's test - Y isn't being normaly distributed

# step functions to see which variables/interactions should be dropped
backward<-step(model, direction = 'backward', trace = 1)
summary(backward) # AIC = -3174.54, Radj = 0.82

forward<-step(lm(price ~ 1, data = fullData), direction = 'forward', trace = 1, scope = formula(model))
summary(forward) # AIC = -3172.65, Radj = 0.817

stepwise<-step(model, direction = 'both')
summary(stepwise) # AIC = -3174.54, Radj = 0.82
# backwards and stepwise give the lowest AIC. i'll use the stepwise's result as my new model

newModel <- lm(price ~ fuelsystem + aspiration + drivewheel + carheight + 
                 horsepower + fuelsystem:horsepower + aspiration:horsepower + 
                 aspiration:carheight, data = fullData)
summary(newModel) # Radj = 0.822

fullData<-fullData[,-c(4, 6)] # dropping variables according to stepwise function
numericData<-numericData[,-2]

# assumptions' full examination
# 1. the e are independent
linearity<-plot(newModel, 1) # X axis is for predictions, Y is the error

# 2. linearity
linearity<-plot(newModel, 1) # X axis is for predictions, Y is the error

# 3. each e come from a normal distribution
normality<-plot(newModel, 2)
shapiro.test(fullData$price)

# 4. equal variances - variation around the regression line is constant
plot(newModel, 3)
n2<-nrow(fullData)
vector3<-fullData$price[row.names(fullData) %in% 1:as.integer(n2/3)]
vector4<-fullData$price[row.names(fullData) %in% (as.integer(n2/3)+1):(2*(as.integer(n2/3)+1) - 1)]
F_res<-var.test(vector1, vector1)
F_res

# ------------------------------------------------------------------------------
# new input - testing model
price<-23890
# boxcoxPrice<-(price ^ lambda - 1) / lambda
MAZDA2_G15_PURE_SP_2023<-data.frame(fuelsystem = 6, aspiration = 1, 
                                    drivewheel = 2, carheight = 58.9, 
                                    horsepower = 110)
# fuel system - mpfi, aspiration - standard, drivewheel - fwd, 
# carheight = 58.9, horsepower - 110
modelsPrice<-predict(newModel, MAZDA2_G15_PURE_SP_2023)
reverseBoxcox_ModelsRealPrice<-(lambda * modelsPrice + 1)**(1/lambda)
reverseBoxcox_ModelsRealPrice

# https://www.carexpert.com.au/mazda/mazda2/2023-g15-pure-sp-ba7d0b7e











