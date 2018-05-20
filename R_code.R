###################################################################################
# Libraries
###################################################################################
require(psych)
require(corrplot)
library(car)
library(randtests)
library(lmtest)
require(glmnet)
###################################################################################


###################################################################################
#Import Data
###################################################################################

bike_11 <- read.csv2("######")
#Delete X and instance columns
bike_11$X<-NULL
bike_11$instant<-NULL
bike_11$casual<-NULL
bike_11$registered<-NULL
bike_11$dteday<-NULL
###################################################################################
#Numeric variables
###################################################################################
#bike_11$dteday <- as.Date(bike_11$dteday)
bike_11<-transform(bike_11, temp = as.numeric(temp))
bike_11<-transform(bike_11, atemp = as.numeric(atemp))
bike_11<-transform(bike_11, hum = as.numeric(hum))
bike_11<-transform(bike_11, windspeed = as.numeric(windspeed))
bike_11<-transform(bike_11, cnt = as.numeric(cnt))
bike_11<-transform(bike_11, mnth = as.numeric(mnth))
bike_11<-transform(bike_11, hr = as.numeric(hr))
bike_11<-transform(bike_11, weekday = as.numeric(weekday))

###################################################################################
#Factor variables
###################################################################################
bike_11<-transform(bike_11, yr = factor(yr))
bike_11<-transform(bike_11, season = factor(season))
bike_11<-transform(bike_11, holiday = factor(holiday))
bike_11<-transform(bike_11, workingday = factor(workingday))
bike_11<-transform(bike_11, weathersit = factor(weathersit))


str(bike_11)
summary(bike_11)


###################################################################################
#Create numeric variable dataset
###################################################################################

index <- sapply(bike_11, class) == "numeric"
bikenum <- bike_11[,index]

###################################################################################
#Create factor variable dataset
###################################################################################

index <- sapply(bike_11, class) == "factor"
bikefact <- bike_11[,index]

###################################################################################
#Visual Analysis for numerical variables
###################################################################################

par(mfrow=c(2,5))
for (i in 1:nrow(bikenum))
    {
      hist(bikenum[,i],col="salmon", main=names(bikenum)[i])
    }

###################################################################################
#Visual Analysis for factors
###################################################################################

par(mfrow=c(3,2))
n <- nrow(bikefact)

barplot(table(bikefact[,1])/n, main="Seasons", col=c("salmon"), names.arg = c("Spring","Summer","Fall","Winter"))
barplot(table(bikefact[,2])/n, main="Years", col=c("salmon"), names.arg = c("2011","2012"))
barplot(table(bikefact[,3])/n, main="Holiday", col=c("salmon"), names.arg = c("Yes","No"))
barplot(table(bikefact[,4])/n, main="Working Day", col=c("salmon"), names.arg = c("Yes","No"))
barplot(table(bikefact[,5])/n, main="Weathersit", col=c("salmon"),args.legend = list(x = "topright"), legend = c("1:Clear Weather","2:Intermidiate Weather","3:Bad Weather"))

###################################################################################
#Pairs of numerical variables
###################################################################################
cor(bikefact)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(bikenum), method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

###################################################################################
#cnt on each numerical variable
###################################################################################
par(mfrow=c(2,5))
for(j in 1:(nrow(bikenum)-1)){
  plot(bikenum[,j], bikenum[,ncol(bikenum)], xlab=names(bikenum)[j], ylab='cnt',cex.lab=1.5,col="salmon")
  abline(lm(bikenum[,ncol(bikenum)]~bikenum[,j]))
}

plot(bikenum[,2], bikenum[,ncol(bikenum)], xlab=names(bikenum)[4], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,2]))
write.csv(bikenum[,2],file="demo")


par(mfrow=c(1,4))
plot(bikenum[,4], bikenum[,ncol(bikenum)], xlab=names(bikenum)[4], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,4]))

plot(bikenum[,5], bikenum[,ncol(bikenum)],xlab=names(bikenum)[5], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,5]))

plot(bikenum[,6], bikenum[,ncol(bikenum)], xlab=names(bikenum)[6], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,6]))

plot(bikenum[,7], bikenum[,ncol(bikenum)], xlab=names(bikenum)[7], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,7]))

par(mfrow=c(2,5))
for(j in 1:(nrow(bikenum)-1)){
  boxplot(bikenum[,ncol(bikenum)]~bikenum[,j], xlab=names(bikenum)[j], ylab='cnt',cex.lab=1.5,col="salmon")
  abline(lm(bikenum[,ncol(bikenum)]~bikenum[,j]),col=2)
}

par(mfrow=c(1,2))
plot(bikenum[,4], bikenum[,ncol(bikenum)], xlab=names(bikenum)[4], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,4]))

plot(bikenum[,5], bikenum[,ncol(bikenum)], xlab=names(bikenum)[5], ylab='cnt',cex.lab=1.5,col="salmon")
abline(lm(bikenum[,ncol(bikenum)]~bikenum[,5]))

###################################################################################
#cnt on factor variables
###################################################################################
par(mfrow=c(3,2))
for(j in 1:(nrow(bikenum)-1)){
  boxplot(bikenum[,ncol(bikenum)]~bikefact[,j], xlab=names(bikefact)[j], ylab='cnt',cex.lab=2.0,col="salmon")
  abline(lm(bikenum[,ncol(bikenum)]~bikefact[,j]))
  }


#----------------------------------------------------------------------------------
#Initial regression model
#----------------------------------------------------------------------------------

#Mosdel (adj 1)
model <- lm(cnt ~., data = bikenum)
summary(model)

#New model
summary(step(model, direction = "both"))


#Collinearity check
round(vif(model),1)#Using VIF 
vif(step(model, direction = "both"))

#Model1
# atemp, windspeed arent significant and we exclude them
model1<- lm(cnt ~.-atemp-windspeed, data = bikenum)
summary(model1)

#Test the significance model1
anova(model1)

#Model2-No intercept
model2<- lm(cnt ~.-1-atemp-windspeed, data = bikenum)
summary(model2)
AIC(model2)
vif(model2)
n <- nrow(bikenum)
true.r2 <- 1-sum(model2$res^2)/((n-1)*var(bikenum$cnt))
true.r2

anova(model1,model2)

#----------------------------------------------------------------------------------
#Constant model 
#----------------------------------------------------------------------------------

model0 <- lm(cnt ~ 1, data = bikenum) 
summary(model0)

anova(model2,model0)


#----------------------------------------------------------------------------------
#Adding factors
#----------------------------------------------------------------------------------
fullmodel<-lm(cnt~., data=bike_11) #FULL MODEL
summary(fullmodel)
vif(fullmodel)
AIC(fullmodel)

#Model 3
model3<-step(fullmodel, direction = "both")
summary(model3)
vif(model3)
AIC(model3)


###################################################################################
#Anova Test
###################################################################################
#Now we test whether the additional parameters in two nested models are zero or not

anova(model2,model3)


#----------------------------------------------------------------------------------
#Centralized Model
#----------------------------------------------------------------------------------

bike_Central<- as.data.frame(scale(bikenum, center = TRUE, scale = F))
bike_Central$cnt<-bikenum$cnt
bike_Central<-as.data.frame(c(bike_Central,bikefact))
sapply(bike_Central,mean)
sapply(bike_Central,sd)
central_model<-lm(cnt~.-mnth-atemp-windspeed-holiday-workingday-yr, data=bike_Central)
summary(central_model)

###################################################
###### Checking the assumptions ###################
###################################################

# ------------------------------
# Check Normality of the residuals & Costant variance
# ------------------------------
finalmodel<-model3

Stud.residuals <- rstudent(finalmodel)
yhat <- fitted(finalmodel)
par(mfrow=c(1,3))

plot(finalmodel,col="salmon", which = 2)

{plot(yhat, Stud.residuals,main="Residuals Variance",col="salmon")
  abline(h=c(-2,2), col=2, lty=2)}

{plot(yhat, Stud.residuals^2,main="Residuals Variance R^2",col="salmon")
  abline(h=4, col=2, lty=2)}

shapiro.test(finalmodel$residuals)
# ------------------
ncvTest(finalmodel)

# ------------------------------
# Check for the variance in quantiles
# -------------------------------
par(mfrow=c(1,3))
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(finalmodel)~yhat.quantiles)
boxplot(rstudent(finalmodel)~yhat.quantiles,col="salmon",main="Variance in Quantiles")

# -------------------------------
# Check for residuals linearity
# -------------------------------

residualPlot(finalmodel, type='rstudent',col="salmon",main="Residuals Linearity")
residualPlots(finalmodel, plot=F)

# --------------------------------
# Check for residuals Independence 
# --------------------------------

plot(rstudent(finalmodel), type='l',col="salmon",main="Residuals Dependence")
runs.test(finalmodel$res)
dwtest(finalmodel)
durbinWatsonTest(finalmodel)

# --------------------------------
# Check for outliers 
# --------------------------------

leveragePlots(finalmodel,col="salmon")

###################################################
###### Organize the plots ###################
###################################################

par(mfrow=c(2,3))

plot(finalmodel,col="salmon", which = 2)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(finalmodel)~yhat.quantiles)
boxplot(rstudent(finalmodel)~yhat.quantiles,col="salmon",main="Variance in Quantiles")

{plot(yhat, Stud.residuals^2,main="Residuals Variance R^2",col="salmon")
  abline(h=4, col=2, lty=2)}

residualPlot(finalmodel, type='rstudent',col="salmon",main="Residuals Linearity")
residualPlots(finalmodel, plot=F)

plot(rstudent(finalmodel), type='l',col="salmon",main="Residuals Independence")
runs.test(finalmodel$res)
dwtest(finalmodel)
durbinWatsonTest(finalmodel)


###################################################
###### Fixing assumaption problems ###################
###################################################
finalmodel<-model3


finalmodel<-lm(log10(cnt)~.+sin(6.28/24*hr)+cos(6.28/240*hr)-hr-mnth-atemp-windspeed-holiday-workingday, data=bike_11)

#or
shapiro.test(finalmodel$residuals)
Stud.residuals <- rstudent(finalmodel)
yhat <- fitted(finalmodel)

plot(finalmodel,col="salmon", which = 2)
residualPlots(finalmodel, plot=F)
summary(finalmodel)

par(mfrow=c(2,3))

plot(finalmodel,col="salmon", which = 2)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)
leveneTest(rstudent(finalmodel)~yhat.quantiles)
boxplot(rstudent(finalmodel)~yhat.quantiles,col="salmon",main="Variance in Quantiles")

{plot(yhat, Stud.residuals^2,main="Residuals Variance R^2",col="salmon")
  abline(h=4, col=2, lty=2)}

residualPlot(finalmodel, type='rstudent',col="salmon",main="Residuals Linearity")
residualPlots(finalmodel, plot=F)

plot(rstudent(finalmodel), type='l',col="salmon",main="Residuals Independence")
runs.test(finalmodel$res)
dwtest(finalmodel)
durbinWatsonTest(finalmodel)



      



##################################################################################
# LASSO 
###################################################################################

X <- model.matrix(fullmodel)[,-1]
lasso <- glmnet(X, bike_11$cnt)
plot(lasso, xvar = "lambda", label = T)

###################################################################################
#Comments: We try to find the "best" variables for our regression model with Lasso
#technic. We apply Lasso in full model (model3)
###################################################################################

###################################################################################
#Find the best value for Lamda
###################################################################################

lasso1 <- cv.glmnet(X, bike_11$cnt, alpha = 1)

###################################################################################
#Comments:Now we want to find the minimum lamda value.
###################################################################################

par(mfrow=c(1,2))

# Results
plot(lasso1)
plot(lasso1$glmnet.fit, xvar="lambda", label=TRUE)

log(lasso1$lambda.min)
log(lasso1$lambda.1se)
coef(lasso1, s=lasso1$lambda.1se)

Lasso_model<-lm(cnt~.-holiday-workingday-windspeed-weathersit-weekday, data=bike_11)
summary(Lasso_model)




#----------------------------------------------------------------------------------
#Centralized Lasso Model
#----------------------------------------------------------------------------------

bike_Central<- as.data.frame(scale(bikenum, center = TRUE, scale = F))
bike_Central$cnt<-bikenum$cnt
bike_Central<-as.data.frame(c(bike_Central,bikefact))
sapply(bike_Central,mean)
sapply(bike_Central,sd)
central_model<-lm(cnt~.-mnth-weekday-windspeed-holiday-workingday-weathersit, data=bike_Central)
summary(central_model)



###################################################################################
#Evaluation
###################################################################################

bike_test <- read.csv2("C:/Users/georg/Dropbox/Business Analytics/Statistics for Business Analytics/2_Assignment/data/bike_test.csv")
#Delete X and instance columns
bike_test$X<-NULL
bike_test$instant<-NULL
bike_test$casual<-NULL
bike_test$registered<-NULL
bike_test$dteday<-NULL
#Numeric variables
bike_test<-transform(bike_test, temp = as.numeric(temp))
bike_test<-transform(bike_test, atemp = as.numeric(atemp))
bike_test<-transform(bike_test, hum = as.numeric(hum))
bike_test<-transform(bike_test, windspeed = as.numeric(windspeed))
bike_test<-transform(bike_test, cnt = as.numeric(cnt))
bike_test<-transform(bike_test, mnth = as.numeric(mnth))
bike_test<-transform(bike_test, hr = as.numeric(hr))
bike_test<-transform(bike_test, weekday = as.numeric(weekday))

#Factor variables
bike_test<-transform(bike_test, yr = factor(yr))
bike_test<-transform(bike_test, season = factor(season))
bike_test<-transform(bike_test, holiday = factor(holiday))
bike_test<-transform(bike_test, workingday = factor(workingday))
bike_test<-transform(bike_test, weathersit = factor(weathersit))

###################################################################################
#Visual Analysis for numerical variables
###################################################################################
index <- sapply(bike_test, class) == "numeric"
bikenum_test <- bike_test[,index]


par(mfrow=c(2,5))
for (i in 1:nrow(bikenum_test))
{
  hist(bikenum_test[,i],col="salmon", main=names(bikenum_test)[i])
}
######Test the significance of the predictors in new dataset

wilcox.test(bikenum[,8],bikenum_test[,8])




###################################################################################
#Visual Analysis for factors
###################################################################################
index <- sapply(bike_test, class) == "factor"
bikefact_test <- bike_test[,index]

par(mfrow=c(3,2))
n <- nrow(bikefact_test)

barplot(table(bikefact_test[,1])/n, main="Seasons", col=c("salmon"), names.arg = c("Spring","Summer","Fall","Winter"))
barplot(table(bikefact_test[,2])/n, main="Years", col=c("salmon"), names.arg = c("2011","2012"))
barplot(table(bikefact_test[,3])/n, main="Holiday", col=c("salmon"), names.arg = c("Yes","No"))
barplot(table(bikefact_test[,4])/n, main="Working Day", col=c("salmon"), names.arg = c("Yes","No"))
barplot(table(bikefact_test[,5])/n, main="Weathersit", col=c("salmon"),args.legend = list(x = "topright"), legend = c("1:Clear Weather","2:Intermidiate Weather","3:Bad Weather"))


#old
nullmodel<-lm(cnt~1, data=bike_11)
fullmodel<-lm(cnt~., data=bike_11) #FULL MODEL
summary(fullmodel)
model3<-step(fullmodel, direction = "both")
model3<-lm(cnt~.-mnth-atemp-windspeed-holiday-workingday, data=bike_11)
summary(model3)
vif(model3)
training_model<-lm(cnt~.-mnth-atemp-windspeed-holiday-workingday-yr, data=bike_11)
summary(training_model)

Lasso_model<-lm(cnt~.-mnth-holiday-workingday-windspeed-weathersit-weekday, data=bike_11)
summary(Lasso_model)
Lasso_model2<-lm(cnt~.-mnth-holiday-workingday-windspeed-weathersit-weekday-atemp-season, data=bike_11)
summary(Lasso_model2)

#new
null_model_test<-lm(cnt~1, data=bike_test)
full_model_test<-lm(cnt~., data=bike_test)
summary(full_model_test)
AIC(full_model_test)

model3_test<-lm(cnt~.-mnth-atemp-windspeed-weekday-holiday-workingday, data=bike_test)
summary(model3_test)
vif(model3_test)
AIC(model3_test)

Lasso_model_test<-lm(cnt~.-mnth-holiday-workingday-windspeed-weathersit-weekday, data=bike_test)
summary(Lasso_model_test)
Lasso_model2_test<-lm(cnt~.-mnth-holiday-workingday-windspeed-weathersit-weekday-atemp-season, data=bike_test)
summary(Lasso_model2_test)


finalmodel_test<-lm(log10(cnt)~.+sin(6.28/24*hr)+cos(6.28/240*hr)-hr-mnth-atemp-windspeed-holiday-workingday, data=bike_test)
summary(finalmodel_test)
AIC(finalmodel_test)

######Evaluation

#library Metrics
y <- bike_test$cnt
y.pred <- predict(Lasso_model, bike_test)

hist(y.pred,col="salmon", main="pred")
hist(y,col="salmon", main="actual")

