rm(list=ls())         #Use in removing all previous environmnets and clear screen


#locate the file directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

#********************************************************************
#load general package 
#********************************************************************
library(ggplot2)
library(splines)
library(moments)
library(Metrics)
library(tidyverse)
library(GGally)
library(caTools)
library(graphics)
library(lattice)
library(caret)
library(caretEnsemble)
library(Rlibeemd)
#********************************************************************

#********************************************************************
#load dataset 
#********************************************************************
dataset<-read.csv("Bitstamp_BTCUSD_minute")
datasets <- dataset[,2:7]

#dataset <- dataset[,2]

#convert data to time series data
#datasets <- ts(datasets)

#checking missing values
anyNA(datasets)
sum(is.na(datasets))
#list rows of data that have missing values
dataset[!complete.cases(datasets),]

#delete the NA's
dataset <- na.omit(datasets)
anyNA(datasets)

#summary statistics
data.frame(mean(datasets$close),sd(datasets$close), kurtosis(datasets$close), skewness(datasets$close))

#Plot original signal and white noise added
#par(mfrow=c(1,2))
plot(ts(datasvr$x), pch=19,lwd=1,xlab="Time/minute",
     ylab="Price/ USD",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2 #main='Closing Price of Bitcoin (USD)' 
)
grid()

#*********************Technical Features *************************
library(fTrading)
library(xts)
x <- datasets[,4]
open <- datasets[,1]
close<- datasets[,4]
high <- datasets[,2]
low <- datasets[,3]
#Open <- ts(open)
#Close <- ts(close)
#High <- ts(high)
#Low <- ts(low)
lambda <- 0.9
lag <- 5   #5,20,50,100,200, since we are predicting for a shorter-time, a shorter moving average will be more effective for trading style.
lag1=5
lag2=10
lag3=20
lag4=30

#Utility Functions
ema <- emaTA(x, lambda, startup = 0)
bias <- biasTA(x, lag)
medprice <- medpriceTA(high, low)
tpicalprice <- typicalpriceTA(high, low, close)
wclose <- wcloseTA(high, low, close)
roc <- rocTA(x, lag)
osc <- oscTA(x, lag1, lag2)

#Oscillator Indicators
mom <- momTA(x, lag)
macd <- macdTA(x, lag1, lag2)
cds <- cdsTA(x, lag1, lag2, lag3)
cdo <- cdoTA(x, lag1, lag2, lag3)
voh <- vohlTA(high, low)
vor <- vorTA(high, low)
stoch <- stochasticTA(close, high, low, lag1=9, lag2=14, lag=20, type = c("fast", "slow")) 
stoch <- stoch[,2]
fkp <- fpkTA(close, high, low, lag)
fpd <- fpdTA(close, high, low, lag1, lag2)
spd <- spdTA(close, high, low, lag1, lag2, lag3)
apd <- apdTA(close, high, low, lag1, lag2, lag3, lag4)
wpr <- wprTA(close, high, low, lag)
rsi <- rsiTA(close, lag=9)

#Moving Averages
sma <- SMA(x, n = 1)
ewma <- EWMA(x, lambda, startup = 0)

ti_1 <- data.frame(ema, bias, medprice, tpicalprice, wclose, roc, osc, 
                   mom, macd, cds, cdo, voh, vor, stoch, fkp, fpd, spd, apd, wpr, rsi, 
                   sma, ewma)

#TTR
library(TTR)
zz <- ZigZag(datasets[,c("high","low")], change=5)
#stochOsc <- stoch(datasets[,c("high","low","close")])                     #Stochastic oscillator
#stochOsc <- stochOsc[,1]
stochWPR <- WPR(datasets[,c("high","low","close")])                        #William’s % R
ad <- williamsAD(datasets[,c("high","low","close")])                      #Williams Accumulation / Distribution (AD) line is a measure of market momentum.
vhf.hilow <- VHF(datasets[,c("high","low","close")], n=5)                   #The Vertical Horizontal Filter (VHF) attempts to identify starting and ending trends
ult.osc <- ultimateOscillator(datasets[,c("high","low","close")],          #The Ultimate Oscillator is a momentum oscillator designed to capture momentum across three
                              n = c(5, 10, 20), wts = c(3, 2, 1))         #different time frames.
bbands <- BBands(datasets[,c("high","low","close")] )                     #Bollinger Bands
bbands <- bbands[,4]
adx <- ADX(datasets[,c("high","low","close")])                            #Directional Movement Index
adx <- adx[,4]
rsi <- RSI(datasets[,c("close")])                                         #relative strength index

#dataframe
ti_2 <- data.frame(zz,stochWPR,ad,vhf.hilow,ult.osc,bbands,adx,rsi)

#Combination
tt <- data.frame(ti_1,ti_2)
tt_close <- data.frame(x,tt)
tt_close1 <- tt_close
dim(tt_close1)

#checking missing values
anyNA(tt_close1)
sum(is.na(tt_close1))

#delete the NA's
tt_close1 <- na.omit(tt_close1)
anyNA(tt_close1)
dim(tt_close1)
data.frame(mean(tt_close1$x),sd(tt_close1$x), kurtosis(tt_close1$x), skewness(tt_close1$x))


#****correlation matrix feature selection
library(corrplot)
library(caret)
datMy <- tt_close1

#scale all the features (from feature 2 bacause feature 1 is the predictor output)
datMy.scale<- scale(datMy[2:ncol(datMy)],center=TRUE,scale=TRUE)

#compute the correlation matrix
corMatMy <- cor(datMy.scale)

#visualize the matrix, clustering features by correlation index.
corrplot(corMatMy, order = "hclust")
#corrplot(res, method="color", type="full", order="hclust")
#Apply correlation filter at 0.99
highlyCor <- findCorrelation(corMatMy, cutoff=0.70)
# print indexes of highly correlated attributes
print(highlyCor)
#then we remove all the variable correlated with more 0.80.
datMyFiltered.scale <- datMy.scale[,-highlyCor]
print(datMyFiltered.scale)
corMatMyy <- cor(datMyFiltered.scale)
par(mfrow=c(1,2))
corrplot(corMatMyy, order = "hclust", method="number")
corrplot(corMatMyy, order = "hclust")


#install.packages("FactoMineR") #principal component analysis
#require(FactoMineR) 
#read the tab file using the read table function.
#datmy <- tt_close1
#scale all the features,  ncp: number of dimensions kept in the results (by default 5)
#pca <- PCA(datmy, scale.unit=TRUE, ncp=5, graph=T)
#This line of code will sort the variables the most linked to each PC. It is very useful when you have many variables.
#dimdesc(pca)


#*********************Boruta Algorithm ****************************
library(Boruta)
set.seed(123)
newdat <- data.frame(tt_close1$x, datMyFiltered.scale)
#checking missing values
anyNA(newdat)
sum(is.na(newdat))

#delete the NA's
newdat <- na.omit(newdat)
anyNA(newdat)

#normalizing #libraries
library(tidyverse)
library(GGally)
dat_scaled <- preProcess(newdat, method=c("range"))
norm2 <- predict(dat_scaled, newdat)
summary(norm2)

boruta.train <- Boruta(tt_close1.x ~., data = norm2, doTrace = 2, ntree = 500)
print(boruta.train)
attStats(boruta.train)

#add the attributes to the x-axis vertically.
par(mfrow=c(1,2))
plot(boruta.train, xlab = "", ylab="Z Score", xaxt = "n")
plotImpHistory(boruta.train, xlab = "Zscore  evolution  during  Boruta  run.", ylab="Z Score", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#taking decision on tentative attributes.
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)

#creating data frame of the final result derived from Boruta
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)
write.csv(boruta.df,"/Users/Gyasams/Desktop/Bitcoin\ Research/Script/boruta.df.csv") 


#*************normailzing and partitioning into training and testing set
library(tidyverse)
library(GGally)
datasvr<-read.csv(file.choose(), header=TRUE)
#datasvr <- tt_close1[c(1,9,11,12,14,15,16,19,24,27,30)]
#write.csv(datasvr,"/Users/Gyasams/Desktop/Bitcoin\ Research/Script/datasvr.csv") 
maxs <- apply(datasvr, 2, max) 
mins <- apply(datasvr, 2, min)
scaled.ti <- as.data.frame(scale(datasvr, center = mins, scale = maxs - mins))

#subsetting for training, validation, and testing 
set.seed(1234)
# Create random training, validation, and test sets

# Set some input variables to define the splitting.
# Input 1. The data frame that you want to split into training, validation, and test.
df <- scaled.ti

# Input 2. Set the fractions of the dataframe you want to split into training, 
# validation, and test.
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(df))
sampleSizeValidation <- floor(fractionValidation * nrow(df))
sampleSizeTest       <- floor(fractionTest       * nrow(df))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
train.ti   <- df[indicesTraining, ]
Xtrain.ti <- train.ti[,2:11]
Ytrain.ti <- train.ti[,1]
validation.ti <- df[indicesValidation, ]
Xvalidate.ti <- validation.ti[,2:11]
Yvalidate.ti <- validation.ti[,1]
test.ti      <- df[indicesTest, ]
Xtest.ti <- test.ti[,2:11]
Ytest.ti <- test.ti[,1]

#********************* Support vector machine technical indicators *********************
#SVM
#practice
set.seed(1234)
n<- dim(scaled.ti)[1]
train.ti.sample <- head(scaled.ti, round(n * (1000/NROW(scaled.ti))))
Xtrain.ti.sample <- train.ti.sample[,2:11]
Ytrain.ti.sample <- train.ti.sample[,1]

#Training
library(e1071)
library(caret)
set.seed(12321)
svm_tune <- svm(Xtrain.ti, Ytrain.ti,
                data = train.ti, 
                kernel = "radial", 
                cost=10, gamma=0.1,degree = 2,
                scale = FALSE)

#Validation
svm_tune <- tune(svm, Xtrain.ti.sample, Ytrain.ti.sample,
                 data=train.ti.sample, kernel="polynomial",
                 ranges = list(cost = 10^(1:4), 
                               gamma=seq(0,5,0.1),
                               degree=seq(1,4,1)
                               ) 
                )
print(svm_tune)
# Draw the tuning graph
plot(svm_tune)
#select the best model
tunedModel <- svm_tune$best.model

#After you find the best cost and gamma, you can create svm model again and try to run again
svm_model_after_tune <- svm(Xtrain.ti.subset, Ytrain.ti.subset,
                            data= train.ti.subset, 
                            kernel="radial", 
                            cost= tunedModel$cost, gamma=tunedModel$gamma)
summary(svm_model_after_tune)



#Run Prediction with new model
svr.test.predict <- predict(svm_tune,
                            newdata = Xtest.ti)

#convert to original data
test.ti.predict <- svr.test.predict * (max(datasvr$x) - min(datasvr$x)) + min(datasvr$x)
test.ti <- Ytest.ti * (max(datasvr$x) - min(datasvr$x)) + min(datasvr$x)

#save the data
write.csv(test.ti.predict,"/Users/Gyasams/Desktop/Bitcoin\ Research/Script/testing_ti_polynomial.csv") 

test.ti.predict <- read.csv("testing_ti_radial.csv")
test.ti <- read.csv("testing_data.csv") 


plot(ts(test.ti$x), pch=19, lwd=3.0, xlab="Time/Minute",
     ylab="Price/USD", col="steelblue", lty=1, bty="n", xlim=c(0,30000), ylim=c(15000,80000))
lines(ts(test.ti.predict$x), pch=19, lwd=0.5, col="red", lty=1)
grid()
legend("bottomleft",legend=c("Original Bitcoin Price", "Predicted Bitcoin Price"),  inset = c(0.2, 0.70),
       col=c("steelblue", "red"), box.lty=0, lty=1:1, lwd=2, cex=0.9, text.font=6)


#evaluation of testing data using the different kernels, cost parameters
actual <- test.ti
predicted <- test.ti.predict
Error <- test.ti - test.ti.predict
squaredError <- Error^2
absError <- abs(Error)
absError

### Calculate MAE
n <- NROW(actual)
estMAE <- sum(abs(Error))
MAE <- estMAE/n
MAE <- data.frame(MAE)
MAE

### Calculate NRMSE_sd
normalizerST_DEV <- sd(Actual)
a <- sqrt(mean((Error)^2)) ###Correct RMSE
NRMSE_sd <- a / normalizerST_DEV
NRMSE_sd <- data.frame(NRMSE_sd)
NRMSE_sd


### Calculate MAPE
#### Use absError from MAE
#### Calculate absActual
absActual <- abs(actual)
#### Calculate absError/absActual
ErrAct <- absError/absActual
#### Calculate MAPE: sum(ErrAct) *(100/n)
MAPE <- sum(ErrAct) * (100/n)
MAPE <- data.frame(MAPE)
MAPE


#*********************VMD*********************
#*#signal the time domain signal (1D) to be decomposed
#alpha the balancing parameter of the data-fidelity constraint
#tau time-step of the dual ascent (pick 0 for noise-slack)
#K the number of modes to be recovered, i set it to k=3 but the default was 3
#DC true if the first mode is put and kept at DC (0-freq)
#init 0 = all omegas start at 0, 1 = all omegas start uniformly distributed or 2 = all
#omegas initialized randomly
#tol tolerance of convergence criterion, typically around 1e-6
write.csv(tt_close1$x,"/Users/Gyasams/Desktop/Bitcoin\ Research/Script/tt_close1.csv") 


library(vmd)
set.seed(200)
training.vmd <-read.csv(file.choose(), header=TRUE)
signal.vmd = training.vmd[,2]
#v = vmd(signal,DC=FALSE,tol=1e-3)
v = vmd(signal.vmd,alpha=1000,tau=0,DC=FALSE,init=0,tol=1e-3,K=10,orderModes=TRUE)

#List of Results
l = v$getResult()
names(l)

#To Data Frame
df = as.data.frame(v)
head(df)

#Input Spectrum
v$plot.input.spectrum()

#Plot Results
plot(v)
plot(v,facet='bymode',scales='free')
plot(v,facet='byclass',scales='free')

#Input Spectrum
v$plot.input.spectrum()

#Spectral Decomposition
v$plot.spectral.decomposition()

#signal absolute average difference
dataset<-read.csv(file.choose(), header=TRUE)
dataset <- dataset[,3:18]
Actual <- dataset$Signal
Error.vmd <- dataset$Signal - dataset$MAgg
n <- NROW(Actual)

#saad
absError.vmd <- abs(Error.vmd)
estSAAD <- sum(absError.vmd )
SAAD <- estSAAD/n
SAAD 

#mpe
MPE.vmd <- sum(Error.vmd/Actual) * (100 / n)
MPE.vmd

### Calculate NRMSE_sd
normalizerST_DEV <- sd(Actual)
a <- sqrt(mean((Error.vmd)^2)) ###Correct RMSE
NRMSE_sd <- a / normalizerST_DEV
NRMSE_sd <- data.frame(NRMSE_sd)
NRMSE_sd

#plot
par(mfrow=c(2,3))
plot(ts(dataset$M1), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #1")
plot(ts(dataset$M2), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #2")
plot(ts(dataset$M3), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #3")
plot(ts(dataset$M4), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #4")
plot(ts(dataset$M5), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #5")
plot(ts(dataset$M6), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #6")

par(mfrow=c(2,3))
plot(ts(dataset$M7), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #7")
plot(ts(dataset$M8), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #8")
plot(ts(dataset$M9), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #9")
plot(ts(dataset$M10), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #10")
plot(ts(dataset$M11), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #11")
plot(ts(dataset$M12), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #12")

par(mfrow=c(2,3))
plot(ts(dataset$M13), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #13")
plot(ts(dataset$M14), pch=19,lwd=1,xlab="Time/Minute",
     ylab="VMF Amplitude",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="VMF #14")
grid()

#plot of aggregate and original series
plot(ts(dataset$Signal), pch=19,lwd=1.5,xlab="Time/Minute",
     ylab="Price/USD",col="steelblue",lty=1,
     cex.lab=2, cex.axis=2, main="")
lines(ts(dataset$MAgg), pch=19,lwd=1, col="red",lty=1)
#lines(q.pred.train.convert[,1],col="violet",pch=19, lty="dotted",lwd=2)  #lower limit 0.05 quantile
grid()
legend("topleft", legend=c("Original Bitcoin Price","Aggregate of VMFs"),
       lty=c("solid","solid"),col=c("steelblue", "red"),
       bg="white",lwd=2,cex = 0.50)


#**********
#estimatimg the correlation coeefficient between the dataset and the IMF
set.seed(200)
try <-read.csv(file.choose(), header=TRUE)
try <- try [,3:17]
correlation <- cor(try[,1],try[,3:15])
write.csv(correlation,"/Users/Gyasams/Desktop/NewResearch/RCodeDataTemp/correlation_signa_imf.csv") 

#Signal reconstitution, output gives relevant IMF for the modelling
mu <- (max(correlation))/((10*max(correlation))-3)
mu <- max(correlation)/10
which(correlation > mu)  
write.csv(mu,"/Users/Gyasams/Desktop/NewResearch/RCodeDataTemp/mu.csv") 

#signal reconstruction
reconstruction <- try[,3] + try[,4] + try[,5]
write.csv(reconstruction,"/Users/Gyasams/Desktop/NewResearch/RCodeDataTemp/reconstruction_signal.csv") 
plot(reconstruction, xlab="nth month", ylab="IMF Amplitude", main="", type="l", 
     pch=19, lwd=2,col="steelblue",lty=1,cex.lab=4, cex.axis=4, axes=TRUE)



#********************* SVR vmd *********************
#normailzing and partitioning into training and testing set
library(tidyverse)
library(GGally)
datav <- read.csv(file.choose(), header=TRUE)  #choose df_14
datavmd <- datav[,1:15]
maxs <- apply(datavmd, 2, max) 
mins <- apply(datavmd, 2, min)
scaled_vmd <- as.data.frame(scale(datavmd, center = mins, scale = maxs - mins))


#subsetting for training, validation, and testing 
set.seed(1234)
# Create random training, validation, and test sets

# Set some input variables to define the splitting.
# Input 1. The data frame that you want to split into training, validation, and test.
df_vmd <- scaled_vmd

# Input 2. Set the fractions of the dataframe you want to split into training, 
# validation, and test.
fractionTraining_vmd   <- 0.60
fractionValidation_vmd <- 0.20
fractionTest_vmd       <- 0.20

# Compute sample sizes.
sampleSizeTraining_vmd   <- floor(fractionTraining_vmd   * nrow(df_vmd))
sampleSizeValidation_vmd <- floor(fractionValidation_vmd * nrow(df_vmd))
sampleSizeTest_vmd       <- floor(fractionTest_vmd       * nrow(df_vmd))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining_vmd    <- sort(sample(seq_len(nrow(df_vmd)), size=sampleSizeTraining_vmd))
indicesNotTraining_vmd <- setdiff(seq_len(nrow(df_vmd)), indicesTraining_vmd)
indicesValidation_vmd  <- sort(sample(indicesNotTraining_vmd, size=sampleSizeValidation_vmd))
indicesTest_vmd        <- setdiff(indicesNotTraining_vmd, indicesValidation_vmd)

# Finally, output the three dataframes for training, validation and test.
train_vmd   <- df_vmd[indicesTraining_vmd, ]
Xtrain_vmd <- train_vmd[,2:11]
Ytrain_vmd <- train_vmd[,1]
validation_vmd <- df_vmd[indicesValidation_vmd, ]
Xvalidate_vmd <- validation_vmd[,2:11]
Yvalidate_vmd <- validation_vmd[,1]
test_vmd      <- df_vmd[indicesTest_vmd, ]
Xtest_vmd <- test_vmd[,2:11]
Ytest_vmd <- test_vmd[,1]


#Training
library(e1071)
library(caret)
set.seed(12321)
svm_tune <- svm(Xtrain_vmd, Ytrain_vmd,
                data = train_vmd, 
                kernel = "radial", 
                cost=10, gamma=0.6,
                scale = FALSE)

svm_tune <- svm(Xtrain_vmd, Ytrain_vmd,
                data = train_vmd, 
                kernel = "polynomial", 
                cost=10, gamma=0.6,degree = 2,
                scale = FALSE)

#Run Prediction with new model
svr.test.predict <- predict(svm_tune,
                            newdata = Xtest_vmd)

#convert to original data
test.ti.predict <- svr.test.predict * (max(datavmd$Signal) - min(datavmd$Signal)) + min(datavmd$Signal)
test.ti <- Ytest_vmd * (max(datavmd$Signal) - min(datavmd$Signal)) + min(datavmd$Signal)

#save the data
write.csv(test.ti.predict,"/Users/Gyasams/Desktop/Bitcoin\ Research/Script/testing_rVMF_polynomial.csv") 

plot(ts(test.ti), pch=19, lwd=1.5, xlab="Time/Minute",
     ylab="Price/USD", col="steelblue", lty=1, bty="n", xlim=c(0,30000), ylim=c(15000,80000))
lines(ts(test.ti.predict), pch=19, lwd=1.5, col="red", lty=1)
grid()
legend("bottomleft",legend=c("Original Bitcoin Price", "Predicted Bitcoin Price"),  inset = c(0.2, 0.70),
       col=c("steelblue", "red"), box.lty=0, lty=1:1, lwd=3, cex=0.9, text.font=6)


#evaluation of testing data using the different kernels, cost parameters
actual <- test.ti
predicted <- test.ti.predict
Error <- test.ti - test.ti.predict
squaredError <- Error^2
absError <- abs(Error)
absError

### Calculate MAE
n <- NROW(actual)
estMAE <- sum(abs(Error))
MAE <- estMAE/n
MAE <- data.frame(MAE)
MAE

### Calculate NRMSE_sd
normalizerST_DEV <- sd(actual)
a <- sqrt(mean((Error)^2)) ###Correct RMSE
NRMSE_sd <- a / normalizerST_DEV
NRMSE_sd <- data.frame(NRMSE_sd)
NRMSE_sd


### Calculate MAPE
#### Use absError from MAE
#### Calculate absActual
absActual <- abs(actual)
#### Calculate absError/absActual
ErrAct <- absError/absActual
#### Calculate MAPE: sum(ErrAct) *(100/n)
MAPE <- sum(ErrAct) * (100/n)
MAPE <- data.frame(MAPE)
MAPE



#**********************SVR_ti_VMD********************
#normailzing and partitioning into training and testing set
library(tidyverse)
library(GGally)
data_vmf <- read.csv(file.choose(), header=TRUE)  #choose df_14
data_vmd <- data_vmf[,2:15]
data_ti <- read.csv(file.choose(), header=TRUE)

data_vmd_ti <- cbind(data_ti, data_vmf)
maxs <- apply(data_vmd_ti, 2, max) 
mins <- apply(data_vmd_ti, 2, min)
scaled_vmd_ti <- as.data.frame(scale(data_vmd_ti, center = mins, scale = maxs - mins))



#subsetting for training, validation, and testing 
set.seed(1234)
# Create random training, validation, and test sets

# Set some input variables to define the splitting.
# Input 1. The data frame that you want to split into training, validation, and test.
df_vmd_ti <- scaled_vmd_ti

# Input 2. Set the fractions of the dataframe you want to split into training, 
# validation, and test.
fractionTraining_vmd_ti   <- 0.60
fractionValidation_vmd_ti <- 0.20
fractionTest_vmd_ti       <- 0.20

# Compute sample sizes.
sampleSizeTraining_vmd_ti   <- floor(fractionTraining_vmd_ti   * nrow(df_vmd_ti))
sampleSizeValidation_vmd_ti <- floor(fractionValidation_vmd_ti * nrow(df_vmd_ti))
sampleSizeTest_vmd_ti       <- floor(fractionTest_vmd_ti       * nrow(df_vmd_ti))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining_vmd_ti    <- sort(sample(seq_len(nrow(df_vmd_ti)), size=sampleSizeTraining_vmd_ti))
indicesNotTraining_vmd_ti <- setdiff(seq_len(nrow(df_vmd_ti)), indicesTraining_vmd_ti)
indicesValidation_vmd_ti  <- sort(sample(indicesNotTraining_vmd_ti, size=sampleSizeValidation_vmd_ti))
indicesTest_vmd_ti        <- setdiff(indicesNotTraining_vmd_ti, indicesValidation_vmd_ti)

# Finally, output the three dataframes for training, validation and test.
train_vmd_ti   <- df_vmd_ti[indicesTraining_vmd_ti, ]
Xtrain_vmd_ti <- train_vmd_ti[,2:25]
Ytrain_vmd_ti <- train_vmd_ti[,1]
validation_vmd_ti <- df_vmd_ti[indicesValidation_vmd_ti, ]
Xvalidate_vmd_ti <- validation_vmd_ti[,2:25]
Yvalidate_vmd_ti <- validation_vmd_ti[,1]
test_vmd_ti      <- df_vmd_ti[indicesTest_vmd_ti, ]
Xtest_vmd_ti <- test_vmd_ti[,2:25]
Ytest_vmd_ti <- test_vmd_ti[,1]


#Training
library(e1071)
library(caret)
set.seed(12321)
svm_tune <- svm(Xtrain_vmd_ti, Ytrain_vmd_ti,
                data = train_vmd_ti, 
                kernel = "radial", 
                cost=10, gamma=0.4,
                scale = FALSE)

svm_tune <- svm(Xtrain_vmd_ti, Ytrain_vmd_ti,
                data = train_vmd_ti, 
                kernel = "polynomial", 
                cost=10, gamma=0.4,degree = 2,
                scale = FALSE)

#Run Prediction with new model
svr.test.predict <- predict(svm_tune,
                            newdata = Xtest_vmd_ti)

#convert to original data
test.ti.predict <- svr.test.predict * (max(data_vmd_ti$x) - min(data_vmd_ti$x)) + min(data_vmd_ti$x)
test.ti <- Ytest_vmd_ti * (max(data_vmd_ti$x) - min(data_vmd_ti$x)) + min(data_vmd_ti$x)

#save the data
write.csv(test.ti.predict,"/Users/Gyasams/Desktop/Bitcoin\ Research/Script/testing_rVMF_ti_polynomial.csv") 

plot(ts(test.ti), pch=19, lwd=1.5, xlab="Time/Minute",
     ylab="Price/USD", col="steelblue", lty=1, bty="n", xlim=c(0,30000), ylim=c(15000,80000))
lines(ts(test.ti.predict), pch=19, lwd=1.5, col="red", lty=1)
grid()
legend("bottomleft",legend=c("Original Bitcoin Price", "Predicted Bitcoin Price"),  inset = c(0.2, 0.70),
       col=c("steelblue", "red"), box.lty=0, lty=1:1, lwd=3, cex=0.9, text.font=6)



#evaluation of testing data using the different kernels, cost parameters
actual <- test.ti
predicted <- test.ti.predict
Error <- test.ti - test.ti.predict
squaredError <- Error^2
absError <- abs(Error)
absError

### Calculate MAE
n <- NROW(actual)
estMAE <- sum(abs(Error))
MAE <- estMAE/n
MAE <- data.frame(MAE)
MAE

### Calculate NRMSE_sd
normalizerST_DEV <- sd(actual)
a <- sqrt(mean((Error)^2)) ###Correct RMSE
NRMSE_sd <- a / normalizerST_DEV
NRMSE_sd <- data.frame(NRMSE_sd)
NRMSE_sd


### Calculate MAPE
#### Use absError from MAE
#### Calculate absActual
absActual <- abs(actual)
#### Calculate absError/absActual
ErrAct <- absError/absActual
#### Calculate MAPE: sum(ErrAct) *(100/n)
MAPE <- sum(ErrAct) * (100/n)
MAPE <- data.frame(MAPE)
MAPE

#Construct histogram
library(devtools)
library(easyGgplot2)

Model <- c("TI-SVR", "SVR-rVMF", "SVR-TI-rVMF")
maeee <- c(1005.8640, 1502.8970, 748.4339)
rmseee <- c(1330.1570, 1687.0840, 993.6821)
nmrse <- c(0.1230, 0.1560, 0.0919)
mapee <- c(0.0243, 4.2626, 1.7828)
model.hist <- data.frame(Model, maeee, rmseee, nmrse, mapee)

par(mfrow=c(1,2))
library(RColorBrewer)
require(graphics)
##MAE
#boxplot.method.millet.rmse<-read.delim(file.choose(), header=T,sep="\t") 
#M.rmse<- boxplot.method.millet.rmse[,2]
barplot(maeee, names.arg ="MAE", beside=TRUE,
        col=brewer.pal(n = 7, name = "RdBu"),
        xlab="", ylab="Performance", ylim=c(0.00,2000))
legend("topright", #set the x and y position of the legend.
       yjust=1,
       legend=c("SVR-TI","SVR-rVMF","SVR-TI-rVMF"),
       cex=.6,
       fill=brewer.pal(n = 7, name = "RdBu"
       )
)

##RMSE
barplot(rmseee, names.arg ="RMSE", beside=TRUE,
        col=brewer.pal(n = 6, name = "RdBu"),
        xlab="", ylab="Performance (t/ha)", ylim=c(0.00,2200))
legend("topright", #set the x and y position of the legend.
       yjust=1,
       legend=c("SVR-TI","SVR-rVMF","SVR-TI-rVMF"),
       cex=.6,
       fill=brewer.pal(n = 7, name = "RdBu"
       )
)

par(mfrow=c(1,2))
##nmrse
barplot(nmrse, names.arg ="NMRSE", beside=TRUE,
        col=brewer.pal(n = 6, name = "RdBu"),
        xlab="", ylab="Performance (t/ha)", ylim=c(0.00,0.30))
legend("topright", #set the x and y position of the legend.
       yjust=1,
       legend=c("SVR-TI","SVR-rVMF","SVR-TI-rVMF"),
       cex=.6,
       fill=brewer.pal(n = 7, name = "RdBu"
       )
)


##mape
barplot(mapee, names.arg ="MAPE", beside=TRUE,
        col=brewer.pal(n = 6, name = "RdBu"),
        xlab="", ylab="Performance (t/ha)", ylim=c(0.00,6.00))
legend("topleft", #set the x and y position of the legend.
       yjust=1,
       legend=c("SVR-TI","SVR-rVMF","SVR-TI-rVMF"),
       cex=.6,
       fill=brewer.pal(n = 7, name = "RdBu"
       )
)



# Create a density plot
pred_data <- read.csv(file.choose(), header=TRUE)  # predicted testing data
pred_data <- pred_data[,2]
orig_data <- read.csv(file.choose(), header=TRUE)  # original testing data
orig_data <- orig_data[,2]

library(EnvStats)
# Big bandwidth
plot(density(pred_data, bw = 5000), lwd = 3,
     col = "steelblue", main = "", lty = "dashed",
     xlab="Closing Price/USD", ylab="Probability density")

abline(v = max(orig_data),                # Add vertical line
       col = "red",            # Modify color
       #lty = "dashed",         # Modify line type
       lwd = 3)

abline(v = min(orig_data),                # Add vertical line
       col = "red",            # Modify color
       #lty = "dashed",         # Modify line type
       lwd = 3)










## train random forest model 
#Code 1

# names of features
features <- setdiff(names(train.ti), "x")
features
set.seed(123)
m2 <- tuneRF(
  x          = train.ti[features],
  y          = train.ti$x,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

# randomForest speed
system.time(
  ames_randomForest <- randomForest(
    formula = x ~ ., 
    data    = train.ti, 
    ntree   = 500,
    mtry    = floor(length(features) / 3)
  )
)

# ranger speed
system.time(
  ames_ranger <- ranger(
    formula   = x ~ ., 
    data      = train.ti, 
    num.trees = 500,
    mtry      = floor(length(features) / 3)
  )
)


# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  # train model
  model <- ranger(
    formula         = Sale_Price ~ ., 
    data            = ames_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

















































