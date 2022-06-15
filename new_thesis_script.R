library(openxlsx)
library(e1071) # svm
library(GLDEX) # which.na
library(caret) # varImp
library(leaps) # regsubsets()
library(glmnet) # Lasso/Ridge
library(MASS) # LDA/QDA
library(dplyr) # Both this and stringr used to get factors as a formula for LASSO.
library(stringr)
library(kernlab) # GPs for classification with gausspr(...)
library(pROC)
library(mclust) # For clPairs(...)
library(scales) # for alpha
library(MLeval) # for evalm(...)
library(CAST) # for bss(...)
# https://rdrr.io/cran/kernlab/man/gausspr.html reference for kernlab

data <- read.xlsx("2019-summer-match-data-clean.xlsx", 1) # Laptop updated.

data <- as.data.frame(unclass(data))
data <- data[,-c(1:5)]
data <- data[,-46]

# Data for players only WITHOUT ROW THAT IS TEAM
data <- na.omit(data)
data.players <- data[which(data$player != "Team"),]

# how to incorporate csd in a row-wise manner? csdat10_TopB? Only need for one color.

data.match <- data.frame(resultB = NA, 
                         #topB = NA, topR = NA, jgB = NA, jgR = NA, midB = NA, midR = NA,
                         #adcB = NA, adcR = NA, suppB = NA, suppR = NA, 
                         topB_gdat10 = NA, topB_gdat15 = NA,
                         jgB_gdat10 = NA, jgB_gdat15 = NA, midB_gdat10 = NA, midB_gdat15 = NA, 
                         adcB_gdat10 = NA, adcB_gdat15 = NA, topB_xpdat10 = NA, jgB_xpdat10 = NA, 
                         midB_xpdat10 = NA, adcB_xpdat10 = NA, suppB_xpdat10 = NA, topB_csdat10 = NA,
                         topB_csdat15 = NA, jgB_csdat10 = NA, jgB_csdat15 = NA, midB_csdat10 = NA, 
                         midB_csdat15 = NA, adcB_csdat10 = NA, adcB_csdat15 = NA, 
                         top_fbB = NA, jg_fbB = NA, mid_fbB = NA, adc_fbB = NA, supp_fbB = NA,
                         top_fdB = NA, jg_fdB = NA, mid_fdB = NA, adc_fdB = NA, supp_fdB = NA,
                         ftB = NA, #fdB = NA,
                         stringsAsFactors=F)

gd10.ind <- grep("gdat10", colnames(data.players))
gd15.ind <- grep("gdat15", colnames(data.players))
xpd10.ind <- grep("xpdat10", colnames(data.players))
csd10.ind <- grep("csdat10", colnames(data.players))
csd15.ind <- grep("csdat15", colnames(data.players))
fb <- grep("fb", colnames(data.players))[1]
fd <- grep("fd", colnames(data.players))[1]
ft <- grep("ft", colnames(data.players))[1]

ind <- 1
for(i in 1:(nrow(data.players)/10)){
  data.match[i, 1] <- as.character(data.players[ind, 16])   # Result
  #data.match[i, 2] <- as.character(data.players[ind, 7])    # Top Blue
  #data.match[i, 3] <- as.character(data.players[ind+5, 7])
  #data.match[i, 4] <- as.character(data.players[ind+1, 7])  # Jg Blue
  #data.match[i, 5] <- as.character(data.players[ind+6, 7])
  #data.match[i, 6] <- as.character(data.players[ind+2, 7])  # Mid Blue
  #data.match[i, 7] <- as.character(data.players[ind+7, 7])  
  #data.match[i, 8] <- as.character(data.players[ind+3, 7])  # ADC Blue
  #data.match[i, 9] <- as.character(data.players[ind+8, 7])
  #data.match[i, 10] <- as.character(data.players[ind+4, 7]) # Supp Blue
  #data.match[i, 11] <- as.character(data.players[ind+9, 7])
  
  data.match[i, 12-10] <- data.players[ind, gd10.ind]                # Top Blue GD at 10
  data.match[i, 13-10] <- data.players[ind, gd15.ind]                # Top Blue GD at 15
  data.match[i, 14-10] <- data.players[ind+1, gd10.ind]              # Jg Blue GD at 10
  data.match[i, 15-10] <- data.players[ind+1, gd15.ind]              # Jg Blue GD at 15
  data.match[i, 16-10] <- data.players[ind+2, gd10.ind]              # Mid Blue GD at 10
  data.match[i, 17-10] <- data.players[ind+2, gd15.ind]              # Mid Blue GD at 15
  data.match[i, 18-10] <- data.players[ind+3, gd10.ind]              # ADC Blue GD at 10
  data.match[i, 19-10] <- data.players[ind+3, gd15.ind]              # ADC Blue GD at 15
  
  data.match[i, 20-10] <- data.players[ind, xpd10.ind]                # Top Blue XPD at 10
  data.match[i, 21-10] <- data.players[ind+1, xpd10.ind]              # Jg Blue XPD at 10
  data.match[i, 22-10] <- data.players[ind+2, xpd10.ind]              # Jg Blue XPD at 10
  data.match[i, 23-10] <- data.players[ind+3, xpd10.ind]              # Jg Blue XPD at 10
  data.match[i, 24-10] <- data.players[ind+4, xpd10.ind]              # Jg Blue XPD at 10
  
  data.match[i, 25-10] <- data.players[ind, csd10.ind]                # Top Blue CSD at 10
  data.match[i, 26-10] <- data.players[ind, csd15.ind]                # Top Blue CSD at 15
  data.match[i, 27-10] <- data.players[ind+1, csd10.ind]              # Jg Blue CSD at 10
  data.match[i, 28-10] <- data.players[ind+1, csd15.ind]              # Jg Blue CSD at 15
  data.match[i, 29-10] <- data.players[ind+2, csd10.ind]              # Mid Blue CSD at 10
  data.match[i, 30-10] <- data.players[ind+2, csd15.ind]              # Mid Blue CSD at 15
  data.match[i, 31-10] <- data.players[ind+3, csd10.ind]              # ADC Blue CSD at 10
  data.match[i, 32-10] <- data.players[ind+3, csd15.ind]              # ADC Blue CSD at 15
  
  # Need to actually do these by role as well...
  data.match[i, 33-10] <- as.character(data.players[ind, fb])  # First Blood Blue
  data.match[i, 34-10] <- as.character(data.players[ind+1, fb])
  data.match[i, 35-10] <- as.character(data.players[ind+2, fb])
  data.match[i, 36-10] <- as.character(data.players[ind+3, fb])
  data.match[i, 37-10] <- as.character(data.players[ind+4, fb])
  
  data.match[i, 38-10] <- as.character(data.players[ind, fd])  # First Dragon Blue
  data.match[i, 39-10] <- as.character(data.players[ind+1, fd])
  data.match[i, 40-10] <- as.character(data.players[ind+2, fd])
  data.match[i, 41-10] <- as.character(data.players[ind+3, fd])
  data.match[i, 42-10] <- as.character(data.players[ind+4, fd])
  
  # First turret only needs the side, not individual players.
  data.match[i, 43-10] <- as.character(data.players[ind, ft])  # First Turret Blue. 
  
  ind <- ind + 10
}
data.match <- na.omit(data.match) # Lots of NAs in certain regions, dropping those matches period.
data.match

# Making character columns into factors...
data.match[, 1] <- as.factor(data.match[, 1])
#for(i in 1:11){
#  data.match[, i] <- as.factor(data.match[, i])
#}

for(i in 23:ncol(data.match)){
  data.match[, i] <- as.factor(data.match[, i])
}

for(i in 28:32){
  data.match[, i] <- as.numeric(data.match[, i])
}

data.match$fdB <- 0
for(i in 1:nrow(data.match)){
  data.match[i, 34] <- sum(data.match[i, 28:32])
}
data.match[,34] <- data.match[,34] - 5
data.match[,34] <- as.factor(data.match[,34])
data.match <- data.match[, -c(28:32)]
data.match

levels(data.match$resultB) <- c("Lose", "Win")

# --- Training/Testing Set Split --- #
set.seed(2)
train <- sample(1:nrow(data.match), size=floor(.8*nrow(data.match)))
test <- -train

data.train <- data.match[train,]
data.test <- data.match[test,]








# Plotting Function
min(data.train$midB_gdat15)
max(data.train$midB_gdat15)
min(data.train$adcB_gdat15)
max(data.train$adcB_gdat15)


min(data.test$midB_gdat15)
max(data.test$midB_gdat15)

min(data.test$adcB_gdat15)
max(data.test$adcB_gdat15)

# Prediction plots
plot_fn <- function(fit, dat){
  #predictions <- rep("0", length(data.test$resultB))
  #px <- predict(m2, data.test, type="response") 
  #predictions[px > 0.5] <- "1"
  
  predictions <- predict(fit, newdata=dat)
  correct.ind <- which(predictions == dat$resultB)
  wrong.ind <- which(predictions != dat$resultB)
  
  print(confusionMatrix(predict(fit, newdata=data.train), reference=data.train$resultB))
  
  
  
  cat("Test Set Confusion Matrix and Accuracy: \n")
  print(confusionMatrix(predict(fit, newdata=data.test), reference=data.test$resultB))
  
  
  
  
  
  
  plot(1, type="n",
       xlab="", ylab="",
       main="", cex.axis=1.2,
       xlim=c(-2900,3500), ylim=c(-3600, 3800))
  
  mtext("Blue Side ADC Gold Difference at 15 Minutes", side=2, line=2.5, cex=1.6)
  mtext("Blue Side Mid Gold Difference at 15 Minutes", side=1, line=3.0, cex=1.6)
  mtext("Training Set Predictions", side=3, line=2, cex=1.6)
  
  for(i in 1:nrow(dat)){
    check <- match(i, correct.ind)
    wrong <- match(i, wrong.ind)
    if(!is.na(check) & dat[correct.ind[check], "resultB"] == "Win"){
      points(x=dat[correct.ind[check], "midB_gdat15"], y=dat[correct.ind[check], "adcB_gdat15"], pch=21, bg=alpha("skyblue", 0.5))
    }
    
    if(!is.na(check) & dat[correct.ind[check], "resultB"] == "Lose"){
      points(x=dat[correct.ind[check], "midB_gdat15"], y=dat[correct.ind[check], "adcB_gdat15"], pch=21, bg=alpha("salmon", 0.5))
    }
    
    if(!is.na(wrong) & dat[wrong.ind[wrong], "resultB"] == "Win"){
      points(x=dat[wrong.ind[wrong], "midB_gdat15"], y=dat[wrong.ind[wrong], "adcB_gdat15"], pch=21, bg="blue4")
    }
    
    if(!is.na(wrong) & dat[wrong.ind[wrong], "resultB"] == "Lose"){
      points(x=dat[wrong.ind[wrong], "midB_gdat15"], y=dat[wrong.ind[wrong], "adcB_gdat15"], pch=21, bg="red4")
    }
  }
  
  legend("topleft", legend=c("Blue Win, Correct Prediction", "Blue Win, Wrong Prediction", "Red Win, Correct Prediction", "Red Win, Wrong Prediction"), pch=c(21, 21, 21, 21), pt.bg=c(alpha("skyblue", 0.5), "blue4", alpha("salmon", 0.5), "red4"), pt.cex=1, cex=1.15)
}


# Logistic Regression BEGIN

preds <- function(fit){
  predictions <- rep("Lose", length(data.test$resultB))
  px <- predict(fit, data.test, type="response") 
  predictions[px > 0.5] <- "Win"
  
  preds2 <- rep("Lose", length(data.train$resultB))
  px2 <- predict(fit, data.train, type="response")
  preds2[px2 >  0.5] <- "Win"
  
  print(confusionMatrix(as.factor(predictions), reference=data.test$resultB))
  print(confusionMatrix(as.factor(preds2), reference=data.train$resultB))
  
  #cat("Model: ", as.character(fit$formula), "\n\n")
  #cat("Percent Correct Predictions: ", (conf.mat[1,1] + conf.mat[2,2]) / (sum(conf.mat[1:2, 1:2])), "\n") # Percent of correct predictions.
  
  #cat("  Percent Wrong Predictions: ", 1 - mean(as.factor(data.test$result) == predictions), "\n") # error rate iirc?
  
  #cat("\n AIC: ", AIC(fit), "\n")
  
  #proc.obj2 <- roc(data.train$resultB, px2, plot=T, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T, xlim=c(1,0))
  #sens.ci <- ci.se(proc.obj2)
  #plot(proc.obj2, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T, xlim=c(1, 0), add=F, print.auc.cex=1.6, print.auc.adj=c(0.45,0), asp=7.4/11)
  #plot(sens.ci, type="shape", col="lightblue")
  #plot(sens.ci, type="bars")
  
  
  #proc.obj <- roc(data.test$resultB, px, plot=T, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T, xlim=c(1,0))
  #sens.ci <- ci.se(proc.obj)
  #plot(proc.obj, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T, xlim=c(1, 0), add=F, print.auc.cex=1.6, print.auc.adj=c(0.45,0), asp=7.4/11)
  #plot(sens.ci, type="shape", col="lightblue")
  #plot(sens.ci, type="bars")
}


# doing 10-fold CV on m1, tr acc 0.7427
caret.model <- train(resultB~jgB_gdat10 + jgB_gdat15 + midB_gdat15 + adcB_gdat15 + adcB_csdat10 + fdB, 
                     data=data.train, 
                     trControl=trainControl(method="cv", 
                                            number=10, 
                                            classProbs=T, 
                                            summaryFunction=twoClassSummary, 
                                            savePred=T), 
                     method="glm", 
                     family="binomial")
caret.model

m1_evals <- evalm(caret.model)
m1_evals$roc

m2_evals <- evalm(m2.caret)
m2_evals$roc

# 10 fold accuracy on m2, tr acc = 0.7592
set.seed(1)
m4.caret.roc <- train(resultB ~ jgB_gdat10 + jgB_gdat15 + midB_gdat15 + adcB_gdat15 + suppB_xpdat10 + jgB_csdat15 + adcB_csdat10 + fdB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, summaryFunction=twoClassSummary, savePred=T), method="glm", family="binomial", metric="ROC")
m4.caret.roc

m4_roc_evals <- evalm(m4.caret.roc)
m4_roc_evals$roc
#twoClassSummary(data=data.train, lev=levels(data.train$resultB), model=m4.caret)

predict(m4.caret.roc, newdata=data.test)
confusionMatrix(predict(m4.caret.roc, newdata=data.train), reference=data.train$resultB)

m4.caret.acc <- train(resultB ~ jgB_gdat10 + jgB_gdat15 + midB_gdat15 + adcB_gdat15 + suppB_xpdat10 + jgB_csdat15 + adcB_csdat10 + fdB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="glm", family="binomial", metric="Accuracy")
m4.caret.acc

m4_acc_evals <- evalm(m4.caret.acc)
m4_acc_evals$roc
#twoClassSummary(data=data.train, lev=levels(data.train$resultB), model=m4.caret)

predict(m4.caret.acc, newdata=data.test)
#confusionMatrix(predict(m4.caret.acc, newdata=data.train), reference=data.train$resultB)

coef(m4.caret.roc)
coef(m4.caret.acc)




# bss, don't do this, takes an extremely long time even removing predictors.
bss_mod <- bss(predictors=data.train[,-c(1,2,4,6,8,10:15,17,19,21,23,24,27)], response=data.train[,1], method="glm", family="binomial", metric="Accuracy", trControl=trainControl(classProbs=T, savePred=T))

# Forward subset selection, still takes a long time. 
set.seed(1)
fss_acc <- ffs(predictors=data.train[,-1], response=data.train[,1], method="glm", family="binomial", metric="Accuracy", trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T))
fss_acc
fss_acc_evals <- evalm(fss_acc)
fss_acc_evals$roc
plot_fn(fss_acc, data.train)
plot_fn(fss_acc, data.test)
coef(fss_acc$finalModel) # 11 preds

# model from above, this is m1 in thesis
set.seed(1)
#tr acc = 0.7799 # WINNER LOGISTIC REGRESSION. Mention that FSS on metric of AUC may have stopped too soon.
#te acc = 0.7556
#aucTr  = 0.81(0.77-0.85)
# aucTe = 0.8398 (0.7737 - 0.9059)
fss_acc1 <- train(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, trControl=trainControl(method="cv", number=10, savePred=T, classProbs=T), method="glm", family="binomial", metric="Accuracy")
fss_acc1
evalm(fss_acc1)$roc
evalm(fss_acc1)$stdres
plot_fn(fss_acc1, data.train)
coef(fss_acc1$finalModel)

probs <- predict(fss_roc, data.train, type="prob")[,2]
roc(data.train$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)



set.seed(1)
fss_roc <- ffs(predictors=data.train[,-1], response=data.train[,1], method="glm", family="binomial", metric="ROC", trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T, summaryFunction=twoClassSummary))
fss_roc
fss_roc_evals <- evalm(fss_roc)
plot_fn(fss_acc, data.train)
plot_fn(fss_acc, data.test)
coef(fss_roc$finalModel)
evalm(fss_roc)$roc


#m2 in thesis
set.seed(1)
#tr acc = 0.7647, but fss_roc1 shows slightly different stuff...
#te acc = 0.7259
#aucTr  = 0.83 (0.79-0.87)
#aucTe  = 0.8022 (0.7279 - 0.8765)
fss_roc1 <- train(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, trControl=trainControl(method="cv", number=10, savePred=T, summaryFunction=twoClassSummary, classProbs=T), method="glm", family="binomial", metric="Accuracy")
fss_roc1
evalm(fss_roc1)$stdres
plot_fn(fss_roc1, data.train)

probs <- predict(fss_roc1, data.test, type="prob")[,2]
roc(data.test$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)







# LDA caret

set.seed(1)
#10fold tr acc = 0.7609 # WINNER LDA
#test acc = 0.7556
#auc = 0.83(0.79-0.87)
#TeAuc = 0.8412 (0.7755 - 0.9069)
caret_lda_acc <- train(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="lda", metric="Accuracy")
caret_lda_acc
plot_fn(caret_lda_acc, data.train)
lda_acc_evals <- evalm(caret_lda_acc)
lda_acc_evals$stdres

probs <- predict(caret_lda_acc, data.train, type="prob")[,2]
roc(data.train$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)


set.seed(1)
#10 fold acc = 0.7347
#te acc = 0.7259
#auc = 0.81 (0.77-0.85) X
caret_lda_roc <- train(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="lda", metric="Accuracy")
caret_lda_roc
lda_roc_evals <- evalm(caret_lda_roc)
lda_roc_evals$stdres
plot_fn(caret_lda_roc, data.train)

set.seed(1)
#10fold tr = 0.7423
#test acc = 0.7704
# auc = 0.82(0.78-0.86)
caret_lda_all <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="lda", metric="Accuracy")
caret_lda_all
lda_all_evals <- evalm(caret_lda_all)
lda_all_evals$stdres
plot_fn(caret_lda_all, data.train)









# QDA caret

set.seed(1)
#10fold acc = 0.7144 # WINNER QDA
#te acc = 0.7704
#auc = 0.78 (0.74-0.82)
#teAuc = 0.8443, 0.7785 - 0.91
caret_qda_acc <- train(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="qda", metric="Accuracy")
caret_qda_acc
qda_acc_evals <- evalm(caret_qda_acc)
qda_acc_evals$stdres
plot_fn(caret_qda_acc, data.train)

probs <- predict(caret_qda_acc, data.test, type="prob")[,2]
roc(data.test$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)

set.seed(1)
#10fold acc = 0.6937
#te acc = 0.7259
#auc = 0.77 (0.73 - 0.81)
caret_qda_roc <- train(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="qda", metric="Accuracy")
caret_qda_roc
qda_roc_evals <- evalm(caret_qda_roc)
qda_roc_evals$stdres
plot_fn(caret_qda_roc, data.train)

set.seed(1)
#10fold acc = 0.6192
#te acc = 0.6519
#auc = 0.66 (0.61 - 0.71)
caret_qda_all <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="qda", metric="Accuracy")
caret_qda_all
qda_all_evals <- evalm(caret_qda_all)
qda_all_evals$stdres
plot_fn(caret_qda_all, data.train)









# caret with LASSO

set.seed(1)
# alpha = 0.1, lambda = 0.0040 # WINNER LASSO slightly. Mention how close they were and the AUC was slightly ahead in LASSO2.
#10fold acc = 0.7535
#test acc = 0.7481
#auc = 0.83 (0.79 - 0.87)
#teAuc = 0.8212 (0.75  0.8924)
lasso_acc <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T), method="glmnet", family="binomial", metric="Accuracy")
lasso_acc
las_acc_evals <- evalm(lasso_acc)
las_acc_evals$stdres
plot_fn(lasso_acc, data.train)

probs <- predict(lasso_acc, data.test, type="prob")[,2]
roc(data.test$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)

set.seed(1)
# alpha = 1, lambda = 0.0040
#10fold acc = 0.7501
#test acc = 0.7481
#auc = 0.83 (0.79 - 0.87) 0.83 (0.79 - 0.87)
lasso_roc <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T, summaryFunction=twoClassSummary), method="glmnet", family="binomial", metric="ROC")
lasso_roc
las_roc_evals <- evalm(lasso_roc)
las_roc_evals$stdres
plot_fn(lasso_roc, data.train)
confusionMatrix(lasso_roc)

coef(lasso_acc$finalModel, lasso_acc$bestTune$lambda)
coef(lasso_roc$finalModel, lasso_roc$bestTune$lambda)








# SVMs caret
set.seed(1)
# 10-fold cv TrAcc = 0.7516 with c=16
# TeAcc = 0.7556
# auc = 0.82 (0.78 - 0.86)
svm_lin <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T), method="svmLinear", metric="Accuracy", tuneGrid = expand.grid(C=c(.1, .25, .5, 1, 1.5, 2, 4, 8, 10, 16, 25, 32, 64, 100)))
svm_lin
svm_lin_evals <- evalm(svm_lin)
svm_lin_evals$stdres
plot_fn(svm_lin, data.train)



#another poly svm, use this one.
set.seed(1) #winner sorta
# 10-fold cv tracc = 0.7591 with c=0.1, d=1, scale=1.5. Isn't this just linear kernel? Why dif results...
# TeAcc = 0.7333
# auc = 0.83 (0.79 - 0.87)
# teAuc = 0.8175 (0.7455 - 0.8895)
svm_poly2 <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T), method="svmPoly", metric="Accuracy", tuneGrid=expand.grid(C=c(.1, .25, .5, 1, 1.5, 2, 4, 8, 10, 16, 25, 32, 64, 100), degree=c(1,2,3), scale=c(.001, .01, .1, 1)))
svm_poly2
svm_poly2_evals <- evalm(svm_poly)
svm_poly2_evals$stdres
plot_fn(svm_poly2, data.train)

probs <- predict(svm_poly, data.test, type="prob")[,2]
roc(data.test$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)


#tr acc = 0.7552 with c=100
#tr auc = 0.82
#te acc = 0.7778
# 2nd degree only
set.seed(1)
svm_poly2 <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T), method="svmPoly", metric="Accuracy", tuneGrid=expand.grid(C=c(.1, .25, .5, 1, 1.5, 2, 4, 8, 10, 16, 25, 32, 64, 100), degree=c(2), scale=c(.001, .01, .1, 1)))
svm_poly2
svm_poly2_evals <- evalm(svm_poly2)
svm_poly2_evals$stdres
plot_fn(svm_poly2, data.train)

# tracc = 0.7515
# trauc = 0.81
# teacc = 0.7481
# 3rd degree only
set.seed(1)
svm_poly3 <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T), method="svmPoly", metric="Accuracy", tuneGrid=expand.grid(C=c(.1, .25, .5, 1, 1.5, 2, 4, 8, 10, 16, 25, 32, 64, 100), degree=c(3), scale=c(.001, .01, .1, 1)))
svm_poly3
svm_poly3_evals <- evalm(svm_poly3)
svm_poly3_evals$stdres
plot_fn(svm_poly3, data.train)





set.seed(1)
# 10-fold cv tracc = 0.7050 with c=0.50, sigma = 0.1
# teacc = 0.6963
# auc = 0.74 (0.70 - 0.78)
svm_rad <- train(resultB~., data=data.train, trControl=trainControl(method="cv", number=10, savePredictions=T, classProbs=T), method="svmRadial", metric="Accuracy", tuneGrid=expand.grid(C=c(.1, .25, .5, 1, 1.5, 2, 4, 8, 10, 16, 25, 32, 64, 100), sigma=c(0.1, 0.15, 0.2, 0.25))) #0.5, 1, 2, 3, 4))) # Commented these out because they did horrible.
svm_rad
svm_rad_evals <- evalm(svm_rad)
svm_rad_evals$stdres

plot_fn(svm_rad, data.train)
plot_fn(svm_rad, data.test)









# caret with GPs

# radial kernels

set.seed(1)
# 10fold acc = 0.732
# te acc = 0.733
# auc = 0.80 (0.76 - 0.84)
gp_all_rad <- train(resultB ~ ., data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprRadial", metric="Accuracy", tuneLength=4)
gp_all_rad
evalm(gp_all_rad)$stdres
plot_fn(gp_all_rad, data.train)

#preds from fss on acc
set.seed(1) # winner for all GPs including laplace kernel
# sigma constant at 0.0700
# 10fold acc = 0.7330
# te acc = 0.7852
# auc = 0.81 (0.77 - 0.85)
# te auc = 0.8341 = 0.7655 - 0.9026
gp_rad_acc <- train(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprRadial", metric="Accuracy", tuneLength=4)
gp_rad_acc
evalm(gp_rad_acc)$stdres
plot_fn(gp_rad_acc, data.train)

probs <- predict(gp_rad_acc, data.test, type="prob")[,2]
roc(data.test$resultB, probs, plot=F, smoothed=T, ci=T, ci.alpha=0.95, stratified=F, auc.polygon=T, grid=T, print.auc=T, show.thres=T)

#preds from fss on auc
set.seed(1)
# sigma constant at 0.1552
# 10fold acc = 0.7216
# te acc = 0.7630
# auc = 0.78 (0.74 - 0.82)
gp_rad_roc <- train(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprRadial", metric="Accuracy", tuneLength=4)
gp_rad_roc
evalm(gp_rad_roc)$stdres
plot_fn(gp_rad_roc, data.train)






# laplace kernel GPs
set.seed(1)
# 10fold acc = 0.7090
# te acc = 0.7259
# auc = 0.80 (0.76 - 0.84)
gp_all_lap <- gausspr(resultB ~ ., data=data.train, kernel="laplacedot", type="classification", cross=10)
gp_all_lap
evalm(gp_all_rad)$stdres
plot_fn(gp_all_lap, data.train)

#preds from fss on acc
set.seed(1)
# sigma constant at 0.0864
# 10fold acc = 0.7183
# te acc = 0.7778
# auc = 0.81 (0.77 - 0.85)
gp_lap_acc <- gausspr(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, kernel="laplacedot", type="classification", cross=10)
gp_lap_acc 
evalm(gp_rad_acc)$stdres
plot_fn(gp_lap_acc, data.train)

#preds from fss on auc
set.seed(1)
# sigma constant at 0.1774
# 10fold acc = 0.7275
# te acc = 0.7630
# auc = 0.78 (0.74 - 0.82)
gp_lap_roc <- gausspr(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, kernel="laplacedot", cross=10, type="classification")
gp_lap_roc
evalm(gp_lap_roc)$stdres
plot_fn(gp_lap_roc, data.train)











# Plots for AUC comparisons
bad_model <- train(resultB ~ supp_fbB -1, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="glm", family="binomial", metric="Accuracy")
bad_model
evalm(list1=list(lasso_acc, bad_model), gnames=c("Decent Model", "Bad Model"), fsize=18)$roc

# LR AUCs tr
evalm(list1=list(fss_roc1, fss_acc1), gnames=c("m1", "m2"), fsize=18)$roc

# LDA AUCs tr
evalm(list1=list(caret_lda_acc, caret_lda_roc, caret_lda_all), gnames=c("LDA1", "LDA2", "LDA3"), fsize=18)$roc

# QDA AUCs tr
evalm(list1=list(caret_qda_acc, caret_qda_roc, caret_qda_all), gnames=c("QDA1", "QDA2", "QDA3"), fsize=18)$roc

# LASSO AUCs tr
evalm(list1=list(lasso_acc, lasso_roc), gnames=c("LASSO1", "LASSO2"), fsize=18)$roc

# SVM AUCs tr
evalm(list1=list(svm_poly, svm_poly2, svm_poly3, svm_rad), gnames=c("Linear", "Polynomial, d=2", "Polynomial, d=3", "Radial"), fsize=18)$roc

# GP AUCs tr
evalm(list1=list(gp_rad_acc, gp_rad_roc, gp_all_rad), gnames=c("GP1", "GP2", "GP3"), fsize=18)$roc


df_lr <- predict(fss_roc1, newdata=data.test, type="prob")
df_lr$obs <- data.test$resultB
df_lr$Group <- "m1"

df_lda <- predict(caret_lda_acc, newdata=data.test, type="prob")
df_lda$obs <- data.test$resultB
df_lda$Group <- "LDA1"

df_qda <- predict(caret_qda_acc, newdata=data.test, type="prob")
df_qda$obs <- data.test$resultB
df_qda$Group <- "QDA1"

df_lasso <- predict(lasso_acc, newdata=data.test, type="prob")
df_lasso$obs <- data.test$resultB
df_lasso$Group <- "LASSO1"

df_svm <- predict(svm_poly, newdata=data.test, type="prob")
df_svm$obs <- data.test$resultB
df_svm$Group <- "SVM-Lin"

df_gp <- predict(gp_rad_acc, newdata=data.test, type="prob")
df_gp$obs <- data.test$resultB
df_gp$Group <- "GP1"


#The final plot
evalm(list1=rbind(df_lr, df_lda, df_qda, df_lasso, df_svm, df_gp), 
      gnames=c("m1", "LDA1", "QDA1", "LASSO1", "SVM-Lin", "GP1"),
      fsize=18)$roc


evalm(df)$roc
evalm(df_lda)$roc


evalm(list1=list(df_lr, df_lda, df_qda, df_lasso, df_svm, df_gp), 
      gnames=c("m1", "LDA1", "QDA1", "LASSO1", "SVM-Lin", "GP2-Gau"), 
      fsize=18)

evalm(list1=list(df_lr, df_gp))$roc

list(gp_all_rad$pred, gp_rad_acc$pred, gp_rad_roc$pred)















# linear kernel GPs

set.seed(1)
#trAcc = 0.7854
#teAcc = 0.7630
#trAuc = 0.82 (0.78 - 0.86)
gp_all_lin <- train(resultB ~ ., data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprLinear", metric="Accuracy")
gp_all_lin 
gp_all_lin_evals <- evalm(gp_all_lin)
gp_all_lin_evals$stdres # to get CI for AUC
plot_fn(gp_all_lin, data.train)

#preds from fss on acc
set.seed(1)
# 10fold acc = 0.7553
# te acc = 0.7556
# auc = 0.83 (0.79 - 0.87)
gp_lin_acc <- train(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprLinear", metric="Accuracy")
gp_lin_acc
evalm(gp_lin_acc)$stdres
plot_fn(gp_lin_acc, data.train)

#preds from fss on auc
set.seed(1)
# 10fold acc = 
# te acc = 
# auc = 
gp_lin_roc <- train(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprLinear", metric="Accuracy")
gp_lin_roc
evalm(gp_lin_roc)$stdres
plot_fn(gp_lin_roc, data.train)






# polynomial kernels

set.seed(1)
#trAcc = 0.7461
#teAcc = 0.7778
#trAuc = 0.82(0.78-0.86)
gp_all_poly <- train(resultB ~ ., data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprPoly", metric="Accuracy")
gp_all_poly # degree = 3, scale = 0.01
gp_all_poly_evals <- evalm(gp_all_poly)
gp_all_poly_evals$stdres # to get CI for AUC
plot_fn(gp_all_poly, data.train)

#preds from fss on acc
set.seed(1)
# 10fold acc = 
# te acc = 
# auc = 
gp_poly_acc <- train(resultB ~ jgB_gdat15 + adcB_gdat15 + midB_csdat15 + topB_csdat15 + adcB_csdat15 + midB_gdat15 + fdB + midB_csdat10 + jgB_gdat10 + adcB_xpdat10 + midB_gdat10, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprPoly", metric="Accuracy")
gp_poly_acc
evalm(gp_poly_acc)$stdres
plot_fn(gp_poly_acc, data.train)

#preds from fss on auc
set.seed(1)
# 
# 10fold acc = 
# te acc = 
# auc = 
gp_poly_roc <- train(resultB ~ midB_gdat15 + jgB_gdat15 + adcB_csdat10 + fdB + jgB_gdat10 + suppB_xpdat10 + jg_fbB, data=data.train, trControl=trainControl(method="cv", number=10, classProbs=T, savePred=T), method="gaussprPoly", metric="Accuracy")
gp_poly_roc
evalm(gp_poly_roc)$stdres
plot_fn(gp_poly_roc, data.train)










