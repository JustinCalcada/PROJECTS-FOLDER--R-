##' MATH 3333 FINAL PROJECT ##
#'
### Photograph examples

## Read in the library and metadata
library(jpeg)
library(caret)
pm <- read.csv("C:/Users/Justin A. Calcada/Downloads/photoMetaData.csv")
n <- nrow(pm)
set.seed(1)
trainFlag <- (runif(n) > 0.8)
y <- as.numeric(pm$category == "outdoor-day")


X <- matrix(NA, ncol=3, nrow=n)
for (j in 1:n){
  img <- readJPEG(paste0("C:\\Users\\Justin A. Calcada\\Downloads\\columbiaImages\\columbiaImages\\",pm$name[j]))
  X[j,] <- apply(img,3,median)
  print(sprintf("%03d / %03d", j, n))
}
X

# build a glm model on these median values
out <- glm(y ~ X, family=binomial, subset=trainFlag)
out$iter
summary(out)

# How well did we do?
pred_log <- 1 / (1 + exp(-1 * cbind(1,X) %*% coef(out)))
y[order(pred_log)]
y[!trainFlag][order(pred_log[!trainFlag])]

mean((as.numeric(pred_log > 0.5) == y)[trainFlag])
mean((as.numeric(pred_log > 0.5) == y)[!trainFlag])

CF_log <- confusionMatrix(factor(y[!trainFlag][order(pred_log[!trainFlag])]),factor(y[!trainFlag]))
#' 
#' The misclassification rate here is given by the "1 - Accuracy", where the value
#' for Accuracy comes from the output of the Confusion Matrix labelled by CF_log.
#' Therefore, the misclassification rate here is:
#' 
#' misclassification rate = 1 - Accuracy = 1 - 0.6107 = 0.3893
#' 
# Using a confusion matrix to get specificity and sensitivity analysis

## ROC curve
roc_log <- function(y, pred_log) {
  alpha <- quantile(pred_log, seq(0,1,by=0.01))
  N <- length(alpha)
  
  sens <- rep(NA,N)
  spec <- rep(NA,N)
  for (i in 1:N) {
    predClass <- as.numeric(pred_log >= alpha[i])
    sens[i] <- sum(predClass == 1 & y == 1) / sum(y == 1)
    spec[i] <- sum(predClass == 0 & y == 0) / sum(y == 0)
  }
  return(list(fpr=1- spec, tpr=sens))
}



r_log <- roc_log(y[!trainFlag], pred_log[!trainFlag])
plot(r_log$fpr, r_log$tpr, xlab="false positive rate", ylab="true positive rate", type="l", main = "ROC curve for basic \n logistic regression")
abline(0,1,lty="dashed")

# auc
auc_log <- function(r_log) {
  sum((r_log$fpr) * diff(c(0,r_log$tpr)))
}
glmAuc <- auc(r_log)
glmAuc

#########################
#########################
# SVM METHOD
library("e1071")
library(jpeg)
pm <- read.csv("C:/Users/Justin A. Calcada/Downloads/photoMetaData.csv")
n <- nrow(pm)

x<-array(dim=c(800,4*7*3))
newimage<-array(dim=c(400,700,3))
set.seed(1)
for (m in 1:800){
  print(m)
  img <- readJPEG(paste0("C:\\Users\\Justin A. Calcada\\Downloads\\columbiaImages\\columbiaImages\\",pm$name[m]))
  if(nrow(img[,,1])>=700){
    newimage[,,1]<-t(img[,,1])[1:400,1:700]
    newimage[,,2]<-t(img[,,2])[1:400,1:700]
    newimage[,,3]<-t(img[,,3])[1:400,1:700]
  } else {
    
    newimage[,,1]<-(img[,,1])[1:400,1:700]
    newimage[,,2]<-(img[,,2])[1:400,1:700]
    newimage[,,3]<-(img[,,3])[1:400,1:700]
  }
  features<-array(dim=c(4,7,3))
  
  for (k in 1:3){
    for (i in 1:4){
      for (j in 1:7){
        # (i,j)th block
        
        features[i,j,k]<-mean(newimage[(100*(i - 1) + 1):(100*i),(100*(j - 1) + 1):(100*j),k])
        #print(c(i,j,k))
        
      }
    }
  }
  x[m,] <- as.vector(features)
  }
head(x)
y <- as.numeric(pm$category == "outdoor-day")
new_images <- cbind(y, x)

svm_model1 <- svm(y~., data = new_images[trainFlag,]) 
summary(svm_model1)

pred_svm <- predict(svm_model1,x[!trainFlag,])
system.time(pred_svm)
table(pred_svm,y[!trainFlag])
CF_svm <- confusionMatrix(factor(y[!trainFlag][order(pred_svm[!trainFlag])]),factor(y[!trainFlag]))
#'
#' The misclassification rate here is given by the "1 - Accuracy", where the value
#' for Accuracy comes from the output of the Confusion Matrix labelled by CF_svm.
#' Therefore, the misclassification rate here is:
#' 
#' misclassification rate = 1 - Accuracy = 1 - 0.6837 = 0.3163
#' 
roc_svm <- function(y, pred_svm) {
  alpha <- quantile(pred_svm, seq(0,1,by=0.01))
  N <- length(alpha)
  
  sens <- rep(NA,N)
  spec <- rep(NA,N)
  for (i in 1:N) {
    predClass <- as.numeric(pred_svm >= alpha[i])
    sens[i] <- sum(predClass == 1 & y == 1) / sum(y == 1)
    spec[i] <- sum(predClass == 0 & y == 0) / sum(y == 0)
  }
  return(list(fpr=1- spec, tpr=sens))
}



r_svm <- roc_svm(y[!trainFlag], pred_svm)
plot(r_svm$fpr, r_svm$tpr, xlab="false positive rate", ylab="true positive rate", type="l", main = "ROC curve for SVM")
abline(0,1,lty="dashed")

# auc
auc_svm <- function(r_svm) {
  sum((r_svm$fpr) * diff(c(0,r_svm$tpr)))
}
svmAuc <- auc_svm(r_svm)
svmAuc
#'
#' After developing the code for SVM analysis, we see that the sensitivity and specificity 
#' when using SVM are higher than when using the basic logistic regression method,
#' so is the AUC, along with the misclassification rate being lower, which establishes
#' the idea that SVM classifies the images more efficiently and accurately.
CF_log
# Confusion Matrix and Statistics
#
#            Reference
# Prediction   0   1
#           0 312 115
#           1 115 103
#
# Accuracy : 0.6434          
# 95% CI : (0.6051, 0.6804)
# No Information Rate : 0.662           
# P-Value [Acc > NIR] : 0.8509          
#
# Kappa : 0.2032          
#
# Mcnemar's Test P-Value : 1.0000          
#                                          
#             Sensitivity : 0.7307          
#             Specificity : 0.4725          
#          Pos Pred Value : 0.7307          
#          Neg Pred Value : 0.4725          
#              Prevalence : 0.6620          
#          Detection Rate : 0.4837          
#    Detection Prevalence : 0.6620          
#       Balanced Accuracy : 0.6016          
#                                          
#        'Positive' Class : 0  
CF_svm
#
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction   0   1
#           0 331  96
#           1  96 122
# 
# Accuracy : 0.7023          
# 95% CI : (0.6654, 0.7374)
# No Information Rate : 0.662           
# P-Value [Acc > NIR] : 0.01618         
#
# Kappa : 0.3348          
#
# Mcnemar's Test P-Value : 1.00000         
#                                          
#             Sensitivity : 0.7752          
#             Specificity : 0.5596          
#          Pos Pred Value : 0.7752          
#          Neg Pred Value : 0.5596          
#              Prevalence : 0.6620          
#          Detection Rate : 0.5132          
#    Detection Prevalence : 0.6620          
#       Balanced Accuracy : 0.6674          
#                                           
#         'Positive' Class : 0 
glmAuc
# > glmAuc
# [1] 0.8176095
svmAuc
# > svmAuc
# [1] 0.8332939

