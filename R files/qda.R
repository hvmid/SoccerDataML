library(MASS)
library(MLmetrics)

team <- read.csv("teamAttrib.csv", stringsAsFactors=TRUE)
defenceTeamWidthClass <- team[,match("defenceTeamWidthClass",names(team))]
teamNumeric<-team[,-c(1,2,3,4,6,8,10,11,13,15,17,18,20,22,24,25,26)]
binded <- cbind(defenceTeamWidthClass,teamNumeric)
bindedrem <- binded[,-2] # remove defenceTeamWidth numeric

ind<- sample(1:nrow(bindedrem),1000)
train <- bindedrem[ind,]
test <- bindedrem[-ind,]

tdwqda <- qda(defenceTeamWidthClass ~ bindedrem[,-9]$defencePressure + bindedrem[,-9]$defenceAggression,data=bindedrem[,-9], CV=TRUE) #Built in LOOCV

ct <- table(defenceTeamWidthClass, tdwqda$class)
diag(prop.table(ct, 1))
ct
# Misclassification Rate from built in CV (jacknife LOOCV)
mis.cv<- 1- sum(diag(prop.table(ct)))
mis.cv

F1_Score(as.numeric(tdwqda$class),as.numeric(defenceTeamWidthClass))

LogLoss <- function(pred, res){ 
  #From https://rstudio-pubs-static.s3.amazonaws.com/157427_74913a13c3254d128bc69937434fbfa8.html
  (-1/length(pred)) * sum (res * log(pred) + (1-res)*log(1-pred))
}

#Log loss score
LogLoss(abs((as.numeric(tdwqda$class)-0.01))/sum(as.numeric(tdwqda$class)), 
        as.numeric(defenceTeamWidthClass)/sum(as.numeric(defenceTeamWidthClass)))
