library(neuralnet)
library(cluster)
library(mclust)
library(ggplot2)
library(factoextra)
library(MASS)
library(FNN)
library("plot3D")

setwd("~/Desktop/UBC/DATA 311/Archive-1/Data/")
team <- read.csv("teamAttrib.csv", stringsAsFactors=TRUE)
teamNumeric<-team[,-c(1,2,3,4,6,8,10,11,13,15,17,18,20,22,24,25, 26)]
pc <- prcomp(as.matrix(teamNumeric), scale.=TRUE)

biplot(pc)
summary(pc)

plot(pc, type='l')
abline(a=1, b=0, col='red')
pc$rotation[,1:3]

df2<-data.frame(pc$x[,1:2])
df2[,3]<-team$GoalRatio

teamlm<-lm(V3 ~., data = df2)
summary(teamlm)
mean((predict(teamlm)-df2$V3)^2)
scatter3D(df2$V3, df2$PC2, df2$PC1,ticktype="detailed", pch=16, d=2)

cvlm <- list()
msecv<-NA

for(i in 1:nrow(df2)){
  cvV3<-df2$V3[-i]
  cvP1<-df2$PC1[-i]
  cvP2<-df2$PC2[-i]
  cvlm[[i]]<-lm(cvV3~cvP1+cvP2)
  msecv[i]<-(predict(cvlm[[i]], newdata=data.frame(cvP1=df2$PC1[i], cvP2=df2$PC2[i])) - df2$V3[i])^2
}

mean(msecv)



# #Log loss score
# LogLoss <- function(pred, res){ 
#   #From https://rstudio-pubs-static.s3.amazonaws.com/157427_74913a13c3254d128bc69937434fbfa8.html
#   (-1/length(pred)) * sum (res * log(pred) + (1-res)*log(1-pred))
# }
# 
# 
# LogLoss(abs((as.numeric(as.vector(na.omit(teamlda$class))))/sum(as.numeric(as.vector(na.omit(teamlda$class))))), 
#         as.numeric(df2$V4)/sum(as.numeric(df2$V4)))
# 
# 
