# libraries
require(ggplot2)
require(extrafont)
require(reshape2)
require(lubridate)
require(corrplot)
require(e1071)
require(dplyr)
require(stringr)
require(RJSONIO)
require(httr)
require(strucchange)
require(ROCR)
require(randomForest)
require(MASS)
require(snow)
require(parallel)

Join3 <- read.csv("Join3.csv")

cl <- makeCluster(c(rep("localhost",4)),type="SOCK")
#ignore <- clusterEvalQ(cl,{library(ROCR); library(e1071)" NULL"})

worker.init <- function(packages) {  for (p in packages) {    library(p, character.only=TRUE)  }   } 

ignore <- clusterCall(cl, worker.init, c('MASS',  'ROCR','e1071'))

out <- clusterApply(cl,1:100,function(iter,Join3){
  
  ans <- sapply(iter,function(i,Join3){
    res <- data.frame(Logit=NA,SVM=NA)
    
    ID <- grep(TRUE,!is.na(apply(Join3[,c("IsUnpaid","emp","est","customer_credit_score","customer_age","AgeSq",
                                          "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")],1,sum)))
    modelData <- Join3[ID,]
    
    trainID <- sample(1:nrow(modelData),size=round(nrow(modelData)*0.7))
    
    trainData <- modelData[trainID,]
    testData <- modelData[-trainID,]
    
    GLMmod2 <- glm(IsUnpaid~emp+est+customer_credit_score+customer_age+AgeSq+CSbreak+order_amount+Science_Val+Education_Val,
                   family=binomial(link='logit'),data=trainData)
    #summary(GLMmod2)
    
    pr2 <- prediction(predict(GLMmod2,newdata = testData,type="response"), testData$IsUnpaid)
    
    prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
    #plot(prf2)
    
    auc2 <- performance(pr2, measure = "auc")
    auc2 <- auc2@y.values[[1]]
    res[1] <- auc2
    
    svmfit <- svm(IsUnpaid~emp+est+customer_credit_score+customer_age+AgeSq+CSbreak+order_amount+Retail_Val+Science_Val+Education_Val+HC_Val+Arts_Val,data=trainData,kernel="radial")
    
    pr3 <- prediction(predict(svmfit,newdata = testData,type="response"), testData$IsUnpaid)
    
    prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
    #plot(prf3)
    
    auc3 <- performance(pr3, measure = "auc")
    auc3 <- auc3@y.values[[1]]
    res[2] <- auc3
    
    return(res)
  },Join3)
  
  return(ans)
  
},Join3)

stopCluster(cl)

ClassCV <- read.csv("CVclassAUC.csv")[-1,-1]

CVdata <- matrix(unlist(out),ncol=2,byrow=TRUE)
CV.df <- data.frame(values=c(CVdata[,1],CVdata[,2],ClassCV[,"RF"],ClassCV[,"GBT"]),
                    type=c(rep("Logit",nrow(CVdata)),rep("Support\nVector\nMachine",nrow(CVdata)),
                           rep("Random\nForest",nrow(CVdata)),rep("Gradient\nBoosted\nTrees",nrow(CVdata))))
CV.df$type <- factor(as.character(CV.df$type),levels=unique(CV.df$type)[c(1,2,3,4)])

ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA7_CVboxplot.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(CV.df,aes(x=type,y=values,fill=type)) +
  geom_boxplot(size=2) +
  geom_point(size=8) +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        panel.border=element_rect(color="grey50",linetype="solid",size=1),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_text(angle=0,hjust=0.5,face="plain",size=Sinfo-10),
        axis.text.y=element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="none",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  ylab("Area Under Curve") +
  xlab("Model") +
  #scale_x_continuous(labels=function(X){paste(X*100,"%",sep="")}) + 
  scale_y_continuous(labels=function(X){paste(X*100,"%",sep="")}) +
  scale_fill_manual(values=c(rgb(0, 123/360, 255/360, 0.5),rgb(85/360, 93/360, 102/360, 0.5),"darkorange","darkgreen"))

dev.off()


ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA7_CVviolin.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(CV.df,aes(x=type,y=values,fill=type)) +
  geom_violin(color=NA,alpha=0.75) +
  facet_wrap(~type,nrow=1,scales="free_x") +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        panel.border=element_rect(color="grey50",linetype="solid",size=1),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_blank(), #element_text(angle=0,hjust=0.5,face="plain",size=Sinfo-10),
        axis.text.y=element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="none",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  ylab("Area Under Curve") +
  xlab("Model") +
  #scale_x_continuous(labels=function(X){paste(X*100,"%",sep="")}) + 
  scale_y_continuous(labels=function(X){paste(X*100,"%",sep="")}) +
  scale_fill_manual(values=c(rgb(0, 123/360, 255/360, 0.75),rgb(85/360, 93/360, 102/360, 0.75),"darkorange","darkgreen"))

dev.off()

CVreg <- read.csv("CVregR2.csv")[-1,-1]
CVgam <- read.csv("CVgam.csv")[-1,-1]
# CVreg[grep(TRUE,CVreg[,1]<0),1] <- 0
# CVreg[grep(TRUE,CVreg[,2]<0),2] <- 0
# CVgam[grep(TRUE,CVgam<0)] <- 0

CV2.df <- data.frame(values=c(CVreg[,1],CVreg[,2],CVgam),
                    type=c(rep("Random\nForest",nrow(CVreg)),rep("Gradient\nBoosted\nTrees",nrow(CVreg)),
                           rep("Generalized\nAdditive\nModel",length(CVgam))),
                    AddsValue=c((CVreg[,1]>=0),(CVreg[,2]>=0),(CVgam>=0)))
CV2.df$AddsValue[grep(TRUE,CV2.df$AddsValue)] <- "Variance Explained\nR-squared"
CV2.df$AddsValue[grep(FALSE,CV2.df$AddsValue)] <- "How Wrong Can It Go?\nNegative R-Squared"
CV2.df$type <- factor(as.character(CV2.df$type),levels=unique(CV2.df$type)[c(1,2,3)])
CV2.df$AddsValue <- factor(as.character(CV2.df$AddsValue),levels=unique(CV2.df$AddsValue)[c(2,1)])

ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA8_CVviolin2.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(CV2.df,aes(x=type,y=values,fill=type)) +
  geom_violin(color=NA,alpha=0.75) +
  facet_grid(AddsValue~type,scales="free") +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        panel.border=element_rect(color="grey50",linetype="solid",size=1),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_blank(), #element_text(angle=0,hjust=0.5,face="plain",size=Sinfo-10),
        axis.text.y=element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="none",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text.x=element_text(family="axiformaextrabold",face="bold",size=rel(3.5)),
        strip.text.y=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  ylab("R-squared") +
  xlab("Model") +
  #scale_x_continuous(labels=function(X){paste(X*100,"%",sep="")}) + 
  scale_y_continuous(labels=function(X){paste(X*100,"%",sep="")}) +
  scale_fill_manual(values=c(rgb(0, 123/360, 255/360, 0.75),rgb(85/360, 93/360, 102/360, 0.75),"darkorange","darkgreen"))

dev.off()