require(SparkR)
require(ROCR)
require(readr)
require(glmnet)

IterMap <- data.frame(Iteration=c(1:100),Input=c(1:100))

sc <- sparkR.session(master = "mesos://zk://EDserverUK1:2181,EDserverUK5:2182,EDserverUK8:2181/mesos",
                     sparkConfig = list(spark.driver.memory="16g",spark.executor.memory="16g"))

IterRDD <- createDataFrame((IterMap),numPartitions = nrow(IterMap))

results <- gapply(IterRDD,IterRDD$Iteration,
                  function(key,Iter){
                    
                    #libraries
                    require(SparkR)
                    require(ROCR)
                    require(readr)
                    require(glmnet)
                    
                    alphas <- seq(0,1,by=0.05)
                    Join3 <- data.frame(read_csv("/mirror/Join3.csv"))[,-1]
                    
                    ID3 <- grep(TRUE,!is.na(apply(Join3[,c("AmountWtd","emp","est","customer_credit_score","customer_age","AgeSq",
                                                           "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")],1,sum)))
                    
                    
                    dataset <- Join3[ID3,c("AmountWtd","emp","est","customer_credit_score","customer_age","AgeSq",
                                           "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")]
                    
                    trainID <- base::sample(1:nrow(dataset),size=round(nrow(dataset)*0.7))
                    
                    trainData <- dataset[trainID,]
                    testData <- dataset[-trainID,]
                    
                    # run GAM
                    ID <- grep(TRUE,colnames(trainData) == "AmountWtd")
                    X <- trainData[,-ID]
                    Y <- trainData[,ID]
                    mses <- Ldas <- rep(NA,length(alphas))
                    # run glm
                    cvfits <- NA
                    for(i in 1:length(alphas)){
                      try(cvfits <- cv.glmnet(as.matrix(X),Y,alpha=alphas[i],nfolds=round(nrow(X)*0.75),
                                              group=FALSE),silent=TRUE)
                      if(!is.na(cvfits)){
                        ID <- max(c(grep(TRUE,(cvfits$cvm) == min(cvfits$cvm)),
                                    min(grep(TRUE,cvfits$glmnet.fit[[3]] > 2))))
                        Ldas[i] <- cvfits$lambda[ID]
                        mses[i] <- (cvfits$cvm)[ID]
                      }
                      
                    }
                    if(!is.na(cvfits)[1]){
                      ## find model parameters
                      ID <- grep(TRUE,mses == min(mses))
                      ModAlpha <- alphas[ID]
                      ModLda <- Ldas[ID]
                      ModMses <- mses[ID]
                      tmpENmodel <- glmnet(x=as.matrix(X),y=Y,family="gaussian",alpha=ModAlpha,lambda=ModLda)
                      tmpENcoef <- coef(tmpENmodel)
                      tmpENres <- Y - ((as.matrix(X)%*%as.matrix(tmpENcoef)[-1,] +
                                          as.matrix(tmpENcoef)[1,]))
                    }
                    
                    tmpPreds <- predict(tmpENmodel, newx=as.matrix(testData[,-ID]),type="response")
                      
                    SSres <- sum((as.numeric(tmpPreds)-testData[,"AmountWtd"])^2)
                    r2GAM <- 1-(SSres/sum((testData[,"AmountWtd"]-mean(testData[,"AmountWtd"]))^2))
                    
                    return(data.frame(Out=r2GAM))
                    
                  },structType(structField("Out","double")))

RunTest <- collect(results)


sparkR.session.stop()

write.csv(RunTest,"CVglm.csv")
