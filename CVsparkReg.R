require(SparkR)
require(ROCR)
require(readr)

Join3 <- data.frame(read_csv("~/QuadPay/Join3.csv"))[,-1]

ID3 <- grep(TRUE,!is.na(apply(Join3[,c("AmountWtd","emp","est","customer_credit_score","customer_age","AgeSq",
                                       "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")],1,sum)))


dataset <- Join3[ID3,c("AmountWtd","emp","est","customer_credit_score","customer_age","AgeSq",
                       "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")]

CVres <- data.frame(RF=NA,GBT=NA)

for(i in 1:100){
  trainID <- base::sample(1:nrow(dataset),size=round(nrow(dataset)*0.7))
  
  trainData <- dataset[trainID,]
  testData <- dataset[-trainID,]
  
  # connect to spark
  sc <- sparkR.session(master = "mesos://zk://EDserverUK1:2181,EDserverUK5:2182,EDserverUK8:2181/mesos",
                       sparkConfig = list(spark.driver.memory="16g",spark.executor.memory="16g"))
  
  trainRDD <- createDataFrame(trainData)
  testRDD <- createDataFrame(testData)
  
  modelRF <- spark.randomForest(trainRDD,AmountWtd~.,"regression",numTrees=40,maxMemoryInMB=1024)
  modelGBT <- spark.gbt(trainRDD, AmountWtd~.,"regression", lossType="squared", maxMemoryInMB=1024,
                        maxIter=50)
  
  #summary(model)
  
  tmpPreds <- collect(predict(modelRF, testRDD))[,c("AmountWtd","prediction")]
  #names(tmpPreds) <- ID3
  
  #plot(tmpPreds,testData$AmountWtd)
  
  SSres <- sum((tmpPreds$prediction-tmpPreds$AmountWtd)^2)
  r2RF <- 1-(SSres/sum((tmpPreds$AmountWtd-mean(tmpPreds$AmountWtd))^2))
  
  
  tmpPreds <- collect(predict(modelGBT, testRDD))[,c("AmountWtd","prediction")]
  #names(tmpPreds) <- ID3
  
  #plot(tmpPreds,testData$AmountWtd)
  SSres <- sum((tmpPreds$prediction-tmpPreds$AmountWtd)^2)
  r2GBT <- 1-(SSres/sum((tmpPreds$AmountWtd-mean(tmpPreds$AmountWtd))^2))
  
  CVres <- rbind(CVres,data.frame(RF=r2RF,GBT=r2GBT))
  
  print(i)
  print(CVres)
  Sys.sleep(0.1)
  
  sparkR.session.stop()
  
}


write.csv(CVres,"CVregR2.csv")

