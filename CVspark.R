require(SparkR)
require(ROCR)
require(readr)

Join3 <- data.frame(read_csv("~/QuadPay/Join3.csv"))[,-1]

ID3 <- grep(TRUE,!is.na(apply(Join3[,c("IsUnpaid","emp","est","customer_credit_score","customer_age","AgeSq",
                                       "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")],1,sum)))


dataset <- Join3[ID3,c("IsUnpaid","emp","est","customer_credit_score","customer_age","AgeSq",
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
  
  modelRF <- spark.randomForest(trainRDD,IsUnpaid~.,"classification",maxMemoryInMB=512, numTrees = 40)
  modelGBT <- spark.gbt(trainRDD, IsUnpaid~.,"classification", maxMemoryInMB=512, maxIter=50)
  
  #summary(model)
  
  tmpPreds <- collect(predict(modelRF, testRDD))[,c("IsUnpaid","prediction")]

  prRF <- prediction(as.numeric(tmpPreds$prediction),tmpPreds$IsUnpaid)
  
  prfRF <- performance(prRF, measure = "tpr", x.measure = "fpr")
  #plot(prfRF)
  
  aucRF <- performance(prRF, measure = "auc")
  aucRF <- aucRF@y.values[[1]]
  #aucRF
  
  tmpPreds <- collect(predict(modelGBT, testRDD))[,c("IsUnpaid","prediction")]
  
  prGBT <- prediction(as.numeric(tmpPreds$prediction),testData$IsUnpaid)
  
  prfGBT <- performance(prGBT, measure = "tpr", x.measure = "fpr")
  #plot(prfGBT)
  
  aucGBT <- performance(prGBT, measure = "auc")
  aucGBT <- aucGBT@y.values[[1]]
  #aucGBT
  
  CVres <- rbind(CVres,data.frame(RF=aucRF,GBT=aucGBT))
  
  print(i)
  print(CVres)
  Sys.sleep(0.1)
  
  sparkR.session.stop()
  
}


write.csv(CVres,"CVclassAUC.csv")

