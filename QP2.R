require(SparkR)
require(ROCR)

Join3 <- data.frame(read_csv("~/QuadPay/Join3.csv"))[,-1]

ID3 <- grep(TRUE,!is.na(apply(Join3[,c("AmountWtd","emp","est","customer_credit_score","customer_age","AgeSq",
                                       "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")],1,sum)))


dataset <- Join3[ID3,c("AmountWtd","emp","est","customer_credit_score","customer_age","AgeSq",
                       "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")]


# connect to spark
sc <- sparkR.session(master = "mesos://zk://EDserverUK1:2181,EDserverUK5:2182,EDserverUK8:2181/mesos",
                     sparkConfig = list(spark.driver.memory="16g",spark.executor.memory="16g"))

qpRDD <- createDataFrame(dataset)

modelRF <- spark.randomForest(qpRDD,AmountWtd~.,"regression",numTrees=10,maxMemoryInMB=512)
modelGBT <- spark.gbt(qpRDD, AmountWtd~.,"regression", lossType="squared", maxMemoryInMB=512)

#summary(model)

tmpPreds <- collect(predict(modelRF, qpRDD))$AmountWtd
names(tmpPreds) <- ID3

#plot(tmpPreds,dataset$AmountWtd)

r2RF <- sum((tmpPreds-dataset$AmountWtd)^2)

tmpPreds <- collect(predict(modelGBT, qpRDD))$AmountWtd
names(tmpPreds) <- ID3

#plot(tmpPreds,dataset$AmountWtd)

r2GBT <- sum((tmpPreds-dataset$AmountWtd)^2)


sparkR.session.stop()

