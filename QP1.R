require(SparkR)
require(ROCR)

Join3 <- data.frame(read_csv("~/QuadPay/Join3.csv"))[,-1]

ID3 <- grep(TRUE,!is.na(apply(Join3[,c("IsUnpaid","emp","est","customer_credit_score","customer_age","AgeSq",
                                       "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")],1,sum)))


dataset <- Join3[ID3,c("IsUnpaid","emp","est","customer_credit_score","customer_age","AgeSq",
                    "CSbreak","order_amount","Retail_Val","Science_Val","Education_Val","HC_Val","Arts_Val")]


# connect to spark
sc <- sparkR.session(master = "mesos://zk://EDserverUK1:2181,EDserverUK5:2182,EDserverUK8:2181/mesos",
                     sparkConfig = list(spark.driver.memory="16g",spark.executor.memory="16g"))

qpRDD <- createDataFrame(dataset)

modelRF <- spark.randomForest(qpRDD,IsUnpaid~.,"classification",numTrees=10,maxMemoryInMB=512)
modelGBT <- spark.gbt(qpRDD, IsUnpaid~.,"classification", maxMemoryInMB=512)

#summary(model)

tmpPreds <- collect(predict(modelRF, qpRDD))$IsUnpaid
names(tmpPreds) <- ID3

prRF <- prediction(tmpPreds,dataset$IsUnpaid)

prfRF <- performance(prRF, measure = "tpr", x.measure = "fpr")
plot(prfRF)

aucRF <- performance(prRF, measure = "auc")
aucRF <- aucRF@y.values[[1]]
aucRF

tmpPreds <- collect(predict(modelGBT, qpRDD))$IsUnpaid
names(tmpPreds) <- ID3

prGBT <- prediction(tmpPreds,dataset$IsUnpaid)

prfGBT <- performance(prGBT, measure = "tpr", x.measure = "fpr")
plot(prfGBT)

aucGBT <- performance(prGBT, measure = "auc")
aucGBT <- aucGBT@y.values[[1]]
aucGBT


sparkR.session.stop()

AUCdataSpark <- data.frame(`True Positive Rate`=c(unlist(attributes(prfRF)$y.values),unlist(attributes(prfGBT)$y.values)),
                           `False Positive Rate`=c(unlist(attributes(prfRF)$x.values),unlist(attributes(prfGBT)$x.values)),
                           type=c(rep("Random Forest",length(unlist(attributes(prfRF)$y.values))),
                                  rep("Gradient Boosted Tree",length(unlist(attributes(prfGBT)$y.values)))))