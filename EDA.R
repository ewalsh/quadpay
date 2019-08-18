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

# exploratory data analysis -- get data
orders <- read.csv("~/QuadPay/orders.csv")

# let's look at how many repaid
EDA1pdat <- data.frame(Count=c(sum(orders$paid_installment_1 == 1),sum(orders$paid_installment_1 == 0),
                               sum(is.na(as.numeric(as.character(orders$paid_installment_1)))),
                               sum(orders$paid_installment_2 == 1),sum(orders$paid_installment_2 == 0),
                               sum(is.na(as.numeric(as.character(orders$paid_installment_2)))),
                               sum(orders$paid_installment_3 == 1),sum(orders$paid_installment_3 == 0),
                               sum(is.na(as.numeric(as.character(orders$paid_installment_3)))),
                               sum(orders$paid_installment_4 == 1),sum(orders$paid_installment_4 == 0),
                               sum(is.na(as.numeric(as.character(orders$paid_installment_4))))),
                       Installment=c(rep("Installment 1",3),rep("Installment 2",3),
                                     rep("Installment 3",3),rep("Installment 4",3)),
                       HasPaid=rep(c("Paid", "Unpaid","NULL"),4))

ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA1.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(EDA1pdat,aes(x=HasPaid,y=Count,fill=HasPaid)) +
  geom_bar(stat="identity") +
  facet_wrap(~Installment) +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        #panel.border=element_blank(),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_text(angle=45,hjust=1,face="plain",size=Sinfo),
        axis.text.y=element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_blank(), #element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="right",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  labs(fill="Payment\nStatus")

dev.off()

# let's transform into a timeseries
TSseq <- c(seq(min(as.Date(orders$checkout_started_at)),max(as.Date(orders$checkout_started_at)),by="7 days"),
           max(as.Date(orders$checkout_started_at)) + days(1))
EDA2pdatTS <- EDA2pdatTSpct <- data.frame(Count=NA,Installment=NA,HasPaid=NA,Start=NA,End=NA)

for(i in 2:length(TSseq)){
  tmp <- orders[grep(TRUE,as.Date(orders$checkout_started_at) < as.Date(TSseq[i]) ),]
  tmp <- tmp[grep(TRUE,as.Date(tmp$checkout_started_at) >= as.Date(TSseq[(i-1)]) ),]
  
  DFtmp <- data.frame(Count=c(sum(tmp$paid_installment_1 == 1),sum(tmp$paid_installment_1 == 0),
                              sum(is.na(as.numeric(as.character(tmp$paid_installment_1)))),
                              sum(tmp$paid_installment_2 == 1),sum(tmp$paid_installment_2 == 0),
                              sum(is.na(as.numeric(as.character(tmp$paid_installment_2)))),
                              sum(tmp$paid_installment_3 == 1),sum(tmp$paid_installment_3 == 0),
                              sum(is.na(as.numeric(as.character(tmp$paid_installment_3)))),
                              sum(tmp$paid_installment_4 == 1),sum(tmp$paid_installment_4 == 0),
                              sum(is.na(as.numeric(as.character(tmp$paid_installment_4))))),
                      Installment=c(rep("Installment 1",3),rep("Installment 2",3),
                                    rep("Installment 3",3),rep("Installment 4",3)),
                      HasPaid=rep(c("Paid", "Unpaid","NULL")),
                      Start=as.character(rep(TSseq[(i-1)],12)),End=as.character(rep(TSseq[(i)],12)))
  DFtmpPct <- DFtmp
  DFtmpPct$Count <- DFtmpPct$Count/nrow(tmp)
  
  EDA2pdatTS <- rbind(EDA2pdatTS,DFtmp)
  
  EDA2pdatTSpct <- rbind(EDA2pdatTSpct,DFtmpPct) # I think percent of total may be more informative
}

EDA2pdatTS <- EDA2pdatTS[-1,]
EDA2pdatTSpct <- EDA2pdatTSpct[-1,]

ggplot(EDA2pdatTS,aes(x=Start,y=Count,fill=HasPaid)) +
  geom_bar(stat="identity",position="stack") +
  facet_grid(HasPaid~Installment,scales="free_y")


# plot percent of total over time
ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA2_PctTS.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(EDA2pdatTSpct,aes(x=Start,y=Count,fill=HasPaid)) +
  geom_bar(stat="identity",position="stack") +
  facet_grid(HasPaid~Installment,scales="free_y") +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        panel.border=element_rect(color="grey50",linetype="solid",size=1),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_text(angle=45,hjust=1,face="plain",size=Sinfo-10),
        axis.text.y=element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_blank(), #element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="right",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  labs(fill="Payment\nStatus") +
  ylab("Percent Of Total") +
  scale_y_continuous(labels=function(X){paste(X*100,"%",sep="")})

dev.off()

# A deeper look at the unpaid
## Added a booliean for if it is unpaid, a variable for which periods, and the expected loss of the unpaid amount
UnpaidData <- cbind(orders,data.frame(UnpaidPeriod=apply(cbind((as.numeric(as.character(orders$paid_installment_1)) == 0),
                                                               (as.numeric(as.character(orders$paid_installment_2)) == 0),
                                                               (as.numeric(as.character(orders$paid_installment_3)) == 0),
                                                               (as.numeric(as.character(orders$paid_installment_4)) == 0)),1,sum),
                                      IsUnpaid=apply(cbind((as.numeric(as.character(orders$paid_installment_1)) == 0),
                                                           (as.numeric(as.character(orders$paid_installment_2)) == 0),
                                                           (as.numeric(as.character(orders$paid_installment_3)) == 0),
                                                           (as.numeric(as.character(orders$paid_installment_4)) == 0)),1,sum)>0))
ordersExpanded <- UnpaidData <- cbind(UnpaidData,data.frame(AmountWtd=UnpaidData$order_amount*UnpaidData$IsUnpaid*(UnpaidData$UnpaidPeriod/4)))
UnpaidData <- UnpaidData[-grep(TRUE,is.na(UnpaidData$IsUnpaid)),]

# relationship betwween the order aount andif it was unpaid
plot(UnpaidData$IsUnpaid,UnpaidData$order_amount) # opposite of what I would have thought

# let's look at the distribution
ggplot(UnpaidData,aes(x=order_amount,color=IsUnpaid)) +
  geom_density() #+
#  facet_wrap(~IsUnpaid,ncol=1)
## No difference

summary(lm(UnpaidData$IsUnpaid~UnpaidData$order_amount)) # confirmed

# relationship betwween the customer's age if it was unpaid
plot(UnpaidData$IsUnpaid,UnpaidData$customer_age)

#summary(lm(UnpaidData$AmountWtd~UnpaidData$customer_age)) # not relevant to the amount

summary(lm(UnpaidData$IsUnpaid~UnpaidData$customer_age)) # makes sense that older customers are less likely to miss payment

UnpaidData <- cbind(UnpaidData,data.frame(PaidNotPaid=rep("Paid",nrow(UnpaidData))))
UnpaidData$PaidNotPaid <- as.character(UnpaidData$PaidNotPaid)
UnpaidData$PaidNotPaid[grep(TRUE,UnpaidData$IsUnpaid)] <- "Missed Payment"

ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA3_Age.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(UnpaidData,aes(x=customer_age,fill=PaidNotPaid)) +
  geom_density() +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        panel.border=element_rect(color="grey50",linetype="solid",size=1),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_text(angle=0,hjust=1,face="plain",size=Sinfo-10),
        axis.text.y=element_blank(), #element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_blank(), #element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="right",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  labs(fill="Payment\nStatus") +
  xlab("Customer's Age") +
  scale_fill_manual(values=c(rgb(0, 123/360, 255/360, 0.5),rgb(85/360, 93/360, 102/360, 0.5)))

dev.off()

# looks like a sliding scale as users get older

plot(UnpaidData$IsUnpaid,UnpaidData$customer_credit_score)

plot(UnpaidData$AmountWtd,UnpaidData$customer_credit_score)

summary(lm(UnpaidData$AmountWtd~UnpaidData$customer_credit_score)) # credit score is a strong predictor

summary(lm(UnpaidData$IsUnpaid~UnpaidData$customer_credit_score))

ppi <- 250
Sinfo = 40
png("~/QuadPay/EDA3_CreditScore.png",width=20*ppi,height=12*ppi,res=ppi)

ggplot(UnpaidData,aes(x=customer_credit_score,fill=PaidNotPaid)) +
  geom_density() +
  theme_bw() +
  theme(panel.grid.major.y=element_line(color="grey50",linetype="dashed",size=0.75),
        panel.grid.major.x=element_line(color="grey50",linetype="dotted",size=0.5),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill=grey(0.95,0.9)),
        panel.border=element_rect(color="grey50",linetype="solid",size=1),
        axis.text=element_text(family="axiformaextrabold",size=Sinfo+3),
        axis.text.x=element_text(angle=0,hjust=1,face="plain",size=Sinfo-10),
        axis.text.y=element_blank(), #element_text(family="axiformaextrabold",size=Sinfo-5),
        axis.line = element_line(color="black"),
        axis.title.x=element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=-0.4),
        axis.title.y=element_blank(), #element_text(face="bold",family="axiformaextrabold",size=Sinfo,vjust=1.5),
        legend.position="right",
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.title=element_text(face="bold",family="axiformaextrabold",size=Sinfo-5),
        legend.text=element_text(family="axiformaextrabold",size=Sinfo-20,lineheight=0.25),
        legend.key.size=unit(.05,"npc"),
        plot.background = element_rect(fill=grey(0.95,0.9)),
        plot.title=element_text(family="axiformaextrabold",face="bold",hjust=0.5,size=Sinfo+20),
        strip.background = element_rect(fill=grey(0.95,0.9),color=NA),
        strip.text=element_text(family="axiformaextrabold",face="bold",size=rel(3.5))) +
  labs(fill="Payment\nStatus") +
  xlab("Customer's Credit Score") +
  scale_fill_manual(values=c(rgb(0, 123/360, 255/360, 0.5),rgb(85/360, 93/360, 102/360, 0.5)))

dev.off()

# I'm thinking a support vector machine could be helpful
#svmfit <- svm(AmountWtd~customer_credit_score+customer_age,data=UnpaidData,kernel="polynomial")
### For now, let's focus only on simplier breakpoints

# plot(predict(svmfit,UnpaidData))
# points(UnpaidData$AmountWtd, predict(svmfit,UnpaidData), col = "blue", pch=4)
# 
# plot(svmfit,UnpaidData)

plot(UnpaidData[,c("AmountWtd","customer_credit_score","customer_age")])

zbp16totals <- read.csv("~/QuadPay/zbp16totals/zbp16totals.txt")

zbp16totals$zip <- as.character(zbp16totals$zip)
zbp16totals$zip <- as.character(sapply(zbp16totals$zip,function(X){
  out <- X
  if(str_length(X) < 5){
    out <- paste(rep("0",5-str_length(X)),X,sep="")
  }
  return(out)
}))

zipJoin <- zbp16totals
colnames(zipJoin)[1] <- "customer_billing_zip"
zipJoin$customer_billing_zip <- as.character(zipJoin$customer_billing_zip)
ordersExpanded1 <- ordersExpanded
ordersExpanded1$customer_billing_zip <- str_sub(ordersExpanded1$customer_billing_zip,1,5)

Join1 <- left_join(ordersExpanded1,zipJoin,by="customer_billing_zip")

summary(lm(AmountWtd~emp+qp1+ap+est+customer_credit_score+customer_age,data=Join1)) # no evidence census data is an improvement

summary(lm(IsUnpaid~emp+qp1+ap+est+customer_credit_score+customer_age,data=Join1))

GLMmod <- glm(IsUnpaid~emp+qp1+ap+est+customer_credit_score+customer_age,family=binomial(link='logit'),data=Join1)
summary(GLMmod)
# Census API calls
CensusAPIkey <- # Enter Key Here
WebRoot <- "https://api.census.gov/data/2012/ewks?get="
Var <- "RCPTOT"
Link1 <- ",OPTAX&for="
Geo <- "place"
Link2 <- ":*&NAICS2012="
IndCode <- "44-45"
Link3 <- "&key="

Retail <- GET(paste(WebRoot,Var,Link1,Geo,Link2,IndCode,Link3,CensusAPIkey,sep=""))
Retail <- fromJSON(content(Retail,"text"))
Retail <- data.frame(do.call(rbind,Retail)[-1,])
#Retail <- Retail[grep(TRUE,!duplicated(Retail$X5)),]
Retail <- data.frame(customer_billing_zip=unique(Retail$X5),
                     Retail_Val=sapply(unique(Retail$X5),function(zip,Retail){
                       max(as.numeric(Retail[grep(TRUE,as.character(Retail$X5) == as.character(zip)),1]))
                     },Retail))
#Retail <- Retail[,c("X5","X1")] #grep(TRUE,as.character(Retail$X2) == "A ")
#colnames(Retail) <- c("customer_billing_zip","Retail_Val")

IndCode <- 54
Science <- GET(paste(WebRoot,Var,Link1,Geo,Link2,IndCode,Link3,CensusAPIkey,sep=""))
Science <- fromJSON(content(Science,"text"))
Science <- data.frame(do.call(rbind,Science)[-1,])
#Science <- Science[grep(TRUE,!duplicated(Science$X5)),]
Science <- data.frame(customer_billing_zip=unique(Science$X5),
                      Science_Val=sapply(unique(Science$X5),function(zip,Science){
                        max(as.numeric(Science[grep(TRUE,as.character(Science$X5) == as.character(zip)),1]))
                      },Science))
# Science <- Science[grep(TRUE,as.character(Science$X2) == "A "),c("X5","X1")]
# colnames(Science) <- c("customer_billing_zip","Science_Val")

IndCode <- 61
Education <- GET(paste(WebRoot,Var,Link1,Geo,Link2,IndCode,Link3,CensusAPIkey,sep=""))
Education <- fromJSON(content(Education,"text"))
Education <- data.frame(do.call(rbind,Education)[-1,])
#Education <- Education[grep(TRUE,!duplicated(Education$X5)),]
Education <- data.frame(customer_billing_zip=unique(Education$X5),
                        Education_Val=sapply(unique(Education$X5),function(zip,Education){
                          max(as.numeric(Education[grep(TRUE,as.character(Education$X5) == as.character(zip)),1]))
                        },Education))
# Education <- Education[grep(TRUE,as.character(Education$X2) == "A "),c("X5","X1")]
# colnames(Education) <- c("customer_billing_zip","Education_Val")

IndCode <- 62
HC <- GET(paste(WebRoot,Var,Link1,Geo,Link2,IndCode,Link3,CensusAPIkey,sep=""))
HC <- fromJSON(content(HC,"text"))
HC <- data.frame(do.call(rbind,HC)[-1,])
# HC <- HC[grep(TRUE,!duplicated(HC$X5)),]
HC <- data.frame(customer_billing_zip=unique(HC$X5),
                 HC_Val=sapply(unique(HC$X5),function(zip,HC){
                   max(as.numeric(HC[grep(TRUE,as.character(HC$X5) == as.character(zip)),1]))
                 },HC))
# HC <- HC[grep(TRUE,as.character(HC$X2) == "A "),c("X5","X1")]
# colnames(HC) <- c("customer_billing_zip","HC_Val")

IndCode <- 71
Arts <- GET(paste(WebRoot,Var,Link1,Geo,Link2,IndCode,Link3,CensusAPIkey,sep=""))
Arts <- fromJSON(content(Arts,"text"))
Arts <- data.frame(do.call(rbind,Arts)[-1,])
# Arts <- Arts[grep(TRUE,!duplicated(Arts$X5)),]
Arts <- data.frame(customer_billing_zip=unique(Arts$X5),
                   Arts_Val=sapply(unique(Arts$X5),function(zip,Arts){
                     max(as.numeric(Arts[grep(TRUE,as.character(Arts$X5) == as.character(zip)),1]))
                   },Arts))
# Arts <- Arts[grep(TRUE,as.character(Arts$X2) == "A "),c("X5","X1")]
# colnames(Arts) <- c("customer_billing_zip","Arts_Val")

CensusJoin <- left_join(Retail,Science,by="customer_billing_zip")
CensusJoin <- left_join(CensusJoin,Education,by="customer_billing_zip")
CensusJoin <- left_join(CensusJoin,HC,by="customer_billing_zip")
CensusJoin <- left_join(CensusJoin,Arts,by="customer_billing_zip")
CensusJoin$customer_billing_zip <- as.character(CensusJoin$customer_billing_zip)

CensusJoin[,-1] <- apply(CensusJoin[,-1],2,function(X){(X-mean(X,na.rm=TRUE))/sd(X,na.rm=TRUE)})

Join1_1 <- Join1
Join1_1$customer_billing_zip <- str_sub(Join1_1$customer_billing_zip,1,5)

Join2 <- left_join(Join1_1,CensusJoin,by="customer_billing_zip")
