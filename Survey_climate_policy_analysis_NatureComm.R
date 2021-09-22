
setwd("E:/Projects/3_CurrentProjects/Topic Modelling Climate/Mestre et al")
#install.packages(c("magrittr", "data.table", 'dplyr', 'ggplot2', 'cluster'))
sapply(c("MASS","DescTools", "magrittr", "data.table", 'plyr','dplyr', 'ggplot2', 'cluster', 'dendextend','pkgbuild','devtools','R.methodsS3','stm','ngram','matrixStats','corrplot','ggpubr','wordcloud','ggplot2'), require, character.only = T)

knitr::opts_chunk$set(fig.width=12, fig.height=12) 
data <- read.csv('file_NatComm.csv', header=T, sep=',')
#data<-data[,c(-1,-3,-4,-5,-7,-8,-9,-10,-74)]#drop irrelevant columns

for (column in c(2:7,9:53,55:67)){
  data[,column]<-as.numeric(as.character(data[,column])) #make sure all numbers are recognized as numbers
  #data[which(data[,column]==.),column]<-NA #make 8 (no opinion) NA
}
#67 columns, with two being textual responses

# #cleaning, renaming and recoding variabe consistent with the other survey
# data$timing_rec3[data$timing_rec3==3]<-NA
# data$timing_rec3[data$timing_rec3==1]<-0
# data$timing_rec3[data$timing_rec3==2]<-1
# #colnames(data)[18]<-"belief.end.of.growth_recoded"
# data$gender[data$gender==3]<-NA
# data$gender[data$gender==2]<-0 #recoding the variable to make consistent with the other survey
# data$favored_growth_environ_strategy[data$favored_growth_environ_strategy==5]<-NA

colnames(data)<-c("Response.ID","Duration","Gender","Age","Age.cohort","Regional.Code","Number.Inhabitants",
                  "Text.CarbonTaxPolicy","Perceived.CarbonTaxKnowledge",
                  "Knowledge.Test1","Knowledge.Test2","Knowledge.Test3","Knowledge.Test4","Knowledge.Test5","Knowledge.Test6",
                  "Experiment.Group","CT.Effectiveness","CT.Fairness","CT.Object","CT.PersonalCost","CT.LowIncomeEffect","CT.TrustInPoliticians","CT.Acceptability","CT.Objective",
                  "CT.Effectiveness_Rev1","CT.Effectiveness_Rev2","CT.Effectiveness_Rev3","CT.Effectiveness_Rev4","CT.Effectiveness_Rev5","CT.Fairness_Rev1","CT.Fairness_Rev2","CT.Fairness_Rev3","CT.Fairness_Rev4","CT.Fairness_Rev5",
                  "CT.PersonalCost_Rev1","CT.PersonalCost_Rev2","CT.PersonalCost_Rev3","CT.PersonalCost_Rev4","CT.PersonalCost_Rev5",
                  "CT.LowIncomeEffect_Rev1","CT.LowIncomeEffect_Rev2","CT.LowIncomeEffect_Rev3","CT.LowIncomeEffect_Rev4","CT.LowIncomeEffect_Rev5",
                  "CT.Acceptability_Rev1","CT.Acceptability_Rev2","CT.Acceptability_Rev3","CT.Acceptability_Rev4","CT.Acceptability_Rev5","Allocation_Climate","Allocation_LowIncome","Allocation_Universal","EffectivenessOrFairness","Text.CTFairness","NumberOfPeers","NumberOfPeersClimChange","ExpectedSpanishAcceptability","InfoSpanishAcceptability","AcceptabilityAfterInfo","HouseholdSize","MonthlyIncome","Education","ClimateConcern","PoliticalView","PoliticalParty","CarUse","DrivingTime")        

# library(openxlsx)
# write.xlsx(data, 'file_for_Sara.xlsx')

summary(data[,c(2:7,9:53,55:67)])

message("SDs of the descriptive variables")
sapply(data[,c(2:7,9:53,55:67)], sd, na.rm = TRUE)

DataCorrplot<-data[,c(-8,-10:-15,-25:-52,-54,-58:-59)]

par(mfrow = c(1, 1) ,mar=c(1,1,1,2))
M <- cor(DataCorrplot)
corrplot(M, method = "circle",tl.cex=.5,tl.col = "black",col=colorRampPalette(c("blue","white","red"))(200))


DataCorrplot2<-DataCorrplot

DataCorrplot2$MonthlyIncome[DataCorrplot$MonthlyIncome==7]<-NA

DataCorrplot2<-na.omit(DataCorrplot2)


M2 <- cor(DataCorrplot2)
M2<-M2[c(1,23),]
message("Correlations with Response id (included for formatting!) monthly income after dropping NAs")
t(M2)


DataCorrplot3<-DataCorrplot

#DataCorrplot3$PoliticalView[DataCorrplot$PoliticalView==11]<-NA
#DataCorrplot3$PoliticalParty[DataCorrplot$PoliticalParty>13]<-NA

DataCorrplot3<-na.omit(DataCorrplot3)


M3 <- cor(DataCorrplot3)
M3<-M3[26:27,]
message("Correlations with political views and parties after dropping NAs")

t(M3)


DataCorrplot<-data[,c(17:18,20:21,23,25:49)]
M_rev <- cor(DataCorrplot)
corrplot(M_rev, method = "circle",tl.cex=.5,tl.col = "black",col=colorRampPalette(c("blue","white","red"))(200))



DataCorrplot<-data[,c(17:18,20:21,23,25,30,35,40,45,26,31,36,41,46,27,32,37,42,47,28,33,38,43,48,29,34,39,44,49)]
M_rev2 <- cor(DataCorrplot)
corrplot(M_rev2, method = "circle",tl.cex=.5,tl.col = "black",col=colorRampPalette(c("blue","white","red"))(200))
print(M_rev2)

data$ClimateOnlyAllocation<-ifelse(data$Allocation_Climate==100,1,0)
data$LowIncomeOnlyAllocation<-ifelse(data$Allocation_LowIncome==100,1,0)
data$UniversalOnlyAllocation<-ifelse(data$Allocation_Universal==100,1,0)
data$ClimatetLowIncomeAllocation<-ifelse(data$Allocation_Universal==0&data$Allocation_LowIncome!=0&data$Allocation_Climate!=0,1,0)
data$ClimateetUniversalAllocation<-ifelse(data$Allocation_LowIncome==0&data$Allocation_Climate!=0&data$Allocation_Universal!=0,1,0)
data$UniversaletLowIncomeAllocation<-ifelse(data$Allocation_Climate==0&data$Allocation_LowIncome!=0&data$Allocation_Universal!=0,1,0)
data$UniversaletLowIncomeetClimateAllocation<-ifelse(data$Allocation_Climate!=0&data$Allocation_LowIncome!=0&data$Allocation_Universal!=0,1,0)

Allocation_Choices<-cbind(data$ClimateOnlyAllocation,data$LowIncomeOnlyAllocation,
                          data$UniversalOnlyAllocation,data$ClimatetLowIncomeAllocation,
                          data$ClimateetUniversalAllocation,data$UniversaletLowIncomeAllocation,data$UniversaletLowIncomeetClimateAllocation)

Allocation_Choices_Comp<-matrix(0,3,7)

colnames(Allocation_Choices)<-c("Climate Only","PoorHH Only","AllHH Only",
                                       "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")

colnames(Allocation_Choices_Comp)<-c("Climate Only","PoorHH Only","AllHH Only",
                                     "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
rownames(Allocation_Choices_Comp)<-c("Climate","PoorHH","AllHH")
#estimate the percentage allocation of climate in climate only  
Allocation_Choices_Comp[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1])/100

Allocation_Choices_Comp[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1])/100

Allocation_Choices_Comp[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1])/100

Allocation_Choices_Comp[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1])/100
Allocation_Choices_Comp[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1])/100

Allocation_Choices_Comp[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1])/100
Allocation_Choices_Comp[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1])/100

Allocation_Choices_Comp[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1])/100
Allocation_Choices_Comp[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1])/100

Allocation_Choices_Comp[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1])/100
Allocation_Choices_Comp[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1])/100
Allocation_Choices_Comp[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1])/100
#sum(Allocation_Choices[,7]==1)

par(mfrow = c(1,3) ,mar=c(15,4,3,2))
# barplot(Allocation_Choices,main="Distribution of allocation choices",
#         col=c("darkblue"),cex.names=0.7,las=2)

barplot(Allocation_Choices_Comp,main="Distribution of allocation choices",
        col=c("#66c2a5","#fc8d62","#8da0cb"), 
        #col=c(rgb(102,194,165),rgb(252,141,98),rgb(141,160,203)),
        legend = c("Climate","PoorHH","AllHH"),cex.names=1.3,las=2,ylab="Number of respondents",cex.axis=1.3,cex.lab=1.3,
        args.legend = list(x="topleft",ncol=1,cex=1.3,bty="n"))


Allocation_Choices_Comp_LowAcc<-matrix(0,3,7)

colnames(Allocation_Choices_Comp_LowAcc)<-c("Climate Only","PoorHH Only","AllHH Only",
                                            "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
rownames(Allocation_Choices_Comp_LowAcc)<-c("Climate","PoorHH","AllHH")
#estimate the percentage allocation of climate in climate only  
Allocation_Choices_Comp_LowAcc[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1 & data$CT.Acceptability<3])/100

Allocation_Choices_Comp_LowAcc[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1 & data$CT.Acceptability<3])/100

Allocation_Choices_Comp_LowAcc[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1 & data$CT.Acceptability<3])/100

Allocation_Choices_Comp_LowAcc[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1 & data$CT.Acceptability<3])/100
Allocation_Choices_Comp_LowAcc[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1 & data$CT.Acceptability<3])/100

Allocation_Choices_Comp_LowAcc[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1 & data$CT.Acceptability<3])/100
Allocation_Choices_Comp_LowAcc[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1 & data$CT.Acceptability<3])/100

Allocation_Choices_Comp_LowAcc[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1 & data$CT.Acceptability<3])/100
Allocation_Choices_Comp_LowAcc[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1 & data$CT.Acceptability<3])/100

Allocation_Choices_Comp_LowAcc[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1 & data$CT.Acceptability<3])/100
Allocation_Choices_Comp_LowAcc[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1 & data$CT.Acceptability<3])/100
Allocation_Choices_Comp_LowAcc[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1 & data$CT.Acceptability<3])/100
#sum(Allocation_Choices[,7]==1)
barplot(Allocation_Choices_Comp_LowAcc,main="... for respondents with low acceptabilty",
        col=c("#66c2a5","#fc8d62","#8da0cb"), 
        legend = c("Climate","PoorHH","AllHH"),cex.names=1.3,las=2,cex.axis=1.3,
        args.legend = list(x="topleft",bty="n",ncol=1,cex=1.3))


Allocation_Choices_Comp_HighAcc<-matrix(0,3,7)

colnames(Allocation_Choices_Comp_HighAcc)<-c("Climate Only","PoorHH Only","AllHH Only",
                                             "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
rownames(Allocation_Choices_Comp_HighAcc)<-c("Climate","PoorHH","AllHH")
#estimate the percentage allocation of climate in climate only  
Allocation_Choices_Comp_HighAcc[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1 & data$CT.Acceptability>3])/100

Allocation_Choices_Comp_HighAcc[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1 & data$CT.Acceptability>3])/100

Allocation_Choices_Comp_HighAcc[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1 & data$CT.Acceptability>3])/100

Allocation_Choices_Comp_HighAcc[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1 & data$CT.Acceptability>3])/100
Allocation_Choices_Comp_HighAcc[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1 & data$CT.Acceptability>3])/100

Allocation_Choices_Comp_HighAcc[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1 & data$CT.Acceptability>3])/100
Allocation_Choices_Comp_HighAcc[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1 & data$CT.Acceptability>3])/100

Allocation_Choices_Comp_HighAcc[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1 & data$CT.Acceptability>3])/100
Allocation_Choices_Comp_HighAcc[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1 & data$CT.Acceptability>3])/100

Allocation_Choices_Comp_HighAcc[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1 & data$CT.Acceptability>3])/100
Allocation_Choices_Comp_HighAcc[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1 & data$CT.Acceptability>3])/100
Allocation_Choices_Comp_HighAcc[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1 & data$CT.Acceptability>3])/100
#sum(Allocation_Choices[,7]==1)
barplot(Allocation_Choices_Comp_HighAcc,main="... for respondents with high acceptabilty",
        col=c("#66c2a5","#fc8d62","#8da0cb"), 
        legend = c("Climate","PoorHH","AllHH"),cex.names=1.3,las=2,cex.axis=1.3,
        args.legend = list(x="topleft",bty="n",ncol=1,cex=1.3))













par(mfrow = c(1,3) ,mar=c(15,4,3,2))
# barplot(Allocation_Choices,main="Distribution of allocation choices",
#         col=c("darkblue"),cex.names=0.7,las=2)

barplot(Allocation_Choices_Comp,main="Distribution of allocation choices",
        col=c("green","red","blue"),
        legend = c("Climate","PoorHH","AllHH"),cex.names=1,las=2,ylab="Number of respondents",
        args.legend = list(x="topleft",ncol=1,cex=1,bty="n"))

Allocation_Choices_Comp_ExpNo<-matrix(0,3,7)

colnames(Allocation_Choices_Comp_ExpNo)<-c("Climate Only","PoorHH Only","AllHH Only",
                                            "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
rownames(Allocation_Choices_Comp_ExpNo)<-c("Climate","PoorHH","AllHH")
#estimate the percentage allocation of climate in climate only  
Allocation_Choices_Comp_ExpNo[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1 & data$Experiment.Group==1])/100

Allocation_Choices_Comp_ExpNo[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1 & data$Experiment.Group==1])/100

Allocation_Choices_Comp_ExpNo[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1 & data$Experiment.Group==1])/100

Allocation_Choices_Comp_ExpNo[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1 & data$Experiment.Group==1])/100
Allocation_Choices_Comp_ExpNo[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1 & data$Experiment.Group==1])/100

Allocation_Choices_Comp_ExpNo[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1 & data$Experiment.Group==1])/100
Allocation_Choices_Comp_ExpNo[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1 & data$Experiment.Group==1])/100

Allocation_Choices_Comp_ExpNo[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1 & data$Experiment.Group==1])/100
Allocation_Choices_Comp_ExpNo[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1 & data$Experiment.Group==1])/100

Allocation_Choices_Comp_ExpNo[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1 & data$Experiment.Group==1])/100
Allocation_Choices_Comp_ExpNo[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1 & data$Experiment.Group==1])/100
Allocation_Choices_Comp_ExpNo[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1 & data$Experiment.Group==1])/100
#sum(Allocation_Choices[,7]==1)
barplot(Allocation_Choices_Comp_ExpNo,main="... for respondents with no information",
        col=c("green","red","blue"),
        legend = c("Climate","PoorHH","AllHH"),cex.names=1,las=2,
        args.legend = list(x="topleft",bty="n",ncol=1,cex=1))








Allocation_Choices_Comp_ExpYes<-matrix(0,3,7)

colnames(Allocation_Choices_Comp_ExpYes)<-c("Climate Only","PoorHH Only","AllHH Only",
                                             "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
rownames(Allocation_Choices_Comp_ExpYes)<-c("Climate","PoorHH","AllHH")
#estimate the percentage allocation of climate in climate only  
Allocation_Choices_Comp_ExpYes[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1 & data$Experiment.Group==2])/100

Allocation_Choices_Comp_ExpYes[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1 & data$Experiment.Group==2])/100

Allocation_Choices_Comp_ExpYes[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1 & data$Experiment.Group==2])/100

Allocation_Choices_Comp_ExpYes[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1 & data$Experiment.Group==2])/100
Allocation_Choices_Comp_ExpYes[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1 & data$Experiment.Group==2])/100

Allocation_Choices_Comp_ExpYes[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1 & data$Experiment.Group==2])/100
Allocation_Choices_Comp_ExpYes[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1 & data$Experiment.Group==2])/100

Allocation_Choices_Comp_ExpYes[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1 & data$Experiment.Group==2])/100
Allocation_Choices_Comp_ExpYes[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1 & data$Experiment.Group==2])/100

Allocation_Choices_Comp_ExpYes[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1 & data$Experiment.Group==2])/100
Allocation_Choices_Comp_ExpYes[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1 & data$Experiment.Group==2])/100
Allocation_Choices_Comp_ExpYes[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1 & data$Experiment.Group==2])/100
#sum(Allocation_Choices[,7]==1)
barplot(Allocation_Choices_Comp_ExpYes,main="... for respondents with information",
        col=c("green","red","blue"),
        legend = c("Climate","PoorHH","AllHH"),cex.names=1,las=2,
        args.legend = list(x="topleft",bty="n",ncol=1,cex=1))









DataCorrplot<-data[,c(1,16,50:52)]
names(DataCorrplot)<-c("Response.ID","Experiment","Allocation_Climate","Allocation_LowIncome","Allocation_Universal")
data_long<-reshape(DataCorrplot, direction = "long", varying = names(DataCorrplot)[3:5], idvar = "Response.ID", sep="_",timevar="Question")

myData <- aggregate(data_long$Allocation,
                    by = list(data_long$Question),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse", "mean", "sd", "n", "se")
myData$RevenueUse<-c('Climate projects','Low-income  transfers','Universal transfers')

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = RevenueUse))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "% Allocation")+ theme(
    axis.text.x = element_blank())+
  scale_fill_discrete(name = "Revenue use", 
                      labels = c('Climate projects','Low-income  transfers','Universal transfers'))#+ 


ggplot(data = myData, aes(x = RevenueUse, y = mean, ymax = myData$mean + 2*myData$se,
                                             ymin = myData$mean - 2*myData$se))+
  geom_point(aes(color = RevenueUse)) +
  geom_errorbar(aes(color = RevenueUse)) +
  labs(x = "Revenue use", y = "% Allocation")+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(name = "Revenue use", 
                   labels = c('Climate projects','Low-income  transfers','Universal transfers'))#+ 

myData <- aggregate(data_long$Allocation,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('Climate projects','Low-income  transfers','Universal transfers')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = factor(InformationProvided)))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "% Allocation")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(labels = c('Climate projects','Low-income  transfers','Universal transfers'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = RevenueUse, y = mean))+
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Revenue use", y = "% Allocation")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
                              'Low-income+Climate','Universal+Climate'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


DataCorrplot<-data[,c(1,16,17,25:29)]
names(DataCorrplot)<-c("Response.ID","Experiment","CT.Effectiveness_Rev0","CT.Effectiveness_Rev1","CT.Effectiveness_Rev2","CT.Effectiveness_Rev3","CT.Effectiveness_Rev4","CT.Effectiveness_Rev5")
data_long<-reshape(DataCorrplot, direction = "long", varying = names(DataCorrplot)[3:8], idvar = "Response.ID", sep="_",timevar="Question")

myData <- aggregate(data_long$CT.Effectiveness,
                    by = list(data_long$Question),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = RevenueUse))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Effectiveness")+ theme(
    axis.text.x = element_blank())+
  scale_fill_discrete(name = "Revenue use", 
                      labels = c('Unspecified','Low-income  transfers','Climate projects',
                                 'Universal transfers','Low-income+Climate','Universal+Climate'))#+ 


ggplot(data = myData, aes(x = RevenueUse, y = mean, ymax = myData$mean + 2*myData$se,
                               ymin = myData$mean - 2*myData$se))+
 geom_point(aes(color = RevenueUse)) +
  geom_errorbar(aes(color = RevenueUse)) +
  labs(x = "Revenue use", y = "Effectiveness")+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(name = "Revenue use", 
                      labels = c('Unspecified','Low-income  transfers','Climate projects',
                                 'Universal transfers','Low-income+Climate','Universal+Climate'))#+ 

myData <- aggregate(data_long$CT.Effectiveness,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = factor(InformationProvided)))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Effectiveness")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
                              'Low-income+Climate','Universal+Climate'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = RevenueUse, y = mean))+
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
             position= position_dodge(0.5), width = 0.2) +
  labs(x = "Revenue use", y = "Effectiveness")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Revenue use", y = "Effectiveness")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('Unspecified','PoorHH','Climate','AllHH',
                              'PoorHH&Climate','AllHH&Climate'))+ 
  theme(axis.text.x = element_text(size = 10, angle = 90))


DataCorrplot<-data[,c(1,16,18,30:34)]
names(DataCorrplot)<-c("Response.ID","Experiment","CT.Fairness_Rev0","CT.Fairness_Rev1","CT.Fairness_Rev2","CT.Fairness_Rev3","CT.Fairness_Rev4","CT.Fairness_Rev5")
data_long<-reshape(DataCorrplot, direction = "long", varying = names(DataCorrplot)[3:8], idvar = "Response.ID", sep="_",timevar="Question")

myData <- aggregate(data_long$CT.Fairness,
                    by = list(data_long$Question),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = RevenueUse))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Fairness")+ theme(
    axis.text.x = element_blank())+
  scale_fill_discrete(name = "Revenue use", 
                      labels = c('Unspecified','Low-income  transfers','Climate projects',
                                 'Universal transfers','Low-income+Climate','Universal+Climate'))


ggplot(data = myData, aes(x = RevenueUse, y = mean, ymax = myData$mean + 2*myData$se,
                               ymin = myData$mean - 2*myData$se))+
 geom_point(aes(color = RevenueUse)) +
  geom_errorbar(aes(color = RevenueUse)) +
  labs(x = "Revenue use", y = "Fairness")+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(name = "Revenue use", 
                   labels = c('Unspecified','Low-income  transfers','Climate projects',
                              'Universal transfers','Low-income+Climate','Universal+Climate'))#+ 


myData <- aggregate(data_long$CT.Fairness,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = factor(InformationProvided)))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Fairness")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
                              'Low-income+Climate','Universal+Climate'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = RevenueUse, y = mean))+ 
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  labs(x = "Revenue use", y = "Fairness")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Revenue use", y = "Fairness")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('Unspecified','PoorHH','Climate','AllHH',
                              'PoorHH&Climate','AllHH&Climate'))+ 
  theme(axis.text.x = element_text(size = 10, angle = 90))




DataCorrplot<-data[,c(1,16,20,35:39)]
names(DataCorrplot)<-c("Response.ID","Experiment","CT.PersonalCost_Rev0","CT.PersonalCost_Rev1","CT.PersonalCost_Rev2","CT.PersonalCost_Rev3","CT.PersonalCost_Rev4","CT.PersonalCost_Rev5")
data_long<-reshape(DataCorrplot, direction = "long", varying = names(DataCorrplot)[3:8], idvar = "Response.ID", sep="_",timevar="Question")

myData <- aggregate(data_long$CT.PersonalCost,
                    by = list(data_long$Question),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')


dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = RevenueUse))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "PersonalCost")+ theme(
    axis.text.x = element_blank())+
  scale_fill_discrete(name = "Revenue use", 
                      labels = c('Unspecified','Low-income  transfers','Climate projects',
                                 'Universal transfers','Low-income+Climate','Universal+Climate'))


 ggplot(data = myData, aes(x = RevenueUse, y = mean, ymax = myData$mean + 2*myData$se,
                               ymin = myData$mean - 2*myData$se))+
geom_point(aes(color = RevenueUse)) +
  geom_errorbar(aes(color = RevenueUse)) +
  labs(x = "Revenue use", y = "PersonalCost")+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(name = "Revenue use", 
                   labels = c('Unspecified','Low-income  transfers','Climate projects',
                              'Universal transfers','Low-income+Climate','Universal+Climate'))#+ 

myData <- aggregate(data_long$CT.PersonalCost,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = factor(InformationProvided)))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Personal cost")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
                              'Low-income+Climate','Universal+Climate'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = RevenueUse, y = mean))+
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  labs(x = "Revenue use", y = "Personal cost")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Revenue use", y = "Personal well-being")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('Unspecified','PoorHH','Climate','AllHH',
                              'PoorHH&Climate','AllHH&Climate'))+ 
  theme(axis.text.x = element_text(size = 10, angle = 90))



DataCorrplot<-data[,c(1,16,21,40:44)]
names(DataCorrplot)<-c("Response.ID","Experiment","CT.LowIncomeEffect_Rev0","CT.LowIncomeEffect_Rev1","CT.LowIncomeEffect_Rev2","CT.LowIncomeEffect_Rev3","CT.LowIncomeEffect_Rev4","CT.LowIncomeEffect_Rev5")
data_long<-reshape(DataCorrplot, direction = "long", varying = names(DataCorrplot)[3:8], idvar = "Response.ID", sep="_",timevar="Question")

myData <- aggregate(data_long$CT.LowIncomeEffect,
                    by = list(data_long$Question),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')


dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = RevenueUse))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Low income effect")+ theme(
    axis.text.x = element_blank())+
  scale_fill_discrete(name = "Revenue use", 
                      labels = c('Unspecified','Low-income  transfers','Climate projects',
                                 'Universal transfers','Low-income+Climate','Universal+Climate'))


ggplot(data = myData, aes(x = RevenueUse, y = mean, ymax = myData$mean + 2*myData$se,
                               ymin = myData$mean - 2*myData$se))+
geom_point(aes(color = RevenueUse)) +
  geom_errorbar(aes(color = RevenueUse)) +
  labs(x = "Revenue use", y = "Low income effect")+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(name = "Revenue use", 
                   labels = c('Unspecified','Low-income  transfers','Climate projects',
                              'Universal transfers','Low-income+Climate','Universal+Climate'))#+ 


myData <- aggregate(data_long$CT.LowIncomeEffect,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = factor(InformationProvided)))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Low income effect")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
                              'Low-income+Climate','Universal+Climate'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = RevenueUse, y = mean))+ 
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  labs(x = "Revenue use", y = "Low income effect")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Revenue use", y = "Low-income effect")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('Unspecified','PoorHH','Climate','AllHH',
                              'PoorHH&Climate','AllHH&Climate'))+ 
  theme(axis.text.x = element_text(size = 10, angle = 90))




DataCorrplot<-data[,c(1,16,23,45:49)]
names(DataCorrplot)<-c("Response.ID","Experiment","CT.Acceptability_Rev0","CT.Acceptability_Rev1","CT.Acceptability_Rev2","CT.Acceptability_Rev3","CT.Acceptability_Rev4","CT.Acceptability_Rev5")
data_long<-reshape(DataCorrplot, direction = "long", varying = names(DataCorrplot)[3:8], idvar = "Response.ID", sep="_",timevar="Question")

myData <- aggregate(data_long$CT.Acceptability,
                    by = list(data_long$Question),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')


dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = RevenueUse))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Acceptability")+ theme(
    axis.text.x = element_blank())+
  scale_fill_discrete(name = "Revenue use", 
                      labels = c('Unspecified','Low-income  transfers','Climate projects',
                                 'Universal transfers','Low-income+Climate','Universal+Climate'))


ggplot(data = myData, aes(x = RevenueUse, y = mean, ymax = myData$mean + 2*myData$se,
                               ymin = myData$mean - 2*myData$se))+
geom_point(aes(color = RevenueUse)) +
  geom_errorbar(aes(color = RevenueUse)) +
  labs(x = "Revenue use", y = "Acceptability")+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(name = "Revenue use", 
                   labels = c('Unspecified','Low-income  transfers','Climate projects',
                              'Universal transfers','Low-income+Climate','Universal+Climate'))#+ 

myData <- aggregate(data_long$CT.Acceptability,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("RevenueUse","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0 Unspecified','1 Low-income  transfers','2 Climate projects',
                     '3 Universal transfers','4 Low-income+Climate','5 Universal+Climate')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = RevenueUse, y = mean, fill = factor(InformationProvided)))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "Revenue use", y = "Acceptability")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  scale_x_discrete(labels = c('Unspecified','Low-income transfers','Climate projects','Universal transfers',
                              'Low-income+Climate','Universal+Climate'))+ 
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = RevenueUse, y = mean))+ 
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  labs(x = "Revenue use", y = "Acceptability")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Revenue use", y = "Acceptability")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('Unspecified','PoorHH','Climate','AllHH',
                              'PoorHH&Climate','AllHH&Climate'))+ 
  theme(axis.text.x = element_text(size = 10, angle = 90))


par(mfrow = c(1,1))
library(conover.test)
sig_level_1<-0.01
sig_level_2<-0.05
sig_level_3<-0.10
data_selected<-cbind(data$CT.Effectiveness     , data$CT.Effectiveness_Rev1,
                     data$CT.Effectiveness_Rev2, data$CT.Effectiveness_Rev3,
                     data$CT.Effectiveness_Rev4, data$CT.Effectiveness_Rev5,
                     data$Experiment.Group)
Sig_matrix_Effectiveness<-matrix(0,6,7)
Sig_matrix_Effectiveness_p<-matrix(1,6,7)
for (i in 1:6){
  for (j in 1:6){
w_test<-wilcox.test(data_selected[,i], data_selected[,j], paired = TRUE, alternative = "greater")
if (w_test$p.value<sig_level_1){Sig_matrix_Effectiveness[i,j]<-3
Sig_matrix_Effectiveness_p[i,j]<-w_test$p.value}
else if (w_test$p.value<sig_level_2){Sig_matrix_Effectiveness[i,j]<-2
Sig_matrix_Effectiveness_p[i,j]<-w_test$p.value}

else if (w_test$p.value<sig_level_3){Sig_matrix_Effectiveness[i,j]<-1
Sig_matrix_Effectiveness_p[i,j]<-w_test$p.value}
#Sig_matrix_Effectiveness[i,j]<-1-w_test$p.value

  }
  j<-7
c_test<-conover.test(data_selected[,i], g=data_selected[,j],method = "bonferroni",alpha=0.01)
if (c_test$P.adjusted<sig_level_1/2){Sig_matrix_Effectiveness[i,j]<-3
Sig_matrix_Effectiveness_p[i,j]<-c_test$P.adjusted*2}
else if (c_test$P.adjusted<sig_level_2/2){Sig_matrix_Effectiveness[i,j]<-2
Sig_matrix_Effectiveness_p[i,j]<-c_test$P.adjusted*2} 
else if (c_test$P.adjusted<sig_level_3/2){Sig_matrix_Effectiveness[i,j]<-1
Sig_matrix_Effectiveness_p[i,j]<-c_test$P.adjusted*2} 
}
colnames(Sig_matrix_Effectiveness)<-c('Unspecified','PoorHH','Climate','AllHH',
                                      'PoorHH&Climate','AllHH&Climate',"Info provided")
rownames(Sig_matrix_Effectiveness)<-c('Unspecified','PoorHH','Climate','AllHH',
                                      'PoorHH&Climate','AllHH&Climate')
corrplot(Sig_matrix_Effectiveness, 
         tl.cex=1,is.corr = FALSE, cl.pos = "n",
         tl.col = "black",cl.lim = c(0,3), p.mat = Sig_matrix_Effectiveness_p, 
         method = "square", sig.level = c(.01, .05, .1), pch.cex = .9,
         insig = "label_sig", pch.col = "white")

#the dark color indicates instances whe the revenue use from the respective row 
#dominates the revenue use from the respective column

data_selected<-cbind(data$CT.Fairness     , data$CT.Fairness_Rev1,
                     data$CT.Fairness_Rev2, data$CT.Fairness_Rev3,
                     data$CT.Fairness_Rev4, data$CT.Fairness_Rev5,
                     data$Experiment.Group)
Sig_matrix_Fairness<-matrix(0,6,7)
Sig_matrix_Fairness_p<-matrix(1,6,7)
for (i in 1:6){
  for (j in 1:6){
    w_test<-wilcox.test(data_selected[,i], data_selected[,j], paired = TRUE, alternative = "greater")
    if (w_test$p.value<sig_level_1){Sig_matrix_Fairness[i,j]<-3
    Sig_matrix_Fairness_p[i,j]<-w_test$p.value}
    else if (w_test$p.value<sig_level_2){Sig_matrix_Fairness[i,j]<-2
    Sig_matrix_Fairness_p[i,j]<-w_test$p.value}
    
    else if (w_test$p.value<sig_level_3){Sig_matrix_Fairness[i,j]<-1
    Sig_matrix_Fairness_p[i,j]<-w_test$p.value}
    #Sig_matrix_Fairness[i,j]<-1-w_test$p.value
    
  }
  j<-7
  c_test<-conover.test(data_selected[,i], g=data_selected[,j],method = "bonferroni",alpha=0.01)
  if (c_test$P.adjusted<sig_level_1/2){Sig_matrix_Fairness[i,j]<-3
  Sig_matrix_Fairness_p[i,j]<-c_test$P.adjusted*2}
  else if (c_test$P.adjusted<sig_level_2/2){Sig_matrix_Fairness[i,j]<-2
  Sig_matrix_Fairness_p[i,j]<-c_test$P.adjusted*2} 
  else if (c_test$P.adjusted<sig_level_3/2){Sig_matrix_Fairness[i,j]<-1
  Sig_matrix_Fairness_p[i,j]<-c_test$P.adjusted*2} 
}
colnames(Sig_matrix_Fairness)<-c('Unspecified','PoorHH','Climate','AllHH',
                                      'PoorHH&Climate','AllHH&Climate',"Info provided")
rownames(Sig_matrix_Fairness)<-c('Unspecified','PoorHH','Climate','AllHH',
                                      'PoorHH&Climate','AllHH&Climate')
corrplot(Sig_matrix_Fairness, 
         tl.cex=1,is.corr = FALSE, cl.pos = "n",
         tl.col = "black",cl.lim = c(0,3), p.mat = Sig_matrix_Fairness_p, 
         method = "square", sig.level = c(.01, .05, .1), pch.cex = .9,
         insig = "label_sig", pch.col = "white")
#the dark color indicates instances whe the revenue use from the respective row 
#dominates the revenue use from the respective column

data_selected<-cbind(data$CT.PersonalCost     , data$CT.PersonalCost_Rev1,
                     data$CT.PersonalCost_Rev2, data$CT.PersonalCost_Rev3,
                     data$CT.PersonalCost_Rev4, data$CT.PersonalCost_Rev5,
                     data$Experiment.Group)
Sig_matrix_PersonalCost<-matrix(0,6,7)
Sig_matrix_PersonalCost_p<-matrix(1,6,7)
for (i in 1:6){
  for (j in 1:6){
    w_test<-wilcox.test(data_selected[,i], data_selected[,j], paired = TRUE, alternative = "greater")
    if (w_test$p.value<sig_level_1){Sig_matrix_PersonalCost[i,j]<-3
    Sig_matrix_PersonalCost_p[i,j]<-w_test$p.value}
    else if (w_test$p.value<sig_level_2){Sig_matrix_PersonalCost[i,j]<-2
    Sig_matrix_PersonalCost_p[i,j]<-w_test$p.value}
    
    else if (w_test$p.value<sig_level_3){Sig_matrix_PersonalCost[i,j]<-1
    Sig_matrix_PersonalCost_p[i,j]<-w_test$p.value}
    #Sig_matrix_PersonalCost[i,j]<-1-w_test$p.value
    
  }
  j<-7
  c_test<-conover.test(data_selected[,i], g=data_selected[,j],method = "bonferroni",alpha=0.01)
  if (c_test$P.adjusted<sig_level_1/2){Sig_matrix_PersonalCost[i,j]<-3
  Sig_matrix_PersonalCost_p[i,j]<-c_test$P.adjusted*2}
  else if (c_test$P.adjusted<sig_level_2/2){Sig_matrix_PersonalCost[i,j]<-2
  Sig_matrix_PersonalCost_p[i,j]<-c_test$P.adjusted*2} 
  else if (c_test$P.adjusted<sig_level_3/2){Sig_matrix_PersonalCost[i,j]<-1
  Sig_matrix_PersonalCost_p[i,j]<-c_test$P.adjusted*2} 
}
colnames(Sig_matrix_PersonalCost)<-c('Unspecified','PoorHH','Climate','AllHH',
                                 'PoorHH&Climate','AllHH&Climate',"Info provided")
rownames(Sig_matrix_PersonalCost)<-c('Unspecified','PoorHH','Climate','AllHH',
                                 'PoorHH&Climate','AllHH&Climate')
corrplot(Sig_matrix_PersonalCost, 
         tl.cex=1,is.corr = FALSE, cl.pos = "n",
         tl.col = "black",cl.lim = c(0,3), p.mat = Sig_matrix_PersonalCost_p, 
         method = "square", sig.level = c(.01, .05, .1), pch.cex = .9,
         insig = "label_sig", pch.col = "white")

#the dark color indicates instances whe the revenue use from the respective row 
#dominates the revenue use from the respective column


data_selected<-cbind(data$CT.LowIncomeEffect     , data$CT.LowIncomeEffect_Rev1,
                     data$CT.LowIncomeEffect_Rev2, data$CT.LowIncomeEffect_Rev3,
                     data$CT.LowIncomeEffect_Rev4, data$CT.LowIncomeEffect_Rev5,
                     data$Experiment.Group)
Sig_matrix_LowIncomeEffect<-matrix(0,6,7)
Sig_matrix_LowIncomeEffect_p<-matrix(1,6,7)
for (i in 1:6){
  for (j in 1:6){
    w_test<-wilcox.test(data_selected[,i], data_selected[,j], paired = TRUE, alternative = "greater")
    if (w_test$p.value<sig_level_1){Sig_matrix_LowIncomeEffect[i,j]<-3
    Sig_matrix_LowIncomeEffect_p[i,j]<-w_test$p.value}
    else if (w_test$p.value<sig_level_2){Sig_matrix_LowIncomeEffect[i,j]<-2
    Sig_matrix_LowIncomeEffect_p[i,j]<-w_test$p.value}
    
    else if (w_test$p.value<sig_level_3){Sig_matrix_LowIncomeEffect[i,j]<-1
    Sig_matrix_LowIncomeEffect_p[i,j]<-w_test$p.value}
    #Sig_matrix_LowIncomeEffect[i,j]<-1-w_test$p.value
    
  }
  j<-7
  c_test<-conover.test(data_selected[,i], g=data_selected[,j],method = "bonferroni",alpha=0.01)
  if (c_test$P.adjusted<sig_level_1/2){Sig_matrix_LowIncomeEffect[i,j]<-3
  Sig_matrix_LowIncomeEffect_p[i,j]<-c_test$P.adjusted*2}
  else if (c_test$P.adjusted<sig_level_2/2){Sig_matrix_LowIncomeEffect[i,j]<-2
  Sig_matrix_LowIncomeEffect_p[i,j]<-c_test$P.adjusted*2} 
  else if (c_test$P.adjusted<sig_level_3/2){Sig_matrix_LowIncomeEffect[i,j]<-1
  Sig_matrix_LowIncomeEffect_p[i,j]<-c_test$P.adjusted*2} 
}
colnames(Sig_matrix_LowIncomeEffect)<-c('Unspecified','PoorHH','Climate','AllHH',
                                     'PoorHH&Climate','AllHH&Climate',"Info provided")
rownames(Sig_matrix_LowIncomeEffect)<-c('Unspecified','PoorHH','Climate','AllHH',
                                     'PoorHH&Climate','AllHH&Climate')
corrplot(Sig_matrix_LowIncomeEffect, 
         tl.cex=1,is.corr = FALSE, cl.pos = "n",
         tl.col = "black",cl.lim = c(0,3), p.mat = Sig_matrix_LowIncomeEffect_p, 
         method = "square", sig.level = c(.01, .05, .1), pch.cex = .9,
         insig = "label_sig", pch.col = "white")
#the dark color indicates instances whe the revenue use from the respective row 
#dominates the revenue use from the respective column


data_selected<-cbind(data$CT.Acceptability     , data$CT.Acceptability_Rev1,
                     data$CT.Acceptability_Rev2, data$CT.Acceptability_Rev3,
                     data$CT.Acceptability_Rev4, data$CT.Acceptability_Rev5,
                     data$Experiment.Group)
Sig_matrix_Acceptability<-matrix(0,6,7)
Sig_matrix_Acceptability_p<-matrix(1,6,7)
for (i in 1:6){
  for (j in 1:6){
    w_test<-wilcox.test(data_selected[,i], data_selected[,j], paired = TRUE, alternative = "greater")
    if (w_test$p.value<sig_level_1){Sig_matrix_Acceptability[i,j]<-3
    Sig_matrix_Acceptability_p[i,j]<-w_test$p.value}
    else if (w_test$p.value<sig_level_2){Sig_matrix_Acceptability[i,j]<-2
    Sig_matrix_Acceptability_p[i,j]<-w_test$p.value}
    
    else if (w_test$p.value<sig_level_3){Sig_matrix_Acceptability[i,j]<-1
    Sig_matrix_Acceptability_p[i,j]<-w_test$p.value}
    #Sig_matrix_Acceptability[i,j]<-1-w_test$p.value
    
  }
  j<-7
  c_test<-conover.test(data_selected[,i], g=data_selected[,j],method = "bonferroni",alpha=0.01)
  if (c_test$P.adjusted<sig_level_1/2){Sig_matrix_Acceptability[i,j]<-3
  Sig_matrix_Acceptability_p[i,j]<-c_test$P.adjusted*2}
  else if (c_test$P.adjusted<sig_level_2/2){Sig_matrix_Acceptability[i,j]<-2
  Sig_matrix_Acceptability_p[i,j]<-c_test$P.adjusted*2} 
  else if (c_test$P.adjusted<sig_level_3/2){Sig_matrix_Acceptability[i,j]<-1
  Sig_matrix_Acceptability_p[i,j]<-c_test$P.adjusted*2} 
}
colnames(Sig_matrix_Acceptability)<-c('Unspecified','PoorHH','Climate','AllHH',
                                        'PoorHH&Climate','AllHH&Climate',"Info provided")
rownames(Sig_matrix_Acceptability)<-c('Unspecified','PoorHH','Climate','AllHH',
                                        'PoorHH&Climate','AllHH&Climate')
corrplot(Sig_matrix_Acceptability, 
         tl.cex=1,is.corr = FALSE, cl.pos = "n",
         tl.col = "black",cl.lim = c(0,3), p.mat = Sig_matrix_Acceptability_p, 
         method = "square", sig.level = c(.01, .05, .1), pch.cex = .9,
         insig = "label_sig", pch.col = "white")
#the dark color indicates instances whe the revenue use from the respective row 
#dominates the revenue use from the respective column




DataCorrplot<-data[,c(1,16,63)]
names(DataCorrplot)<-c("Response.ID","Experiment","ClimateConcern")
data_long<-DataCorrplot


myData <- aggregate(data_long$ClimateConcern,
                    by = list(data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("InformationProvided", "mean", "sd", "n", "se")
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = myData$mean + 2*myData$se,
              ymin = myData$mean - 2*myData$se)
p <- ggplot(data = myData, aes(x = InformationProvided, y = mean))
p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  labs(x = "InformationProvided", y = "Climate Concern")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+ theme(
    axis.text.x = element_blank())+
  theme(axis.text.x = element_text(size = 8, angle = 90))


ggplot(data = myData, aes(x = InformationProvided, y = mean))+ 
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  labs(x = "Revenue use", y = "Climate Concern")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "", y = "Climate Concern")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  theme(axis.text.x = element_text(size = 10, angle = 90))

summary(data$ClimateConcern[data$Experiment.Group==1])
c_test<-conover.test(data$ClimateConcern, g=data$Experiment.Group,method = "bonferroni",alpha=0.01)
c_test<-conover.test(data$Education, g=data$Experiment.Group,method = "bonferroni",alpha=0.01)
data_ct<-data
data_ct$MonthlyIncome[data_ct$MonthlyIncome==7]<-NA
data_ct$PoliticalView[data_ct$PoliticalView==11]<-NA
c_test<-conover.test(data_ct$MonthlyIncome, g=data$Experiment.Group,method = "bonferroni",alpha=0.01)
c_test<-conover.test(data_ct$PoliticalView, g=data$Experiment.Group,method = "bonferroni",alpha=0.01)
c_test<-conover.test(data$CT.TrustInPoliticians, g=data$Experiment.Group,method = "bonferroni",alpha=0.01)







# gender<-matrix(0,1,length(data$Gender))
# gender[data$Gender==1]<-"Male"
# gender[data$Gender==2]<-"Female"
hist(data$Gender, main="Gender distribution", xaxt="n", xlab="")
axis(1, at=c(1,2), labels=c("Male","Female")) 
#sum(data$Gender==1) #number of male respondentents
#sum(data$Gender==2) #number of female respondentents


par(mfrow = c(1, 2) ,mar=c(4,4,1,2))
# gender<-matrix(0,1,length(data$Gender))
# gender[data$Gender==1]<-"Male"
# gender[data$Gender==2]<-"Female"
hist(data$Age, main="Age distribution", xlab="")

hist(data$Age.cohort, main="Age cohort distribution", breaks=c(0,1,2,3,4,5,6,7), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5,6.5), labels=c("0-17","18-24","25-34","35-44","45-54","55-64","65+")) 

table(data$Age.cohort)/length(data$Age.cohort)

par(mfrow = c(1, 2) ,mar=c(10,4,1,2))
data$Regional.Code[data$Regional.Code==99]<-20
hist(data$Regional.Code, main="Regional distribution",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5), labels=c("Andalucía","Aragón","Principado de Asturias","Illes Balears","Canarias","Cantabria","Castilla y León","Castilla-La Mancha","Catalunya",
"Comunitat Valenciana","Extremadura","Galicia","Madrid","Murcia","Navarra","País Vasco","La Rioja","Ceuta","Melilla","NA"),las=2) 


data$Number.Inhabitants[data$Number.Inhabitants==99]<-6
hist(data$Number.Inhabitants, main="Number of inhabitants", breaks=c(0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5), labels=c("<10.000","10.000 - 50.000","50.000 - 100.000","100.000 - 500.000",">500.000","NA"),las=2) 

par(mfrow = c(1, 2) ,mar=c(4,4,1,2))
hist(data$NumberOfPeers, main="Number of (regular) peers", breaks=100, xlab="")
hist(data$NumberOfPeersClimChange, main="Peers to discuss climate change", breaks=100,  xlab="")

par(mfrow = c(1, 2) ,mar=c(4,4,1,2))
hist(data$NumberOfPeers, main="Number of (regular) peers", breaks=10, xlab="")
hist(data$NumberOfPeersClimChange, main="Peers to discuss climate change", breaks=10,  xlab="")
#summary(data)
boxplot(data$NumberOfPeers,data=data, main="Number of (regular) peers",xlab="", ylab="") 
boxplot(data$NumberOfPeersClimChange,data=data, main="Peers to discuss climate change",xlab="", ylab="") 
par(mfrow = c(1, 3) ,mar=c(4,4,1,2))
hist(data$ExpectedSpanishAcceptability, main="Expected acceptability rate in Spain", breaks=100, xlab="")
hist(data$ExpectedSpanishAcceptability, main="Expected acceptability rate in Spain", breaks=10, xlab="")
boxplot(data$ExpectedSpanishAcceptability,data=data, main="Expected acceptability rate in Spain",xlab="", ylab="") 

data$InfoSpanishAcceptability[data$InfoSpanishAcceptability==1]<-19
data$InfoSpanishAcceptability[data$InfoSpanishAcceptability==2]<-67
hist(data$ExpectedSpanishAcceptability[data$InfoSpanishAcceptability==19], main="Expected rate of those received 19%", breaks=100, xlab="")
hist(data$ExpectedSpanishAcceptability[data$InfoSpanishAcceptability==67], main="Expected rate of those received 67%", breaks=100, xlab="")
boxplot(ExpectedSpanishAcceptability~InfoSpanishAcceptability,data=data, main="Expected acceptability rate in Spain",xlab="Rate provided", ylab="") 


par(mfrow = c(1, 2) ,mar=c(10,4,1,2))
hist(data$HouseholdSize, main="Household size",breaks=100, xlab="")


hist(data$MonthlyIncome, main="Monthly income", breaks=c(0,1,2,3,4,5,6,7), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5,6.5), labels=c("No income","<1000","1001 - 2000","2001 - 3000","3001 - 4000",">4000","I prefer not to answer"),las=2)

par(mfrow = c(1, 2) ,mar=c(10,4,1,2))
hist(data$Education, main="Education",  breaks=c(0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5), labels=c("> 5 years of school","primary","secondary","medium prof formation","superior professional","univeristy"),las=2)

hist(data$ClimateConcern, main="Climate Concern",  breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("Not at all","Little","Somewhat","Much","very much"),las=2)

par(mfrow = c(1,2) ,mar=c(14,4,1,2))
hist(data$PoliticalView, main=" Political view",  breaks=c(0,1,2,3,4,5,6,7,8,9,10,11), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5), labels=c("1 left-wing","2","3","4","5","6","7","8","9","10 right-wing","Don't know/ prefer not to answer"),las=2)


hist(data$PoliticalParty, main="Political party voted",  breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5), labels=c("PSOE","PP","Ciudadanos","Unidas Podemos","Vox","ERC-Sobiranistes","JxCat","EAJ-PNV","EH Bildu","CC-PNC","NA+","Compromís","PRC","'Otros' Especificar","Voté en blanco","Voté nulo","No voté en estas elecciones"),las=2)

par(mfrow = c(1,2) ,mar=c(14,4,1,2))
hist(data$CarUse, main="Car use",  breaks=c(0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(1,2,3,4,5,6), labels=c("Never","<1 a month","1-2 a month","1 a week","3 a week","Everyday"),las=2)



hist(data$DrivingTime, main="Driving time",  breaks=c(0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5), labels=c("Nothing","<30 minutes","30 - 60 minutes","61 - 90 minutes","91 - 120 minutes",">120 minutes"),las=2)

sp <- ggscatter(data, x = "CarUse", y = "DrivingTime",
add = "reg.line",  # Add regressin line
add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
conf.int = TRUE)
# Add correlation coefficient
sp + stat_cor(method = "spearman", label.x = 3, label.y = 10)

data$Text.CarbonTaxPolicy<-as.character(data$Text.CarbonTaxPolicy)
#Average length of response on economic growth is 1.94 words
c1<-c()
for (i in 1:length(data$Text.CarbonTaxPolicy)){
  c1<-c(c1,wordcount(data$Text.CarbonTaxPolicy[i]))}

par(mfrow = c(1, 1) ,mar=c(4,4,1,1))

hist(c1,breaks=max(c1), main="Length of responses on first open question",
     xlab="# of words", xlim = c(1,max(c1)))
summary(c1)


data$Text.CTFairness<-as.character(data$Text.CTFairness)
#Average length of response on economic growth is 1.94 words
c2<-c()
for (i in 1:length(data$Text.CTFairness))
  {c2<-c(c2,wordcount(data$Text.CTFairness[i]))}
par(mfrow = c(1, 1) ,mar=c(4,4,1,1))

hist(c2,breaks=180, main="Length of responses on second open question",xlab="# of words")
summary(c2)

dataframe_responses<-cbind(data$Duration, c1,c2, c1+c2)
colnames(dataframe_responses)<-c("Duration", "L Q1", "L Q2", "L Q1 + Q2")


M_responses <- cor(dataframe_responses)
corrplot.mixed(M_responses)


par(mfrow = c(3, 3) ,mar=c(8,4,1,1))

hist(data$Gender, main="Gender", xaxt="n", xlab="")
axis(1, at=c(1,2), labels=c("Male","Female")) 

hist(data$Age, main="Age", xlab="")

hist(data$Education, main="Education",  breaks=c(0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5), labels=c("> 5ys school","primary","secondary","med prof form","sup professional","univeristy"),las=2)

hist(data$CarUse, main="Car use",  breaks=c(0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5), labels=c("Never","<1 a month","1-2 a month","1 a week","3 a week","Everyday"),las=2)

hist(data$Perceived.CarbonTaxKnowledge, main="Perceived knowledge about carbon tax",breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("	Not at all","A little","Somewhat","A lot","Very much"),las=2) 

hist(data$CT.Acceptability, main="Acceptability of a carbon tax",  breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("Completely not","Somewhat not","Neither nor","Somewhat yes","Completely yes"),las=2)




#par(mfrow = c(1, 1) ,mar=c(8,4,1,1))
hist(data$CT.Fairness, main="Perceived fairness of a carbon tax",  breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("Very unfair","Somewhat unfair","Neither nor","Somewhat fair","Very fair"),las=2)



data$KnowledgePoints=matrix(0,length(data$Knowledge.Test1),1)
data$KnowledgePoints[data$Knowledge.Test1==1]=data$KnowledgePoints[data$Knowledge.Test1==1]+1  #correct answer for Q1 was 1
data$KnowledgePoints[data$Knowledge.Test1==2]=data$KnowledgePoints[data$Knowledge.Test1==2]-1
data$KnowledgePoints[data$Knowledge.Test1==3]=data$KnowledgePoints[data$Knowledge.Test1==3]+0

data$KnowledgePoints[data$Knowledge.Test2==1]=data$KnowledgePoints[data$Knowledge.Test2==1]-1  #correct answer for Q2 was 2
data$KnowledgePoints[data$Knowledge.Test2==2]=data$KnowledgePoints[data$Knowledge.Test2==2]+1
data$KnowledgePoints[data$Knowledge.Test2==3]=data$KnowledgePoints[data$Knowledge.Test2==3]+0

data$KnowledgePoints[data$Knowledge.Test3==1]=data$KnowledgePoints[data$Knowledge.Test3==1]-1  #correct answer for Q3 was 2
data$KnowledgePoints[data$Knowledge.Test3==2]=data$KnowledgePoints[data$Knowledge.Test3==2]+1
data$KnowledgePoints[data$Knowledge.Test3==3]=data$KnowledgePoints[data$Knowledge.Test3==3]+0

data$KnowledgePoints[data$Knowledge.Test4==1]=data$KnowledgePoints[data$Knowledge.Test4==1]-1  #correct answer for Q4 was 2
data$KnowledgePoints[data$Knowledge.Test4==2]=data$KnowledgePoints[data$Knowledge.Test4==2]+1
data$KnowledgePoints[data$Knowledge.Test4==3]=data$KnowledgePoints[data$Knowledge.Test4==3]+0

data$KnowledgePoints[data$Knowledge.Test5==1]=data$KnowledgePoints[data$Knowledge.Test5==1]+1  #correct answer for Q5 was 1
data$KnowledgePoints[data$Knowledge.Test5==2]=data$KnowledgePoints[data$Knowledge.Test5==2]-1
data$KnowledgePoints[data$Knowledge.Test5==3]=data$KnowledgePoints[data$Knowledge.Test5==3]+0

data$KnowledgePoints[data$Knowledge.Test6==1]=data$KnowledgePoints[data$Knowledge.Test6==1]-1  #correct answer for Q6 was 2
data$KnowledgePoints[data$Knowledge.Test6==2]=data$KnowledgePoints[data$Knowledge.Test6==2]+1
data$KnowledgePoints[data$Knowledge.Test6==3]=data$KnowledgePoints[data$Knowledge.Test6==3]+0

data$KPts1<-matrix(0,length(data$Knowledge.Test1),1)
data$KPts1[data$Knowledge.Test1==1]<- 1
data$KPts1[data$Knowledge.Test1==2]<- -1

data$KPts2<-matrix(0,length(data$Knowledge.Test1),1)
data$KPts2[data$Knowledge.Test2==1]<- -1
data$KPts2[data$Knowledge.Test2==2]<- 1

data$KPts3<-matrix(0,length(data$Knowledge.Test1),1)
data$KPts3[data$Knowledge.Test3==1]<- -1
data$KPts3[data$Knowledge.Test3==2]<- 1

data$KPts4<-matrix(0,length(data$Knowledge.Test1),1)
data$KPts4[data$Knowledge.Test4==1]<- -1
data$KPts4[data$Knowledge.Test4==2]<- 1

data$KPts5<-matrix(0,length(data$Knowledge.Test1),1)
data$KPts5[data$Knowledge.Test5==1]<- 1
data$KPts5[data$Knowledge.Test5==2]<- -1

data$KPts6<-matrix(0,length(data$Knowledge.Test1),1)
data$KPts6[data$Knowledge.Test6==1]<- -1
data$KPts6[data$Knowledge.Test6==2]<- 1

#an alternative measure of knowledge counting only positives 
data$KnowledgePoints01=matrix(0,length(data$Knowledge.Test1),1)
data$KnowledgePoints01[data$Knowledge.Test1==1]=data$KnowledgePoints01[data$Knowledge.Test1==1]+1  #correct answer for Q1 was 1
data$KnowledgePoints01[data$Knowledge.Test1==2]=data$KnowledgePoints01[data$Knowledge.Test1==2]-0
data$KnowledgePoints01[data$Knowledge.Test1==3]=data$KnowledgePoints01[data$Knowledge.Test1==3]+0

data$KnowledgePoints01[data$Knowledge.Test2==1]=data$KnowledgePoints01[data$Knowledge.Test2==1]-0  #correct answer for Q2 was 2
data$KnowledgePoints01[data$Knowledge.Test2==2]=data$KnowledgePoints01[data$Knowledge.Test2==2]+1
data$KnowledgePoints01[data$Knowledge.Test2==3]=data$KnowledgePoints01[data$Knowledge.Test2==3]+0

data$KnowledgePoints01[data$Knowledge.Test3==1]=data$KnowledgePoints01[data$Knowledge.Test3==1]-0  #correct answer for Q3 was 2
data$KnowledgePoints01[data$Knowledge.Test3==2]=data$KnowledgePoints01[data$Knowledge.Test3==2]+1
data$KnowledgePoints01[data$Knowledge.Test3==3]=data$KnowledgePoints01[data$Knowledge.Test3==3]+0

data$KnowledgePoints01[data$Knowledge.Test4==1]=data$KnowledgePoints01[data$Knowledge.Test4==1]-0  #correct answer for Q4 was 2
data$KnowledgePoints01[data$Knowledge.Test4==2]=data$KnowledgePoints01[data$Knowledge.Test4==2]+1
data$KnowledgePoints01[data$Knowledge.Test4==3]=data$KnowledgePoints01[data$Knowledge.Test4==3]+0

data$KnowledgePoints01[data$Knowledge.Test5==1]=data$KnowledgePoints01[data$Knowledge.Test5==1]+1  #correct answer for Q5 was 1
data$KnowledgePoints01[data$Knowledge.Test5==2]=data$KnowledgePoints01[data$Knowledge.Test5==2]-0
data$KnowledgePoints01[data$Knowledge.Test5==3]=data$KnowledgePoints01[data$Knowledge.Test5==3]+0

data$KnowledgePoints01[data$Knowledge.Test6==1]=data$KnowledgePoints01[data$Knowledge.Test6==1]-0  #correct answer for Q6 was 2
data$KnowledgePoints01[data$Knowledge.Test6==2]=data$KnowledgePoints01[data$Knowledge.Test6==2]+1
data$KnowledgePoints01[data$Knowledge.Test6==3]=data$KnowledgePoints01[data$Knowledge.Test6==3]+0


data$KPtsBi1<-matrix(0,length(data$Knowledge.Test1),1)
data$KPtsBi1[data$Knowledge.Test1==1]<- 1

data$KPtsBi2<-matrix(0,length(data$Knowledge.Test1),1)
data$KPtsBi2[data$Knowledge.Test2==2]<- 1

data$KPtsBi3<-matrix(0,length(data$Knowledge.Test1),1)
data$KPtsBi3[data$Knowledge.Test3==2]<- 1

data$KPtsBi4<-matrix(0,length(data$Knowledge.Test1),1)
data$KPtsBi4[data$Knowledge.Test4==2]<- 1

data$KPtsBi5<-matrix(0,length(data$Knowledge.Test1),1)
data$KPtsBi5[data$Knowledge.Test5==1]<- 1

data$KPtsBi6<-matrix(0,length(data$Knowledge.Test1),1)
data$KPtsBi6[data$Knowledge.Test6==2]<- 1



hist(data$KnowledgePoints, main="Actual knowledge (based on 6 questions)",breaks=c(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6), xaxt="n", xlab="")
axis(1, at=c(-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5, 0.5,1.5,2.5,3.5,4.5,5.5), labels=c("-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6"),las=1)
table(data$KnowledgePoints)

sp <- ggscatter(data, x = "Perceived.CarbonTaxKnowledge", y = "KnowledgePoints01",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
                ylim=c(0,+6))
# Add correlation coefficient
sp + stat_cor(method = "spearman", label.x = 3, label.y = 1.5, cex=3)


Distribution_of_responses<-matrix(0,3,6)
Distribution_of_responses[,1]<-table(data$Knowledge.Test1)
Distribution_of_responses[,2]<-table(data$Knowledge.Test2)
Distribution_of_responses[,3]<-table(data$Knowledge.Test3)
Distribution_of_responses[,4]<-table(data$Knowledge.Test4)
Distribution_of_responses[,5]<-table(data$Knowledge.Test5)
Distribution_of_responses[,6]<-table(data$Knowledge.Test6)

colnames(Distribution_of_responses)<-c("Question 1","Question 2","Question 3","Question 4","Question 5","Question 6")
barplot(Distribution_of_responses,main="Distribution of responses on knowledge questions",
        xlab="", col=c("darkblue","red","white"),
        legend = c("True","False","Don't know"),las=2)

Distribution_of_responses_CI<-Distribution_of_responses
Distribution_of_responses_CI[,2]<-Distribution_of_responses[c(2,1,3),2]
Distribution_of_responses_CI[,3]<-Distribution_of_responses[c(2,1,3),3]
Distribution_of_responses_CI[,4]<-Distribution_of_responses[c(2,1,3),4]
Distribution_of_responses_CI[,6]<-Distribution_of_responses[c(2,1,3),6]

# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

names_questions=c("A carbon tax is levied on the carbon content of fossil fuels, such as coal and oil (TRUE)",
            "A carbon tax mandates all producers and consumers which low-carbon technology they should adopt (FALSE)",
            "A carbon tax makes renewable energy sources, such as solar electricity, more expensive than fossil fuels (FALSE)",
            "A carbon tax imposes a legally binding limit on the amount of CO2 emissions that firms and consumers are allowed to emit (FALSE)",
            "A carbon tax allows reducing other, existing taxes such as VAT or labour taxes (TRUE)",
            "A carbon tax will raise the price of coal and reduce the price of gasoline (FALSE)")


wr.lap <- wrap.labels(names_questions, 40)

par(mfrow = c(1, 1) ,mar=c(7,15,1,4))
barplot(Distribution_of_responses_CI[,(seq(6,1,by=-1))],main="",
        xlab="", col=c("darkblue","red","white"),
        horiz = TRUE,
        names.arg = rev(wr.lap),#reorder(wr.lap,seq(6,1,by=-1)),
        cex.names=0.9,
        legend = c("Correct response","Incorrect response","Do not know"),
        args.legend = list(x="bottomright",bty="n", inset=c(1.2,-.2), xpd = TRUE,ncol=1),
        #beside=TRUE,
        las=2)

library(dplyr)
df.summary <- data %>%
  group_by(CT.Acceptability) %>%
  summarise(
    sd = sd(Perceived.CarbonTaxKnowledge, na.rm = TRUE),
    se = sd(Perceived.CarbonTaxKnowledge, na.rm = TRUE)/length(Perceived.CarbonTaxKnowledge)^(1/2),
    Perceived.CarbonTaxKnowledge = mean(Perceived.CarbonTaxKnowledge)
  )
df.summary


df.summary1 <- data %>%
  group_by(CT.Acceptability) %>%
  summarise(
    sd = sd(KnowledgePoints, na.rm = TRUE),
    se = sd(KnowledgePoints, na.rm = TRUE)/length(KnowledgePoints)^(1/2),
    KnowledgePoints = mean(KnowledgePoints)
  )
df.summary1

df.summary2 <- data %>%
  group_by(CT.Acceptability) %>%
  summarise(
    sd = sd(KnowledgePoints01, na.rm = TRUE),
    se = sd(KnowledgePoints01, na.rm = TRUE)/length(KnowledgePoints01)^(1/2),
    KnowledgePoints01 = mean(KnowledgePoints01)
  )
df.summary2

ggplot(data, aes(CT.Acceptability, Perceived.CarbonTaxKnowledge)) +
  # geom_jitter(
  #   position = position_jitter(0.2), color = "darkgray"
  # ) + 
  geom_pointrange(
    aes(ymin = Perceived.CarbonTaxKnowledge-se, ymax = Perceived.CarbonTaxKnowledge+se),
    data = df.summary
  )+
  labs(x = "CT Acceptability", y = "Perceived knowledge")

ggplot(data, aes(CT.Acceptability, KnowledgePoints)) +
  # geom_jitter(
  #   position = position_jitter(0.2), color = "darkgray"
  # ) + 
  geom_pointrange(
    aes(ymin = KnowledgePoints-se, ymax = KnowledgePoints+se),
    data = df.summary1
  )

ggplot(data, aes(CT.Acceptability, KnowledgePoints01)) +
  # geom_jitter(
  #   position = position_jitter(0.2), color = "darkgray"
  # ) + 
  geom_pointrange(
    aes(ymin = KnowledgePoints01-se, ymax = KnowledgePoints01+se),
    data = df.summary2
  )


DataCorrplot_KPts<-data[,c(76:81)]
par(mfrow = c(1, 1) ,mar=c(1,1,1,2))
M_KPts <- cor(DataCorrplot_KPts)
corrplot(M_KPts, method = "circle",tl.cex=.5,tl.col = "black",col=colorRampPalette(c("blue","white","red"))(200))


DataCorrplot_KPtsBi<-data[,c(83:88)]
par(mfrow = c(1, 1) ,mar=c(1,1,1,2))
M_KPtsBi <- cor(DataCorrplot_KPtsBi)

colnames(M_KPtsBi)<-c('Q1','Q2','Q3','Q4','Q5','Q6')
rownames(M_KPtsBi)<-c('Q1','Q2','Q3','Q4','Q5','Q6')
corrplot(M_KPtsBi, method = "circle",tl.cex=1,tl.col = "black",col=colorRampPalette(c("blue","white","red"))(200))



library(mokken)
coefH(DataCorrplot_KPtsBi)
#we have a weak scale of 0.346 mainly because of the fifth item
check.reliability(DataCorrplot_KPtsBi)

coefH(DataCorrplot_KPtsBi[,-5])
#but throwing the fifth item away does not help a lot as the scale remains below 0.4 (0.372)
check.reliability(DataCorrplot_KPtsBi[,-5])

monotonicity.com <- check.monotonicity(DataCorrplot_KPtsBi)
summary(monotonicity.com)
plot(monotonicity.com)
# even though the results for items 4 and 5 are at best weakly monotonic, 
#we find no strict violation of monotonicity

pmatrix.com <- check.pmatrix(DataCorrplot_KPtsBi)
summary(pmatrix.com)
#plot(pmatrix.com)
#we have no violation of the nonintersection test

restscore.com <- check.restscore(DataCorrplot_KPtsBi)
summary(restscore.com)
#plot(restscore.com)
#the resscore test identifies few violations, but finds them not significant

aisp(DataCorrplot_KPtsBi, lowerbound = .3, alpha = .05)
#the automated item selection procedure shows that all items except item 5 
#belong to the same Mokken scale

# check.monotonicity(DataCorrplot_KPtsBi, minvi = .03)
# check.pmatrix(DataCorrplot_KPtsBi, minvi = .03)
# check.restscore(DataCorrplot_KPtsBi, minvi = .03)

data$KnowledgePoints01ed<-data$KnowledgePoints01-data$KPtsBi5


par(mfrow = c(1, 1) ,mar=c(6,4,1,2))
hist(data$Perceived.CarbonTaxKnowledge, main="Self-perceived knowledge",breaks=c(0,1,2,3,4,5), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5), labels=c("	Not at all","A little","Somewhat","A lot","Very much"),las=2) 
#sum(data$Perceived.CarbonTaxKnowledge==1)
#sum(data$Perceived.CarbonTaxKnowledge==2)

hist(data$KnowledgePoints01ed, main="Assessed knowledge",breaks=c(-1,0,1,2,3,4,5), xaxt="n",  xlab="")
axis(1, at=c(-.5,.5,1.5,2.5,3.5,4.5), labels=c(0,1,2,3,4,5)) 
#sum(data$Perceived.CarbonTaxKnowledge==1)
#sum(data$Perceived.CarbonTaxKnowledge==2)
summary(data$KnowledgePoints01ed)


data_long<-data[,c(1,16,23,89)]
data_long[,4]<-as.character(data_long[,4])
names(data_long)<-c("Response.ID","Experiment","CT.Acceptability","Question")

myData <- aggregate(data_long$CT.Acceptability,
                    by = list(data_long$Question,data_long$Experiment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),n= length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)
colnames(myData) <- c("Assessed knowledge","InformationProvided", "mean", "sd", "n", "se")
myData$RevenueUse<-c('0','1','2','3','4','5')
myData$InformationProvided<-as.character(myData$InformationProvided)
myData$InformationProvided[myData$InformationProvided=="1"]<-"No"
myData$InformationProvided[myData$InformationProvided=="2"]<-"Yes"


ggplot(data = myData, aes(x = RevenueUse, y = mean))+ 
  geom_errorbar(aes(ymin = mean-2*se, ymax = mean+2*se, color = InformationProvided),
                position= position_dodge(0.5), width = 0.2) +
  labs(x = "Assessed knowledge", y = "Acceptability")+
  geom_point(aes(color = InformationProvided), position = position_dodge(0.5)) +
  labs(x = "Assessed knowledge", y = "Acceptability")+
  scale_fill_discrete(name = "Information provision", labels=c("no", "yes"))+
  scale_x_discrete(labels = c('0','1','2','3','4','5'))+ 
  theme(axis.text.x = element_text(size = 10, angle = 0))

sig_level<-0.10
Sig_matrix_Knowledge<-matrix(0,1,6)
for (i in 0:5){
w_test<-wilcox.test(data_long$CT.Acceptability[data_long$Question==i & data_long$Experiment==1], 
                    data_long$CT.Acceptability[data_long$Question==i & data_long$Experiment==2],
                    alternative = "less")
Sig_matrix_Knowledge[i+1]<-w_test$p.value 
}
colnames(Sig_matrix_Knowledge)<-c("0","1","2","3","4","5")
rownames(Sig_matrix_Knowledge)<-""
library('plot.matrix')
library("RColorBrewer")
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
colfunc<-colorRampPalette(c("springgreen","yellow","yellow","orange","red"))
plot(Sig_matrix_Knowledge, fmt.cell='%.3f', xlab="Assessed knowledge", ylab="",yaxt="n",col="white",main="", key=NULL, digits = 3)


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
ggplot(data, aes(x=Perceived.CarbonTaxKnowledge, y=KnowledgePoints01ed) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+   
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  scale_alpha_continuous(limits=c(0.001,0.02))+ labs(fill = "density", title="",
                                                     x ="Self-perceived knowledge", y = "Assessed knowledge")




sp <- ggscatter(data, x = "Perceived.CarbonTaxKnowledge", y = "KnowledgePoints01ed",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE, # Add confidence interval
                ylim=c(0,+6),
                xlab="Self-assessed knowledge", ylab="Assessed knowledge")
# Add correlation coefficient
sp + stat_cor(method = "spearman", label.x = 3, label.y = 1.5, cex=3)


df.summary3 <- data %>%
  group_by(CT.Acceptability) %>%
  summarise(
    sd = sd(KnowledgePoints01ed, na.rm = TRUE),
    se = sd(KnowledgePoints01ed, na.rm = TRUE)/length(KnowledgePoints01ed)^(1/2),
    KnowledgePoints01ed = mean(KnowledgePoints01ed)
  )
df.summary3



ggplot(data, aes(CT.Acceptability, Perceived.CarbonTaxKnowledge)) +
  # geom_jitter(
  #   position = position_jitter(0.2), color = "darkgray"
  # ) + 
  geom_pointrange(
    aes(ymin = Perceived.CarbonTaxKnowledge-2*se, ymax = Perceived.CarbonTaxKnowledge+2*se),
    data = df.summary
  )+
  labs(x = "Acceptability", y = "Self-perceived knowledge")


ggplot(data, aes(CT.Acceptability, KnowledgePoints01ed)) +
  # geom_jitter(
  #   position = position_jitter(0.2), color = "darkgray"
  # ) + 
  geom_pointrange(
    aes(ymin = KnowledgePoints01ed-2*se, ymax = KnowledgePoints01ed+2*se),
    data = df.summary3
  )+
  labs(x = "Acceptability", y = "Assessed knowledge")

data$Perceived.CarbonTaxKnowledge_z<-(data$Perceived.CarbonTaxKnowledge-mean(data$Perceived.CarbonTaxKnowledge))/sd(data$Perceived.CarbonTaxKnowledge)
data$KnowledgePoints01ed_z<-(data$KnowledgePoints01ed-mean(data$KnowledgePoints01ed))/sd(data$KnowledgePoints01ed)

data$KnowledgeGap<-data$Perceived.CarbonTaxKnowledge_z-data$KnowledgePoints01ed_z
df.summary4 <- data %>%
  group_by(CT.Acceptability) %>%
  summarise(
    sd = sd(KnowledgeGap, na.rm = TRUE),
    se = sd(KnowledgeGap, na.rm = TRUE)/length(KnowledgeGap)^(1/2),
    KnowledgeGap = mean(KnowledgeGap)
  )
df.summary4

ggplot(data, aes(CT.Acceptability, KnowledgeGap)) +
  # geom_jitter(
  #   position = position_jitter(0.2), color = "darkgray"
  # ) + 
  geom_pointrange(
    aes(ymin = KnowledgeGap-2*se, ymax = KnowledgeGap+2*se),
    data = df.summary4
  )+
  #geom_smooth(method='lm',formula=data$KnowledgeGap~data$CT.Acceptability)+
  labs(x = "Acceptability", y = "Knowledge gap")

linearMod <- lm(KnowledgeGap ~ CT.Acceptability, data=data)
summary(linearMod)

ggplot(data, aes(x=CT.Acceptability, y=KnowledgeGap) )+ geom_point()  +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+   
  #geom_smooth(method='lm',formula=data$KnowledgeGap~data$CT.Acceptability)+
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  #ylim(-0.1,.1)+
  coord_cartesian(ylim=c(-.3,.3))
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  labs(fill = "density", title="",
                                                     x ="Acceptability", y = "Knowledge gap")

ggplot(data, aes(x=CT.Acceptability, y=KnowledgeGap) )+ geom_point()  +
  #stat_density_2d(aes(fill = ..level..), geom = "polygon")+   
  geom_smooth(method = "lm", se = FALSE,colour="red")+ 
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  coord_cartesian(ylim=c(-.5,.5))
  #scale_alpha_continuous(limits=c(0.001,0.02))+ 
  labs(fill = "density", title="",
       x ="Acceptability", y = "Knowledge gap")

  
  
  
  
  
  
  
  par(mfrow = c(1,3) ,mar=c(15,4,3,2))
  # barplot(Allocation_Choices,main="Distribution of allocation choices",
  #         col=c("darkblue"),cex.names=0.7,las=2)
  
  barplot(Allocation_Choices_Comp,main="Distribution of allocation choices",
          col=c("green","red","blue"),
          legend = c("Climate","PoorHH","AllHH"),cex.names=1,las=2,ylab="Number of respondents",
          args.legend = list(x="topleft",ncol=1,cex=1,bty="n"))
  
  
  Allocation_Choices_Comp_LowKnw<-matrix(0,3,7)
  
  colnames(Allocation_Choices_Comp_LowKnw)<-c("Climate Only","PoorHH Only","AllHH Only",
                                              "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
  rownames(Allocation_Choices_Comp_LowKnw)<-c("Climate","PoorHH","AllHH")
  #estimate the percentage allocation of climate in climate only  
  Allocation_Choices_Comp_LowKnw[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1 & data$KnowledgePoints01ed<3])/100
  
  Allocation_Choices_Comp_LowKnw[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1 & data$KnowledgePoints01ed<3])/100
  
  Allocation_Choices_Comp_LowKnw[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1 & data$KnowledgePoints01ed<3])/100
  
  Allocation_Choices_Comp_LowKnw[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1 & data$KnowledgePoints01ed<3])/100
  Allocation_Choices_Comp_LowKnw[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1 & data$KnowledgePoints01ed<3])/100
  
  Allocation_Choices_Comp_LowKnw[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1 & data$KnowledgePoints01ed<3])/100
  Allocation_Choices_Comp_LowKnw[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1 & data$KnowledgePoints01ed<3])/100
  
  Allocation_Choices_Comp_LowKnw[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1 & data$KnowledgePoints01ed<3])/100
  Allocation_Choices_Comp_LowKnw[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1 & data$KnowledgePoints01ed<3])/100
  
  Allocation_Choices_Comp_LowKnw[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1 & data$KnowledgePoints01ed<3])/100
  Allocation_Choices_Comp_LowKnw[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1 & data$KnowledgePoints01ed<3])/100
  Allocation_Choices_Comp_LowKnw[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1 & data$KnowledgePoints01ed<3])/100
  #sum(Allocation_Choices[,7]==1)
  barplot(Allocation_Choices_Comp_LowKnw,main="... for respondents with low knowledge",
          col=c("green","red","blue"),
          legend = c("Climate","PoorHH","AllHH"),cex.names=1,las=2,
          args.legend = list(x="topleft",bty="n",ncol=1,cex=1))
  
  
  Allocation_Choices_Comp_HighKnw<-matrix(0,3,7)
  
  colnames(Allocation_Choices_Comp_HighKnw)<-c("Climate Only","PoorHH Only","AllHH Only",
                                               "Climate&PoorHH","Climate&AllHH","AllHH&PoorHH","AllHH&PoorHH&Climate")
  rownames(Allocation_Choices_Comp_HighKnw)<-c("Climate","PoorHH","AllHH")
  #estimate the percentage allocation of climate in climate only  
  Allocation_Choices_Comp_HighKnw[1,1]<-sum(data$Allocation_Climate[Allocation_Choices[,1]==1 & data$KnowledgePoints01ed>=3])/100
  
  Allocation_Choices_Comp_HighKnw[2,2]<-sum(data$Allocation_LowIncome[Allocation_Choices[,2]==1 & data$KnowledgePoints01ed>=3])/100
  
  Allocation_Choices_Comp_HighKnw[3,3]<-sum(data$Allocation_Universal[Allocation_Choices[,3]==1 & data$KnowledgePoints01ed>=3])/100
  
  Allocation_Choices_Comp_HighKnw[1,4]<-sum(data$Allocation_Climate[Allocation_Choices[,4]==1 & data$KnowledgePoints01ed>=3])/100
  Allocation_Choices_Comp_HighKnw[2,4]<-sum(data$Allocation_LowIncome[Allocation_Choices[,4]==1 & data$KnowledgePoints01ed>=3])/100
  
  Allocation_Choices_Comp_HighKnw[1,5]<-sum(data$Allocation_Climate[Allocation_Choices[,5]==1 & data$KnowledgePoints01ed>=3])/100
  Allocation_Choices_Comp_HighKnw[3,5]<-sum(data$Allocation_Universal[Allocation_Choices[,5]==1 & data$KnowledgePoints01ed>=3])/100
  
  Allocation_Choices_Comp_HighKnw[2,6]<-sum(data$Allocation_LowIncome[Allocation_Choices[,6]==1 & data$KnowledgePoints01ed>=3])/100
  Allocation_Choices_Comp_HighKnw[3,6]<-sum(data$Allocation_Universal[Allocation_Choices[,6]==1 & data$KnowledgePoints01ed>=3])/100
  
  Allocation_Choices_Comp_HighKnw[1,7]<-sum(data$Allocation_Climate[Allocation_Choices[,7]==1 & data$KnowledgePoints01ed>=3])/100
  Allocation_Choices_Comp_HighKnw[2,7]<-sum(data$Allocation_LowIncome[Allocation_Choices[,7]==1 & data$KnowledgePoints01ed>=3])/100
  Allocation_Choices_Comp_HighKnw[3,7]<-sum(data$Allocation_Universal[Allocation_Choices[,7]==1 & data$KnowledgePoints01ed>=3])/100
  #sum(Allocation_Choices[,7]==1)
  barplot(Allocation_Choices_Comp_HighKnw,main="... for respondents with high knowledge",
          col=c("green","red","blue"),
          legend = c("Climate","PoorHH","AllHH"),cex.names=1,las=2,
          args.legend = list(x="topleft",bty="n",ncol=1,cex=1))
  
DataCorrplot_KPtsBi<-data[,c(83:88)]
par(mfrow = c(1, 1) ,mar=c(1,1,1,2))
M_KPtsBi <- cor(DataCorrplot_KPtsBi)
corrplot(M_KPtsBi, method = "circle",tl.cex=.5,tl.col = "black",col=colorRampPalette(c("blue","white","red"))(200))

data$InfoSpanishAcceptabilityDummy<-as.numeric(matrix(0,length(data$KnowledgeGap),1))
data$InfoSpanishAcceptabilityDummy[data$InfoSpanishAcceptability==67]<-1
data$Minority<-as.numeric(matrix(0,length(data$KnowledgeGap),1))
data$Minority[data$InfoSpanishAcceptability==67 & data$CT.Acceptability_Rev2<3]<-1
data$Minority[data$InfoSpanishAcceptability==19 & data$CT.Acceptability_Rev2>3]<-1
summary(data$Minority)
data_reg<-data
colnames(data_reg)[c(17,18,20,21,23,89,9,16)]<-c("CT.Effectiveness_Unspecified",
              "CT.Fairness_Unspecified","CT.PersonalCost_Unspecified",
              "CT.LowIncomeEffect_Unspecified","CT.Acceptability_Unspecified",
              "ObjectiveCTKnowledge","PerceivedCTKnowledge","InfoProvided")
colnames(data_reg)[c(25:49)]<-c("CT.Effectiveness_LITransfers","CT.Effectiveness_ClimProjects",
          "CT.Effectiveness_UnivTrans","CT.Effectiveness_LITransetClimate","CT.Effectiveness_UnivetClimate","CT.Fairness_LITransfers",       
          "CT.Fairness_ClimProjects","CT.Fairness_UnivTrans","CT.Fairness_LITransetClimate",      
          "CT.Fairness_UnivetClimate","CT.PersonalCost_LITransfers","CT.PersonalCost_ClimProjects",   
          "CT.PersonalCost_UnivTrans","CT.PersonalCost_LITransetClimate","CT.PersonalCost_UnivetClimate",   
          "CT.LowIncomeEffect_LITransfers","CT.LowIncomeEffect_ClimProjects","CT.LowIncomeEffect_UnivTrans",
          "CT.LowIncomeEffect_LITransetClimate","CT.LowIncomeEffect_UnivetClimate","CT.Acceptability_LITransfers",  
          "CT.Acceptability_ClimProjects","CT.Acceptability_UnivTrans","CT.Acceptability_LITransetClimate",  
          "CT.Acceptability_UnivetClimate")  


#for Stefano
data_reg_sc<-data_reg
data_reg_sc$InfoProvided<-data_reg_sc$InfoProvided-1
summary(data_reg_sc)

data_reg_sc$InterceptObjKInformation<-data_reg_sc$InfoProvided*(data_reg_sc$ObjectiveCTKnowledge+1)
summary(data_reg_sc$InterceptObjKInformation)

data_reg_sc$CT.Acceptability_Unspecified<-as.factor(data_reg_sc$CT.Acceptability_Unspecified)
m_accept <- polr(CT.Acceptability_Unspecified ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
                 #+KnowledgeGap
                 # + InfoProvided 
                 #+ ObjectiveCTKnowledge
                 + InterceptObjKInformation
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_reg_sc, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")



data_reg$PoliticalView[data_reg$PoliticalView==11]<-NA
data_reg$MonthlyIncome[data_reg$MonthlyIncome==7]<-NA
data_reg<-data_reg[,c(-8,-54)]
summary(data_reg)
hist(data_reg$PoliticalView)
hist(data_reg$MonthlyIncome)
#number of missings in at least ne of the two vectors
sum(colSums(rbind(as.numeric(is.na(data_reg$MonthlyIncome)) + as.numeric(is.na(data_reg$PoliticalView)) ))>0)
#index_missings<-which(colSums(rbind(as.numeric(is.na(data_reg$MonthlyIncome)) + as.numeric(is.na(data_reg$PoliticalView)) ))>0)
#data_reg_withmissing<-data_reg


AcceptabilityLevels<-data_reg[,c(22,44:48)]
write.csv(AcceptabilityLevels, "AcceptabilityLevels.csv")


data_long_reg<-reshape(data_reg, direction = "long", varying = names(data_reg)[c(16:17,19:20,22,24:48)], idvar = "Response.ID", sep="_",timevar="Question")

#transform the variable to be 0/1 with 1 standing for Info provided
data_long_reg$InfoProvided<-data_long_reg$InfoProvided-1
#transform the variable to be 0/1 with 1 standing for Females
data_long_reg$Gender<-data_long_reg$Gender-1


data_long_reg$Unspecified<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$Unspecified[data_long_reg$Question=="Unspecified"]<-1
data_long_reg$LITransfers<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$LITransfers[data_long_reg$Question=="LITransfers"]<-1
data_long_reg$ClimProjects<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$ClimProjects[data_long_reg$Question=="ClimProjects"]<-1
data_long_reg$UnivTrans<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$UnivTrans[data_long_reg$Question=="UnivTrans"]<-1
data_long_reg$LITransetClimate<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$LITransetClimate[data_long_reg$Question=="LITransetClimate"]<-1
data_long_reg$UnivetClimate<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$UnivetClimate[data_long_reg$Question=="UnivetClimate"]<-1



data_long_reg$CT.Acceptability<-as.factor(data_long_reg$CT.Acceptability)

#Following the guide here https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
          #+KnowledgeGap     
          + InfoProvided 
          + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
          + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
          + CT.TrustInPoliticians + HouseholdSize
          , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept)) 
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")


m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap     
                 + InfoProvided 
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse 
                 + ExpectedSpanishAcceptability
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept)) 
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")



m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize, data = data_long_reg[data_long_reg$Unspecified==1,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize, data = data_long_reg[data_long_reg$LITransfers==1,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize, data = data_long_reg[data_long_reg$ClimProjects==1,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize, data = data_long_reg[data_long_reg$UnivTrans==1,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize, data = data_long_reg[data_long_reg$LITransetClimate==1,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize, data = data_long_reg[data_long_reg$UnivetClimate==1,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")


data_long_reg$InterceptInfoLITransfers<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptInfoLITransfers[data_long_reg$LITransfers==1 & data_long_reg$InfoProvided==1]<-1
data_long_reg$InterceptInfoClimProjects<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptInfoClimProjects[data_long_reg$ClimProjects==1 & data_long_reg$InfoProvided==1]<-1
data_long_reg$InterceptInfoUnivTrans<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptInfoUnivTrans[data_long_reg$UnivTrans==1 & data_long_reg$InfoProvided==1]<-1
data_long_reg$InterceptInfoLITransetClimate<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptInfoLITransetClimate[data_long_reg$LITransetClimate==1 & data_long_reg$InfoProvided==1]<-1
data_long_reg$InterceptInfoUnivetClimate<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptInfoUnivetClimate[data_long_reg$UnivetClimate==1 & data_long_reg$InfoProvided==1]<-1
data_long_reg$InterceptInfoUnspecified<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptInfoUnspecified[data_long_reg$Unspecified==1 & data_long_reg$InfoProvided==1]<-1



data_long_reg$InterceptObjKLITransfers<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptObjKLITransfers[data_long_reg$LITransfers==1]<-1
data_long_reg$InterceptObjKLITransfers<-data_long_reg$LITransfers*(data_long_reg$ObjectiveCTKnowledge+1)
data_long_reg$InterceptObjKClimProjects<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptObjKClimProjects[data_long_reg$ClimProjects==1]<-1
data_long_reg$InterceptObjKClimProjects<-data_long_reg$ClimProjects*(data_long_reg$ObjectiveCTKnowledge+1)
data_long_reg$InterceptObjKUnivTrans<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptObjKUnivTrans[data_long_reg$UnivTrans==1]<-1
data_long_reg$InterceptObjKUnivTrans<-data_long_reg$UnivTrans*(data_long_reg$ObjectiveCTKnowledge+1)
data_long_reg$InterceptObjKLITransetClimate<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptObjKLITransetClimate[data_long_reg$LITransetClimate==1]<-1
data_long_reg$InterceptObjKLITransetClimate<-data_long_reg$LITransetClimate*(data_long_reg$ObjectiveCTKnowledge+1)
data_long_reg$InterceptObjKUnivetClimate<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptObjKUnivetClimate[data_long_reg$UnivetClimate==1]<-1
data_long_reg$InterceptObjKUnivetClimate<-data_long_reg$UnivetClimate*(data_long_reg$ObjectiveCTKnowledge+1)
data_long_reg$InterceptObjKUnspecified<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$InterceptObjKUnspecified[data_long_reg$Unspecified==1]<-1
data_long_reg$InterceptObjKUnspecified<-data_long_reg$Unspecified*(data_long_reg$ObjectiveCTKnowledge+1)

summary(data_long_reg)

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
                 #+KnowledgeGap
                 #+ InfoProvided #+ ObjectiveCTKnowledge

                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 
                  + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
                  + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
                 
                  + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
                  + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate

                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
                 #+KnowledgeGap
                 #+ InfoProvided + ObjectiveCTKnowledge
                 
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 
                 + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
                 + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
                 
                 + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
                 + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
                 
                 + Age + Gender + Education + ClimateConcern + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")

### for EFFECTIVENESS

data_long_reg$CT.Effectiveness<-as.factor(data_long_reg$CT.Effectiveness)

#Following the guide here https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
m_effect <- polr(CT.Effectiveness ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap     
                 + InfoProvided 
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_effect)
(ctable <- coef(summary(m_effect)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_effect))
# exp(coef(m_effect))
# ## OR and CI
# exp(cbind(OR = coef(m_effect), ci))
PseudoR2(m_effect,which="Nagelkerke")


m_effect <- polr(CT.Effectiveness ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
                 #+KnowledgeGap
                 #+ InfoProvided
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 
                 + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
                 + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
                 
                 + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
                 + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
                 
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_effect)
(ctable <- coef(summary(m_effect)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_effect))
exp(coef(m_effect))
## OR and CI
exp(cbind(OR = coef(m_effect), ci))
PseudoR2(m_effect,which="Nagelkerke")


m_effect <- polr(CT.Effectiveness ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
                 #+KnowledgeGap
                 #+ InfoProvided
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 
                 + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
                 + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
                 
                 + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
                 + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
                 
                 + Age + Gender + Education + ClimateConcern  + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_effect)
(ctable <- coef(summary(m_effect)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_effect))
exp(coef(m_effect))
## OR and CI
exp(cbind(OR = coef(m_effect), ci))
PseudoR2(m_effect,which="Nagelkerke")

### for FAIRNESS

data_long_reg$CT.Fairness<-as.factor(data_long_reg$CT.Fairness)

#Following the guide here https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
m_fair <- polr(CT.Fairness ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 #+KnowledgeGap     
                 + InfoProvided 
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_fair)
(ctable <- coef(summary(m_fair)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_fair)) 
# exp(coef(m_fair))
# ## OR and CI
# exp(cbind(OR = coef(m_fair), ci))
PseudoR2(m_fair,which="Nagelkerke")


m_fair <- polr(CT.Fairness ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
                 #+KnowledgeGap
                 #+ InfoProvided
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
               
               + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
               + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
               
               + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
               + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
               
               + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_fair)
(ctable <- coef(summary(m_fair)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_fair))
exp(coef(m_fair))
## OR and CI
exp(cbind(OR = coef(m_fair), ci))
PseudoR2(m_fair,which="Nagelkerke")


m_fair <- polr(CT.Fairness ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
               #+KnowledgeGap
               #+ InfoProvided
               + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
               
               + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
               + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
               
               + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
               + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
               
               + Age + Gender + Education + ClimateConcern  + CarUse
               + CT.TrustInPoliticians + HouseholdSize
               , data = data_long_reg, Hess=TRUE)
summary(m_fair)
(ctable <- coef(summary(m_fair)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_fair))
exp(coef(m_fair))
## OR and CI
exp(cbind(OR = coef(m_fair), ci))
PseudoR2(m_fair,which="Nagelkerke")
### for LOWINCOMEEFFECT

data_long_reg$CT.LowIncomeEffect<-as.factor(data_long_reg$CT.LowIncomeEffect)

#Following the guide here https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
m_lie <- polr(CT.LowIncomeEffect ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
               #+KnowledgeGap     
               + InfoProvided 
               + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
               + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                + CT.TrustInPoliticians + HouseholdSize
                , data = data_long_reg, Hess=TRUE)
summary(m_lie)
(ctable <- coef(summary(m_lie)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_lie))
# exp(coef(m_lie))
# ## OR and CI
# exp(cbind(OR = coef(m_lie), ci))
PseudoR2(m_lie,which="Nagelkerke")


m_lie <- polr(CT.LowIncomeEffect ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
               #+KnowledgeGap
               #+ InfoProvided
               + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
              
              + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
              + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
              
              + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
              + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
              
              + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
               + CT.TrustInPoliticians + HouseholdSize
               , data = data_long_reg, Hess=TRUE)
summary(m_lie )
(ctable <- coef(summary(m_lie )))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_lie))
exp(coef(m_lie))
## OR and CI
exp(cbind(OR = coef(m_lie), ci))
PseudoR2(m_lie,which="Nagelkerke")


m_lie <- polr(CT.LowIncomeEffect ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
              #+KnowledgeGap
              #+ InfoProvided
              + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
              
              + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
              + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
              
              + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
              + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
              
              + Age + Gender + Education + ClimateConcern  + CarUse
              + CT.TrustInPoliticians + HouseholdSize
              , data = data_long_reg, Hess=TRUE)
summary(m_lie )
(ctable <- coef(summary(m_lie )))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_lie))
exp(coef(m_lie))
## OR and CI
exp(cbind(OR = coef(m_lie), ci))
PseudoR2(m_lie,which="Nagelkerke")

### for PERSONAL COST

data_long_reg$CT.PersonalCost<-as.factor(data_long_reg$CT.PersonalCost)

#Following the guide here https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
m_pc <- polr(CT.PersonalCost ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
              #+KnowledgeGap     
              + InfoProvided 
              + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
              + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
              + CT.TrustInPoliticians + HouseholdSize
              , data = data_long_reg, Hess=TRUE)
summary(m_pc)
(ctable <- coef(summary(m_pc)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_pc)) 
# exp(coef(m_pc))
# ## OR and CI
# exp(cbind(OR = coef(m_pc), ci))
PseudoR2(m_pc,which="Nagelkerke")


m_pc <- polr(CT.PersonalCost ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
              #+KnowledgeGap
              #+ InfoProvided
              + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
             
             + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
             + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
             
             + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
             + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
             
             + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
              + CT.TrustInPoliticians + HouseholdSize
              , data = data_long_reg, Hess=TRUE)
summary(m_pc )
(ctable <- coef(summary(m_pc )))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_pc))
exp(coef(m_pc))
## OR and CI
exp(cbind(OR = coef(m_pc), ci))
PseudoR2(m_pc,which="Nagelkerke")



m_pc <- polr(CT.PersonalCost ~  PerceivedCTKnowledge #+ ObjectiveCTKnowledge
             #+KnowledgeGap
             #+ InfoProvided
             + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
             
             + InterceptObjKUnspecified+ InterceptObjKLITransfers + InterceptObjKClimProjects
             + InterceptObjKUnivTrans + InterceptObjKLITransetClimate + InterceptObjKUnivetClimate
             
             + InterceptInfoUnspecified + InterceptInfoLITransfers + InterceptInfoClimProjects
             + InterceptInfoUnivTrans   + InterceptInfoLITransetClimate + InterceptInfoUnivetClimate
             
             + Age + Gender + Education + ClimateConcern + CarUse
             + CT.TrustInPoliticians + HouseholdSize
             , data = data_long_reg, Hess=TRUE)
summary(m_pc )
(ctable <- coef(summary(m_pc )))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_pc))
exp(coef(m_pc))
## OR and CI
exp(cbind(OR = coef(m_pc), ci))
PseudoR2(m_pc,which="Nagelkerke")

###MEDIATION EFFECT FOR FAIRNESS??
data_long_reg$CT.PersonalCost<-as.numeric(data_long_reg$CT.PersonalCost)
data_long_reg$CT.LowIncomeEffect<-as.numeric(data_long_reg$CT.LowIncomeEffect)
data_long_reg$CT.Fairness<-as.factor(data_long_reg$CT.Fairness)

# m_fair <- polr(CT.Fairness ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
#                #+KnowledgeGap  
#                + CT.PersonalCost + CT.LowIncomeEffect
#                + InfoProvided 
#                + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
#                + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
#                + CT.TrustInPoliticians + HouseholdSize
#                , data = data_long_reg, Hess=TRUE)
# summary(m_fair)
# (ctable <- coef(summary(m_fair)))
# ## calculate and store p values
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ## combined table
# (ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_fair)) 
# exp(coef(m_fair))
# ## OR and CI
# exp(cbind(OR = coef(m_fair), ci))
# PseudoR2(m_fair,which="Nagelkerke")


m_fair <- polr(CT.Fairness ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
               #+KnowledgeGap  
               + CT.PersonalCost + CT.LowIncomeEffect
               + InfoProvided 
               #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
               + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
               + CT.TrustInPoliticians + HouseholdSize
               , data = data_long_reg, Hess=TRUE)
summary(m_fair)
(ctable <- coef(summary(m_fair)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_fair)) 
exp(coef(m_fair))
## OR and CI
exp(cbind(OR = coef(m_fair), ci))
PseudoR2(m_fair,which="Nagelkerke")

m_fair <- polr(CT.Fairness ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
               #+KnowledgeGap  
               + CT.PersonalCost + CT.LowIncomeEffect
               + InfoProvided 
               #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
               + Age + Gender + Education + ClimateConcern  + CarUse
               + CT.TrustInPoliticians + HouseholdSize
               , data = data_long_reg, Hess=TRUE)
summary(m_fair)
(ctable <- coef(summary(m_fair)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_fair)) 
exp(coef(m_fair))
## OR and CI
exp(cbind(OR = coef(m_fair), ci))
PseudoR2(m_fair,which="Nagelkerke")

data_long_reg$CT.Effectiveness<-as.numeric(data_long_reg$CT.Effectiveness)

# m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
#                  + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
#                  + InfoProvided
#                  + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
#                  + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
#                  + CT.TrustInPoliticians + HouseholdSize
#                  , data = data_long_reg, Hess=TRUE)
# summary(m_accept)
# (ctable <- coef(summary(m_accept)))
# ## calculate and store p values
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ## combined table
# (ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_accept))
# exp(coef(m_accept))
# ## OR and CI
# exp(cbind(OR = coef(m_accept), ci))
# PseudoR2(m_accept,which="Nagelkerke")


m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")

data_long_reg$CT.Fairness<-as.numeric(data_long_reg$CT.Fairness)
# 
# m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
#                  + CT.Effectiveness 
#                  + CT.Fairness
#                  + InfoProvided
#                  + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
#                  + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
#                  + CT.TrustInPoliticians + HouseholdSize
#                  , data = data_long_reg, Hess=TRUE)
# summary(m_accept)
# (ctable <- coef(summary(m_accept)))
# ## calculate and store p values
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ## combined table
# (ctable <- cbind(ctable, "p value" = p))
# (ci <- confint(m_accept))
# exp(coef(m_accept))
# ## OR and CI
# exp(cbind(OR = coef(m_accept), ci))
# PseudoR2(m_accept,which="Nagelkerke")

m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness 
                 + CT.Fairness
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")


m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness 
                 + CT.Fairness
                 + InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern  + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")

data$EffectivenessOrFairness[data$EffectivenessOrFairness==6]<-NA
summary(data$EffectivenessOrFairness)

DataCorrplot<-data_long_reg[,64:68]
DataCorrplot$CT.Acceptability<-as.numeric(DataCorrplot$CT.Acceptability)
DataCorrplot$CT.Fairness<-as.numeric(DataCorrplot$CT.Fairness)

Mmatrix<-matrix(0,5,5)
for(i in 1:5){
  for (j in 1:5){
    if (i>j){cc<-cor.test(DataCorrplot[,i],DataCorrplot[,j],method = "pearson")
    Mmatrix[i,j]<-cc$estimate
    } else if(j>i){cc<-cor.test(DataCorrplot[,i],DataCorrplot[,j],method = "spearman",exact=FALSE)
    Mmatrix[i,j]<-cc$estimate
    } else{ Mmatrix[i,j]<-1}
  }
}
colnames(Mmatrix)<-c("Effectiveness", "Fairness","Personal effects", "Low-income effects", "Acceptability")
rownames(Mmatrix)<-c("Effectiveness", "Fairness","Personal effects", "Low-income effects", "Acceptability")
corrplot(Mmatrix, method = "number")
corrplot(Mmatrix, method = "color",col=colorRampPalette(c("blue","white","red"))(200),
         order = "hclust", number.cex = .7,cl.lim=c(0,1),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90) # Text label color and rotation)


#following http://uc-r.github.io/gbm_regression
library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization


data_gbm<-data_long_reg[,c(64:68, 57, 3,4,8,15,28:32,34,17)]
colnames(data_gbm)<-c("Effectiveness","Fairness","PersonalEffects","LowIncomeEffects",
                      "Acceptability","PriorKnowledge","Gender","Age","PerceivedKnowledge",
                      "InformationProvided","HouseholdSize","MonthlyIncome","Education","ClimateConcern",
                      "PoliticalView","CarUse","TrustInPoliticians")


# for reproducibility
set.seed(123)

# train GBM model
gbm.fit.final <- gbm(
  formula = Acceptability ~ .,
  distribution = "multinomial",
  data = data_gbm,
  n.trees = 1000,
  interaction.depth = 1,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .75, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

par(mar = c(5, 12, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 16,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)



hist(data$Experiment.Group, main="Distribution of informed individuals", xaxt="n", xlab="")
axis(1, at=c(1,2), labels=c("Control","Experiment")) 
#sum(data$Experiment.Group==1) #number of controls
#sum(data$Experiment.Group==0) #number of experiments

data$ExperimentID[data$Experiment.Group==1]<- "Control"
data$ExperimentID[data$Experiment.Group==2]<- "Experiment"

# data$CT.Efficiency_word[data$CT.Efficiency==1]<-"Very ineffective"
# data$CT.Efficiency_word[data$CT.Efficiency==2]<-"Ineffective"
# data$CT.Efficiency_word[data$CT.Efficiency==3]<-"Neither nor"
# data$CT.Efficiency_word[data$CT.Efficiency==4]<-"Effective"
# data$CT.Efficiency_word[data$CT.Efficiency==5]<-"Very effective"


mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.Effectiveness))
p<-ggplot(data, aes(x=CT.Effectiveness, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5)        #dodge to have them side by side
# Add mean lines
p+
#scale_x_discrete(breaks=c(1,2,3,4,5), labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("How effective do you think a carbon tax is for reducing CO2 emissions? Very ineffective-Very effective") +  xlab("")



### FOR THEO

m_accept <- polr(CT.Acceptability ~  #PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 #+ InfoProvided
                 #+ LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 #+ Age + Gender + Education + ClimateConcern +PoliticalView + MonthlyIncome + CarUse
                 #+ CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m_accept))
exp(coef(m_accept))
## OR and CI
exp(cbind(OR = coef(m_accept), ci))
PseudoR2(m_accept,which="Nagelkerke")

data_long_reg$CT.Acceptability<-as.numeric(data_long_reg$CT.Acceptability)
lmodel <- lm(CT.Acceptability  ~ CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect - 1, data = data_long_reg)
summary(lmodel)

data_long_reg$CT.Acceptability_rescaled<-matrix(0,length(data_long_reg$CT.Acceptability),1)
data_long_reg$CT.Acceptability_rescaled[data_long_reg$CT.Acceptability==1]<-0
data_long_reg$CT.Acceptability_rescaled[data_long_reg$CT.Acceptability==2]<-0.25
data_long_reg$CT.Acceptability_rescaled[data_long_reg$CT.Acceptability==3]<-0.5
data_long_reg$CT.Acceptability_rescaled[data_long_reg$CT.Acceptability==4]<-0.75
data_long_reg$CT.Acceptability_rescaled[data_long_reg$CT.Acceptability==5]<-1

data_long_reg$CT.Effectiveness_rescaled<-matrix(0,length(data_long_reg$CT.Effectiveness),1)
data_long_reg$CT.Effectiveness_rescaled[data_long_reg$CT.Effectiveness==1]<-0
data_long_reg$CT.Effectiveness_rescaled[data_long_reg$CT.Effectiveness==2]<-0.25
data_long_reg$CT.Effectiveness_rescaled[data_long_reg$CT.Effectiveness==3]<-0.5
data_long_reg$CT.Effectiveness_rescaled[data_long_reg$CT.Effectiveness==4]<-0.75
data_long_reg$CT.Effectiveness_rescaled[data_long_reg$CT.Effectiveness==5]<-1

data_long_reg$CT.PersonalCost_rescaled<-matrix(0,length(data_long_reg$CT.PersonalCost),1)
data_long_reg$CT.PersonalCost_rescaled[data_long_reg$CT.PersonalCost==1]<-0
data_long_reg$CT.PersonalCost_rescaled[data_long_reg$CT.PersonalCost==2]<-0.25
data_long_reg$CT.PersonalCost_rescaled[data_long_reg$CT.PersonalCost==3]<-0.5
data_long_reg$CT.PersonalCost_rescaled[data_long_reg$CT.PersonalCost==4]<-0.75
data_long_reg$CT.PersonalCost_rescaled[data_long_reg$CT.PersonalCost==5]<-1

data_long_reg$CT.LowIncomeEffect_rescaled<-matrix(0,length(data_long_reg$CT.LowIncomeEffect),1)
data_long_reg$CT.LowIncomeEffect_rescaled[data_long_reg$CT.LowIncomeEffect==1]<-0
data_long_reg$CT.LowIncomeEffect_rescaled[data_long_reg$CT.LowIncomeEffect==2]<-0.25
data_long_reg$CT.LowIncomeEffect_rescaled[data_long_reg$CT.LowIncomeEffect==3]<-0.5
data_long_reg$CT.LowIncomeEffect_rescaled[data_long_reg$CT.LowIncomeEffect==4]<-0.75
data_long_reg$CT.LowIncomeEffect_rescaled[data_long_reg$CT.LowIncomeEffect==5]<-1

lmodel <- lm(CT.Acceptability_rescaled  ~ CT.Effectiveness_rescaled 
             + CT.PersonalCost_rescaled + CT.LowIncomeEffect_rescaled, data = data_long_reg)
summary(lmodel)

### lets see if splitting the sample on political affiliation plays a role
summary(data_reg$PoliticalView)
par(mfrow = c(1,1) ,mar=c(6,4,1,2))
hist(data_reg$PoliticalView, main="Political view",  breaks=c(0,1,2,3,4,5,6,7,8,9,10), xaxt="n", xlab="")
axis(1, at=c(.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5), labels=c("1 left-wing","2","3","4","5","6","7","8","9","10 right-wing"),las=2)

table(data_long_reg$PoliticalView)
data_long_reg$CT.Acceptability<-as.factor(data_long_reg$CT.Acceptability)

#with controls
m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 + InfoProvided
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern 
                 + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg[data_long_reg$PoliticalView<=5,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")


m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 + InfoProvided
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern 
                 + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg[data_long_reg$PoliticalView>=6,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

#without controls
m_accept <- polr(CT.Acceptability ~ CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 , data = data_long_reg[data_long_reg$PoliticalView<=5,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")


m_accept <- polr(CT.Acceptability ~ CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 , data = data_long_reg[data_long_reg$PoliticalView>=6,], Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")

lmodel <- lm(CT.Acceptability_rescaled  ~ CT.Effectiveness_rescaled 
             + CT.PersonalCost_rescaled + CT.LowIncomeEffect_rescaled, data = data_long_reg[data_long_reg$PoliticalView<=5,])
summary(lmodel)

lmodel <- lm(CT.Acceptability_rescaled  ~ CT.Effectiveness_rescaled 
             + CT.PersonalCost_rescaled + CT.LowIncomeEffect_rescaled, data = data_long_reg[data_long_reg$PoliticalView>=6,])
summary(lmodel)

data_long_reg$CT.Effectiveness_PolView<-data_long_reg$CT.Effectiveness*data_long_reg$PoliticalView
data_long_reg$CT.PersonalCost_PolView<-data_long_reg$CT.PersonalCost*data_long_reg$PoliticalView
data_long_reg$CT.LowIncomeEffect_PolView<-data_long_reg$CT.LowIncomeEffect*data_long_reg$PoliticalView
summary(data_long_reg$CT.Effectiveness_PolView)

data_long_reg$CT.Effectiveness_PolView_rescaled<-data_long_reg$CT.Effectiveness_rescaled*data_long_reg$PoliticalView
data_long_reg$CT.PersonalCost_PolView_rescaled<-data_long_reg$CT.PersonalCost_rescaled*data_long_reg$PoliticalView
data_long_reg$CT.LowIncomeEffect_PolView_rescaled<-data_long_reg$CT.LowIncomeEffect_rescaled*data_long_reg$PoliticalView
summary(data_long_reg$CT.Effectiveness_PolView_rescaled)

lmodel <- lm(CT.Acceptability_rescaled  ~ CT.Effectiveness_rescaled 
             + CT.PersonalCost_rescaled + CT.LowIncomeEffect_rescaled + 
               CT.Effectiveness_PolView_rescaled + CT.PersonalCost_PolView_rescaled + CT.LowIncomeEffect_PolView_rescaled
             , data = data_long_reg)
summary(lmodel)

lmodel <- lm(CT.Acceptability_rescaled  ~ CT.Effectiveness_rescaled 
             + CT.PersonalCost_rescaled + CT.LowIncomeEffect_rescaled + 
               CT.Effectiveness_PolView_rescaled + CT.PersonalCost_PolView_rescaled + CT.LowIncomeEffect_PolView_rescaled - 1
             , data = data_long_reg)
summary(lmodel)

#with controls
m_accept <- polr(CT.Acceptability ~  PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 + CT.Effectiveness_PolView + CT.PersonalCost_PolView + CT.LowIncomeEffect_PolView
                 + InfoProvided
                 + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 + Age + Gender + Education + ClimateConcern 
                 + MonthlyIncome + CarUse
                 + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")



#without controls
m_accept <- polr(CT.Acceptability ~  #PerceivedCTKnowledge + ObjectiveCTKnowledge
                 + CT.Effectiveness + CT.PersonalCost + CT.LowIncomeEffect
                 + CT.Effectiveness_PolView + CT.PersonalCost_PolView + CT.LowIncomeEffect_PolView
                 # + InfoProvided
                 # + LITransfers + ClimProjects + UnivTrans + LITransetClimate + UnivetClimate
                 # + Age + Gender + Education + ClimateConcern 
                 # + MonthlyIncome + CarUse
                 # + CT.TrustInPoliticians + HouseholdSize
                 , data = data_long_reg, Hess=TRUE)
summary(m_accept)
(ctable <- coef(summary(m_accept)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
PseudoR2(m_accept,which="Nagelkerke")








mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.Fairness))
p<-ggplot(data, aes(x=CT.Fairness, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("How fair or unfair do you consider a carbon tax? Very unfair - very fair") +  xlab("")



mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.Object))
p<-ggplot(data, aes(x=CT.Object, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("Who do you think should carry most of the burden of the carbon tax? Business - consumers - both - none") +  xlab("")






mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.PersonalCost))
p<-ggplot(data, aes(x=CT.PersonalCost, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("How do you think a carbon tax will affect you personally? Much worse off - much better off") +  xlab("")




mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.LowIncomeEffect))
p<-ggplot(data, aes(x=CT.LowIncomeEffect, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("How do you think a carbon tax will affect low-income households? Much worse off - much better off") +  xlab("")



mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.TrustInPoliticians))
p<-ggplot(data, aes(x=CT.TrustInPoliticians, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("How trustworthy do you think politicians are in implementing the carbon tax properly? Very untrustworthy - very trustworthy") +  xlab("")




mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.Acceptability))
p<-ggplot(data, aes(x=CT.Acceptability, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("How acceptable do you find a carbon tax? Completely not - completely accaptable") +  xlab("")



mu <- ddply(data, "ExperimentID", summarise, grp.mean=mean(CT.Objective))
p<-ggplot(data, aes(x=CT.Objective, fill=ExperimentID, color=ExperimentID)) +
geom_histogram(position="identity", alpha=0.5) 
# Add mean lines
p+ 
# scale_x_discrete(breaks=c("1","2","3","4","5"),
# labels=c("Very ineffective", "Ineffective", "Neither nor","Effective","Very effective"))+ 
# theme(axis.text.x = element_text(angle = 45, hjust = 1))+
geom_vline(data=mu, aes(xintercept=grp.mean, color=ExperimentID),
linetype="dashed")+
ggtitle("Which of these two objectives do you think is the main purpose of a carbon tax? Generate revenues - change behaviour - I don't know") +  xlab("")

