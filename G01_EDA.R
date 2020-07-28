rm(list=ls())
setwd("C:/Users/Vinayak/Desktop/Business Data Analytics/Project")
library(dplyr)
library(ggplot2)
library(reshape2)
library(Information)
library(woe)
library(fuzzyjoin)
library(scorecard)

#Accessing the two datasets
credit <- read.csv("Credit_Bureau.csv",stringsAsFactors = TRUE)
dem <- read.csv("demogs.csv",stringsAsFactors = TRUE)

summary(dem)

summary(credit)
attach(dem)
attach(credit)

levels(dem$Gender)[levels(dem$Gender)==""]<-"NA"
levels(dem$Marital.Status..at.the.time.of.application.)[levels(dem$Marital.Status..at.the.time.of.application.)==""]<-"NA"
levels(dem$Education)[levels(dem$Education)==""]<-"NA"
levels(dem$Profession)[levels(dem$Profession)==""]<-"NA"
levels(dem$Type.of.residence)[levels(dem$Type.of.residence)==""]<-"NA"

#checking for duplicates
dem %>%
  group_by(Application.ID)%>%
  filter(n()>1)

credit %>%
  group_by(Application.ID)%>%
  filter(n()>1)

#selecting only unique ID's
dem <- dem %>%
  group_by(Application.ID)%>%
  filter(n()==1)

credit <-  credit %>%
  group_by(Application.ID)%>%
  filter(n()==1)

#Mergimg the datasets
merged_data <- merge(dem,credit,by=c("Application.ID"))
#removing performance variable obtained from dem. Performance.Tag.y is from credit. Same result
merged_data <- merged_data[,-12]

#checking if categorical independent variables are factors
#also checking if dependent categorical variable is integer (not factor)
#both of these are done to use the woe and IV functions effectively
#dem_class contains the type of data of each coloumns
merged_data_class <- data.frame(colnames(merged_data))
colnames(merged_data_class) <- "Variable"
for (i in 1:ncol(dem)) {
  merged_data_class$Class[i] <- class(merged_data[,i])
}
merged_data_class 

#checking for NA in Performance.Tag i.e dependent categorical variable
#missing values in dependent variable cannot be practically solved
#thus we resort to removing them from the dataset
merged_data$Performance.Tag.y %>%
  is.na()%>%
  sum()

merged_data<- merged_data %>%
  filter(!is.na(Performance.Tag.y))



## DATA CLEANING AND PREPARATION ##

#woe data
woe_data<-woebin_ply(merged_data[,-1],bins)
#changing dependent coloumn
woe_data$Performance.Tag <- woe_data$Performance.Tag.y
#merging application id data
colnames(woe_data)[1]<-"Application.ID"
woe_data$Application.ID <- merged_data$Application.ID
#writing file 
write.csv(woe_data,file="woe_data.csv")
#summary
summary(woe_data)

#obtaining all the IV values from bins
IV_temp <- round(c(bins$Age$total_iv[1],bins$Gender$total_iv[1],
                   bins$Marital.Status..at.the.time.of.application.$total_iv[1],
                   bins$No.of.dependents$total_iv[1],bins$Income$total_iv[1],bins$Education$total_iv[1],
                   bins$Profession$total_iv[1],bins$Type.of.residence$total_iv[1],bins$No.of.months.in.current.residence$total_iv[1],
                   bins$No.of.months.in.current.company$total_iv[1],bins$No.of.times.90.DPD.or.worse.in.last.6.months$total_iv[1],bins$No.of.times.60.DPD.or.worse.in.last.6.months$total_iv[1],
                   bins$No.of.times.30.DPD.or.worse.in.last.6.months$total_iv[1],bins$No.of.times.90.DPD.or.worse.in.last.12.months$total_iv[1],bins$No.of.times.60.DPD.or.worse.in.last.12.months$total_iv[1],
                   bins$No.of.times.30.DPD.or.worse.in.last.12.months$total_iv[1],bins$Avgas.CC.Utilization.in.last.12.months$total_iv[1],bins$No.of.trades.opened.in.last.6.months$total_iv[1],
                   bins$No.of.trades.opened.in.last.12.months$total_iv[1],bins$No.of.PL.trades.opened.in.last.6.months$total_iv[1],bins$No.of.PL.trades.opened.in.last.12.months$total_iv[1],
                   bins$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$total_iv[1],bins$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$total_iv[1],bins$Presence.of.open.home.loan$total_iv[1],
                   bins$Outstanding.Balance$total_iv[1],bins$Total.No.of.Trades$total_iv[1],bins$Presence.of.open.auto.loan$total_iv[1]),2)

#creating a table of IV values
IV<- data.frame("Variable"=colnames(merged_data)[-c(1,ncol(merged_data))],
                "IV"=IV_temp)
IV$Variable<-as.character(IV$Variable)
IV_desc <- IV[order(-IV$IV),]
IV_cut <- subset(IV_desc,IV_desc$IV>0.1)
rownames(IV_cut)<-1:nrow(IV_cut)
print(IV_desc)

print(IV_cut)

#WOE PLOTS
par(mfrow=c(2,2))
woebin_plot(bins[IV_cut$Variable],show_iv = F,line_value = c("woe"))


#BAD PROB PLOTS 
ggplot(bins$Avgas.CC.Utilization.in.last.12.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+ labs(title = "Bad probability for Avgas.CC.Utilization.in.last.12.months")

ggplot(bins$No.of.PL.trades.opened.in.last.12.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.PL.trades.opened.in.last.12.months")


ggplot(bins$No.of.trades.opened.in.last.12.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.trades.opened.in.last.12.months")


ggplot(bins$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")


ggplot(bins$No.of.times.30.DPD.or.worse.in.last.6.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.times.30.DPD.or.worse.in.last.6.months")


ggplot(bins$Outstanding.Balance,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for Outstanding.Balance")


ggplot(bins$Total.No.of.Trades,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for Total.No.of.Trades")


ggplot(bins$No.of.times.30.DPD.or.worse.in.last.12.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.times.30.DPD.or.worse.in.last.12.months")


ggplot(bins$No.of.PL.trades.opened.in.last.6.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.PL.trades.opened.in.last.6.months")


ggplot(bins$No.of.times.60.DPD.or.worse.in.last.6.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.times.60.DPD.or.worse.in.last.6.months")


ggplot(bins$No.of.times.90.DPD.or.worse.in.last.12.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.times.90.DPD.or.worse.in.last.12.months")


ggplot(bins$No.of.times.60.DPD.or.worse.in.last.12.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.times.60.DPD.or.worse.in.last.12.months")


ggplot(bins$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")


ggplot(bins$No.of.trades.opened.in.last.6.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "No.of.trades.opened.in.last.6.months")


ggplot(bins$No.of.times.90.DPD.or.worse.in.last.6.months,aes(x=bin,y=badprob))+
  geom_bar(stat="identity")+labs(title = "Bad probability for No.of.times.90.DPD.or.worse.in.last.6.months")



#computing IV and WOE
bins <- woebin(merged_data[,-1],"Performance.Tag.y")
bins$Gender
