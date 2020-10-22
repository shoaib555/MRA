rm(list = ls())
ca=read.csv("caf.csv")
summary(ca)
str(ca)
library(DataExplorer)
plot_missing(ca)
ca$Category=as.factor(ca$Category)

str(ca$Time)
ca$Time=format(as.POSIXct(ca$Time,format="%H:%M:%S"),format="%H:%S")
str(ca$Time)

ca$Time=as.numeric(sub(":",".",ca$Time))

str(ca$Time)
str(ca$ï..Date)

ca$ï..Date=as.Date(ca$ï..Date,format="%d-%b-%y")
ca$Day=format(ca$ï..Date,"%a")
ca$Month=format(ca$ï..Date,"%b")
#Creating day for further analysis
for(i in 1:nrow(ca)){
  if(ca$Time[i]>=4.00 & ca$Time[i]<=12.00){
    ca$day_bin[i]="Morning"
  }
  else if(ca$Time[i]>=12.01 & ca$Time[i]<=16.00){
    ca$day_bin[i]="Afternoon"
  }
  else if(ca$Time[i]>=16.01 & ca$Time[i]<=19.00){
    ca$day_bin[i]="Evening"
  }
  else if(ca$Time[i]>=19.01 & ca$Time[i]<=23.59){
    ca$day_bin[i]="Night"
  }
  else if (ca$Time[i]>=00.00 & ca$Time[i]<=3.59){
    ca$day_bin[i]="lateN/earlyM"
  }
}

ca$day_bin=as.factor(ca$day_bin)


ca=ca[,-1]
ca$Day=as.factor(ca$Day)

summary(ca)
str(ca)

library(tidyverse)

ca%>%group_by(day_bin)%>%summarize(n=sum(Total))%>%ggplot(aes(x=reorder(day_bin,-n),y=n))+geom_bar(stat='identity')+
  labs(x="Time of Day",y="Total price")+ggtitle("Total sales by day time")+ scale_x_discrete(limit = c("Morning", "Afternoon", "Night","lateN/earlyM"))

ca%>%group_by(Category)%>%summarize(n=sum(Total))%>%ggplot(aes(x=reorder(Category,-n),y=n))+geom_bar(stat='identity')+
  labs(x="Category",y="Total sales")+ggtitle("Total sales by Category")


ca%>%group_by(Day)%>%summarize(n=sum(Total))%>%ggplot(aes(x=reorder(Day,-n),y=n))+geom_bar(stat='identity')+
  labs(x="Day",y="Total sales")+ggtitle("Total sales by Day")+ scale_x_discrete(limit = c("Mon", "Tue", "Wed","Thu","Fri","Sat","Sun"))

ca%>%group_by(Month)%>%summarize(n=sum(Total))%>%ggplot(aes(x=reorder(Month,n),y=n))+geom_bar(stat='identity')+
  labs(x="Month",y="Total sales")+ggtitle("Total sales by Month")+ scale_x_discrete(limit = c("Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"))


cct=aggregate(Tax~Category,data=ca,FUN = mean)

ggplot(cct,aes(x=reorder(Category,-Tax),y=Tax))+geom_bar(stat='identity')+
  labs(x="Category",y="Avg Tax")+ggtitle("Avg Tax By category")




bk=ggplot(ca,aes(x=(Total),y=Quantity,size=Total))
bk+geom_point(aes(color=Category))+guides(size=F)+facet_grid(.~Day)+ggtitle("Sales by Catergory and Day ")

bk=ggplot(ca,aes(x=Total,y=Quantity,size=Total))
bk+geom_point(aes(color=Category))+guides(size=F)+facet_grid(.~Month)+ggtitle("Sales by Category and Month")

bk=ggplot(ca,aes(x=Total,y=Quantity,size=Total))
bk+geom_point(aes(color=Category))+guides(size=F)+facet_grid(.~day_bin)+ggtitle("Sales by Category and Time of the day")

bk=ggplot(ca,aes(x=Discount,y=Quantity,size=Discount))
bk+geom_point(aes(color=Category))+guides(size=F)+facet_grid(.~Day)+ggtitle("Discount by Catergory and Day ")

bk=ggplot(ca,aes(x=Discount,y=Quantity,size=Discount))
bk+geom_point(aes(color=Category))+guides(size=F)+facet_grid(.~Month)+ggtitle("Discount by Category and Month")

bk=ggplot(ca,aes(x=Discount,y=Quantity,size=Discount))
bk+geom_point(aes(color=Category))+guides(size=F)+facet_grid(.~day_bin)+ggtitle("Discount by Category and Time of the day")


table(ca$Category,ca$Day,ca$day_bin)

library(rpivotTable)
rpivotTable(ca)

#Menu Analysis

library(arules) 
library(arulesViz) 

ca$Bill.Number=as.factor(ca$Bill.Number)
str(ca)
ca.agg=split(ca$Item.Desc,ca$Bill.Number)
ca.agg2=list()
for( i in 1:length(ca.agg)){
  ca.agg2[[i]]=unique(ca.agg[[i]])
}
head(ca.agg,10)
head(ca.agg2)
txn=as(ca.agg2,"transactions")
summary(txn)
inspect(txn[1000])
freq=itemFrequency(txn)
freq=freq[order(-freq)]
barplot(freq[1:10])
itemFrequencyPlot(txn,support=0.05)
itemFrequencyPlot(txn,topN=15,main="Top 15 items by order frequency")
arules1=apriori(data=txn,parameter = list(support=0.002,confidence=0.05,maxlen=50))
inspect(sort(arules1,by="lift"))
subrule=head(sort(arules1,by="lift"),20)
print(subrule)
inspect(subrule)
plot(subrule,method = "graph")
rules=as(subrule,"data.frame")

arules2=apriori(data=txn,parameter = list(support=0.001,confidence=0.05,maxlen=50))
inspect(sort(arules2,by="lift"))
subrule1=head(sort(arules2,by="lift"),20)
print(subrule1)
inspect(subrule1)
plot(subrule1,method = "graph")
rules1=as(subrule1,"data.frame")

print(rules1)
rules1$LHSP=rules1$support/rules1$confidence
rules1$RHSP=rules1$confidence/rules1$lift
rules1%>%arrange(desc(lift))->rules2

