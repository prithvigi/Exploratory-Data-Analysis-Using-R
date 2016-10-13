#For the rollingsales_manhattan data set.

require(gdata)
bkmn<- read.xls("rollingsales_manhattan.xls",perl=perl,pattern="BOROUGH")
head(bkmn)
summary(bkmn)
bkmn$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bkmn$SALE.PRICE))
count(is.na(bkmn$SALE.PRICE.N))
names(bkmn) <- tolower(names(bkmn))

## clean/format the data with regular expressions
bkmn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bkmn$gross.square.feet))
bkmn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bkmn$land.square.feet))
bkmn$sale.date <- as.Date(bkmn$sale.date)
bkmn$year.built <- as.numeric(as.character(bkmn$year.built))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bkmn)
hist(sale.price.n)
hist(gross.sqft[sale.price.n==0])
detach(bkmn)
## keep only the actual sales
bkmn.sale <- bkmn[bkmn$sale.price.n!=0,]
plot(bkmn.sale$gross.sqft,bkmn.sale$sale.price.n)
plot(log(bkmn.sale$gross.sqft),log(bkmn.sale$sale.price.n))

area <- aggregate(bkmn$sale.price.n,by=list(Category=bkmn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood","Total.Sales")
library(plyr)
area <- arrange(area,desc(Total.Sales))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Total.Sales),y=Total.Sales,fill=Neighborhood)) + geom_bar(stat="identity")

## for now, let's look at 1-, 2-, and 3-family homes
bkmn.homes <- bkmn.sale[which(grepl("RENTALS",bkmn.sale$building.class.category)),]
plot(log(bkmn.homes$gross.sqft),log(bkmn.homes$sale.price.n))
bkmn.homes[which(bkmn.homes$sale.price.n<100000),][order(bkmn.homes[which(bkmn.homes$sale.price.n<100000),]$sale.price.n),]
## remove outliers that seem like they weren't actual sales
bkmn.homes$outliers <- (log(bkmn.homes$sale.price.n) <=5) + 0
bkmn.homes <- bkmn.homes[which(bkmn.homes$outliers==0),]
plot(log(bkmn.homes$gross.sqft),log(bkmn.homes$sale.price.n))

area <- aggregate(bkmn$sale.price.n,by=list(Category=bkmn$neighborhood),FUN=max)
colnames(area) <- c("Neighborhood","Biggest.Sale")
area <- arrange(area,desc(Biggest.Sale))
ggplot(tail(area,5),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")
ggplot(head(area,10),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")

#arranged biggets sale in ascending order
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Biggest.Sale),y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")

bkmn$year<-gsub(".*(\\d{4})-(\\d{2}).*","\\1-\\2",bkmn$sale.date)
yearly <- aggregate(bkmn$sale.price.n,by=list(Category=bkmn$year),FUN=sum)
colnames(yearly) <- c("Year.Month", "Sales")
ggplot(yearly,aes(x= reorder(Year.Month,-Sales),y=Sales,fill=Year.Month)) + geom_bar(stat="identity")

filterbkmn <- bkmn[bkmn$land.sqft>0,]
filterbkmn <- filterbkmn[filterbkmn$gross.sqft>0,]
byland <- aggregate(filterbkmn$land.sqft,by=list(Category=filterbkmn$year),FUN=sum)

area <- aggregate(filterbkmn$land.sqft,by=list(Category=filterbkmn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood", "Land.Sq.Ft")
area <- arrange(area,desc(Land.Sq.Ft))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Land.Sq.Ft),y=Land.Sq.Ft,fill=Neighborhood)) + geom_bar(stat="identity")




#For the rollingsales_queens data set.
require(gdata)
bkqn<- read.xls("rollingsales_queens.xls",perl=perl,pattern="BOROUGH")
head(bkqn)
summary(bkqn)
bkqn$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bkqn$SALE.PRICE))
count(is.na(bkqn$SALE.PRICE.N))
names(bkqn) <- tolower(names(bkqn))

## clean/format the data with regular expressions
bkqn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bkqn$gross.square.feet))
bkqn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bkqn$land.square.feet))
bkqn$sale.date <- as.Date(bkqn$sale.date)
bkqn$year.built <- as.numeric(as.character(bkqn$year.built))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bkqn)
hist(sale.price.n)
hist(gross.sqft[sale.price.n==0])
detach(bkqn)
## keep only the actual sales
bkqn.sale <- bkqn[bkqn$sale.price.n!=0,]
plot(bkqn.sale$gross.sqft,bkqn.sale$sale.price.n)
plot(log(bkqn.sale$gross.sqft),log(bkqn.sale$sale.price.n))

area <- aggregate(bkqn$sale.price.n,by=list(Category=bkqn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood","Total.Sales")
library(plyr)
area <- arrange(area,desc(Total.Sales))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Total.Sales),y=Total.Sales,fill=Neighborhood)) + geom_bar(stat="identity")

## for now, let's look at 1-, 2-, and 3-family homes
bkqn.homes <- bkqn.sale[which(grepl("RENTALS",bkqn.sale$building.class.category)),]
plot(log(bkqn.homes$gross.sqft),log(bkqn.homes$sale.price.n))
bkqn.homes[which(bkqn.homes$sale.price.n<100000),][order(bkqn.homes[which(bkqn.homes$sale.price.n<100000),]$sale.price.n),]
## remove outliers that seem like they weren't actual sales
bkqn.homes$outliers <- (log(bkqn.homes$sale.price.n) <=5) + 0
bkqn.homes <- bkqn.homes[which(bkqn.homes$outliers==0),]
plot(log(bkmn.homes$gross.sqft),log(bkmn.homes$sale.price.n))

area <- aggregate(bkqn$sale.price.n,by=list(Category=bkqn$neighborhood),FUN=max)
colnames(area) <- c("Neighborhood","Biggest.Sale")
area <- arrange(area,desc(Biggest.Sale))
ggplot(head(area,10),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")
#arranged biggets sale in ascending order
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Biggest.Sale),y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")

bkqn$year<-gsub(".*(\\d{4})-(\\d{2}).*","\\1-\\2",bkqn$sale.date)
yearly <- aggregate(bkqn$sale.price.n,by=list(Category=bkqn$year),FUN=sum)
colnames(yearly) <- c("Year.Month", "Sales")
ggplot(yearly,aes(x= reorder(Year.Month,-Sales),y=Sales,fill=Year.Month)) + geom_bar(stat="identity")

filterbkqn <- bkqn[bkqn$land.sqft>0,]
filterbkqn <- filterbkqn[filterbkqn$gross.sqft>0,]
byland <- aggregate(filterbkqn$land.sqft,by=list(Category=filterbkqn$year),FUN=sum)

area <- aggregate(filterbkqn$land.sqft,by=list(Category=filterbkqn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood", "Land.Sq.Ft")
area <- arrange(area,desc(Land.Sq.Ft))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Land.Sq.Ft),y=Land.Sq.Ft,fill=Neighborhood)) + geom_bar(stat="identity")




#For the rollingsales_statenisland data set.
require(gdata)
bksn<- read.xls("rollingsales_statenisland.xls",perl=perl,pattern="BOROUGH")
head(bksn)
summary(bksn)
bksn$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bksn$SALE.PRICE))
count(is.na(bksn$SALE.PRICE.N))
names(bksn) <- tolower(names(bksn))

## clean/format the data with regular expressions
bksn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bksn$gross.square.feet))
bksn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bksn$land.square.feet))
bksn$sale.date <- as.Date(bksn$sale.date)
bksn$year.built <- as.numeric(as.character(bksn$year.built))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bksn)
hist(sale.price.n)
hist(gross.sqft[sale.price.n==0])
detach(bksn)
## keep only the actual sales
bksn.sale <- bksn[bksn$sale.price.n!=0,]
plot(bksn.sale$gross.sqft,bksn.sale$sale.price.n)
plot(log(bksn.sale$gross.sqft),log(bksn.sale$sale.price.n))

area <- aggregate(bksn$sale.price.n,by=list(Category=bksn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood","Total.Sales")
library(plyr)
area <- arrange(area,desc(Total.Sales))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Total.Sales),y=Total.Sales,fill=Neighborhood)) + geom_bar(stat="identity")

## for now, let's look at 1-, 2-, and 3-family homes
bksn.homes <- bksn.sale[which(grepl("RENTALS",bksn.sale$building.class.category)),]
plot(log(bksn.homes$gross.sqft),log(bksn.homes$sale.price.n))
bksn.homes[which(bksn.homes$sale.price.n<100000),][order(bksn.homes[which(bksn.homes$sale.price.n<100000),]$sale.price.n),]
## remove outliers that seem like they weren't actual sales
bksn.homes$outliers <- (log(bksn.homes$sale.price.n) <=5) + 0
bksn.homes <- bksn.homes[which(bksn.homes$outliers==0),]
plot(log(bksn.homes$gross.sqft),log(bksn.homes$sale.price.n))

area <- aggregate(bksn$sale.price.n,by=list(Category=bksn$neighborhood),FUN=max)
colnames(area) <- c("Neighborhood","Biggest.Sale")
area <- arrange(area,desc(Biggest.Sale))
ggplot(head(area,10),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")
#arranged biggets sale in ascending order
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Biggest.Sale),y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")

bksn$year<-gsub(".*(\\d{4})-(\\d{2}).*","\\1-\\2",bksn$sale.date)
yearly <- aggregate(bksn$sale.price.n,by=list(Category=bksn$year),FUN=sum)
colnames(yearly) <- c("Year.Month", "Sales")
ggplot(yearly,aes(x= reorder(Year.Month,-Sales),y=Sales,fill=Year.Month)) + geom_bar(stat="identity")

filterbksn <- bksn[bksn$land.sqft>0,]
filterbksn <- filterbksn[filterbksn$gross.sqft>0,]
byland <- aggregate(filterbksn$land.sqft,by=list(Category=filterbksn$year),FUN=sum)

area <- aggregate(filterbksn$land.sqft,by=list(Category=filterbksn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood", "Land.Sq.Ft")
area <- arrange(area,desc(Land.Sq.Ft))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Land.Sq.Ft),y=Land.Sq.Ft,fill=Neighborhood)) + geom_bar(stat="identity")



#For the rollingsales_bronx data set.
require(gdata)
bkxn<- read.xls("rollingsales_bronx.xls",perl=perl,pattern="BOROUGH")
head(bkxn)
summary(bkxn)
bkxn$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bkxn$SALE.PRICE))
count(is.na(bkxn$SALE.PRICE.N))
names(bkxn) <- tolower(names(bkxn))

## clean/format the data with regular expressions
bkxn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bkxn$gross.square.feet))
bkxn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bkxn$land.square.feet))
bkxn$sale.date <- as.Date(bkxn$sale.date)
bkxn$year.built <- as.numeric(as.character(bkxn$year.built))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bkxn)
hist(sale.price.n)
hist(gross.sqft[sale.price.n==0])
detach(bkxn)
## keep only the actual sales
bkxn.sale <- bkxn[bkxn$sale.price.n!=0,]
plot(bkxn.sale$gross.sqft,bkxn.sale$sale.price.n)
plot(log(bkxn.sale$gross.sqft),log(bkxn.sale$sale.price.n))

area <- aggregate(bkxn$sale.price.n,by=list(Category=bkxn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood","Total.Sales")
library(plyr)
area <- arrange(area,desc(Total.Sales))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Total.Sales),y=Total.Sales,fill=Neighborhood)) + geom_bar(stat="identity")

## for now, let's look at 1-, 2-, and 3-family homes
bkxn.homes <- bkxn.sale[which(grepl("RENTALS",bkxn.sale$building.class.category)),]
plot(log(bkxn.homes$gross.sqft),log(bkxn.homes$sale.price.n))
bkxn.homes[which(bkxn.homes$sale.price.n<100000),][order(bkxn.homes[which(bksn.homes$sale.price.n<100000),]$sale.price.n),]
## remove outliers that seem like they weren't actual sales
bkxn.homes$outliers <- (log(bkxn.homes$sale.price.n) <=5) + 0
bkxn.homes <- bkxn.homes[which(bkxn.homes$outliers==0),]
plot(log(bksn.homes$gross.sqft),log(bksn.homes$sale.price.n))

area <- aggregate(bkxn$sale.price.n,by=list(Category=bkxn$neighborhood),FUN=max)
colnames(area) <- c("Neighborhood","Biggest.Sale")
area <- arrange(area,desc(Biggest.Sale))
ggplot(head(area,10),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")
#arranged biggets sale in ascending order
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Biggest.Sale),y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")

bkxn$year<-gsub(".*(\\d{4})-(\\d{2}).*","\\1-\\2",bkxn$sale.date)
yearly <- aggregate(bkxn$sale.price.n,by=list(Category=bkxn$year),FUN=sum)
colnames(yearly) <- c("Year.Month", "Sales")
ggplot(yearly,aes(x= reorder(Year.Month,-Sales),y=Sales,fill=Year.Month)) + geom_bar(stat="identity")

filterbkxn <- bkxn[bkxn$land.sqft>0,]
filterbkxn <- filterbkxn[filterbkxn$gross.sqft>0,]
byland <- aggregate(filterbkxn$land.sqft,by=list(Category=filterbkxn$year),FUN=sum)

area <- aggregate(filterbkxn$land.sqft,by=list(Category=filterbkxn$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood", "Land.Sq.Ft")
area <- arrange(area,desc(Land.Sq.Ft))
#3###333#CONSIDER
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Land.Sq.Ft),y=Land.Sq.Ft,fill=Neighborhood)) + geom_bar(stat="identity")
