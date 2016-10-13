require(gdata)
bk <- read.xls("rollingsales_brooklyn.xls",perl=perl,pattern="BOROUGH")
head(bk)
summary(bk)
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))
names(bk) <- tolower(names(bk))

## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))
## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n)
hist(gross.sqft[sale.price.n==0])
detach(bk)
## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))

area <- aggregate(bk$sale.price.n,by=list(Category=bk$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood","Total.Sales")
library(plyr)
area <- arrange(area,desc(Total.Sales))
library(ggplot2)
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Total.Sales),y=Total.Sales,fill=Neighborhood)) + geom_bar(stat="identity")


## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("RENTALS",bk.sale$building.class.category)),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),]$sale.price.n),]
## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))



area <- aggregate(bk$sale.price.n,by=list(Category=bk$neighborhood),FUN=max)
colnames(area) <- c("Neighborhood","Biggest.Sale")
area <- arrange(area,desc(Biggest.Sale))
ggplot(head(area,5),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")
ggplot(tail(area,5),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")
ggplot(head(area,10),aes(x=Neighborhood,y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")

#arranged biggets sale in ascending order
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Biggest.Sale),y=Biggest.Sale,fill=Neighborhood)) + geom_bar(stat="identity")


bk$year<-gsub(".*(\\d{4})-(\\d{2}).*","\\1-\\2",bk$sale.date)
yearly <- aggregate(bk$sale.price.n,by=list(Category=bk$year),FUN=sum)
colnames(yearly) <- c("Year.Month", "Sales")
ggplot(yearly,aes(x= reorder(Year.Month,-Sales),y=Sales,fill=Year.Month)) + geom_bar(stat="identity")


filterbk <- bk[bk$land.sqft>0,]
filterbk <- filterbk[filterbk$gross.sqft>0,]
byland <- aggregate(filterbk$land.sqft,by=list(Category=filterbk$year),FUN=sum)


area <- aggregate(filterbk$land.sqft,by=list(Category=filterbk$neighborhood),FUN=sum)
colnames(area) <- c("Neighborhood", "Land.Sq.Ft")
area <- arrange(area,desc(Land.Sq.Ft))
ggplot(head(area,10),aes(x=reorder(Neighborhood,-Land.Sq.Ft),y=Land.Sq.Ft,fill=Neighborhood)) + geom_bar(stat="identity")

