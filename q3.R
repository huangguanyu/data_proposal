library(maps)
library(geosphere)
library(plotly)


airports <- read.csv("599627084_T_MASTER_CORD.csv",header = TRUE)
flights  <- read.csv("599524932_T_T100D_SEGMENT_US_CARRIER_ONLY.csv",header=TRUE)

xlim <- c(-171.738281, -56.601563)
ylim <- c(10, 55)


flight_dl <-flights[flights$UNIQUE_CARRIER=="DL",]
#we only use the top 20 passagers air routes

# we calculate annual passerger for each route
dl_year_passengers <- aggregate(flight_dl$PASSENGERS,by=list(flight_dl$ORIGIN,flight_dl$DEST),sum,na.rm=TRUE)
colnames(dl_year_passengers) <- c("ORIGIN","DEST","PASSENGERS")
dl_year_passengers <- dl_year_passengers[order(dl_year_passengers$PASSENGERS,decreasing=TRUE)[1:200],]

jpeg("delta.jpg",width=1920, height=1080)
map("world", col="#f2f2f2", fill=TRUE, bg="white", 
    lwd=0.05, xlim=xlim, ylim=ylim)

for (i in (1:length(dl_year_passengers$ORIGIN))) {
  
  air1 <- airports[as.character(airports$AIRPORT) == as.character(dl_year_passengers[i,]$ORIGIN),]
  air2 <- airports[as.character(airports$AIRPORT) == as.character(dl_year_passengers[i,]$DEST),]
  
  inter <- gcIntermediate(c(air1[1,]$LONGITUDE, air1[1,]$LATITUDE), c(air2[1,]$LONGITUDE, air2[1,]$LATITUDE), n=100, addStartEnd=TRUE)
  
  lines(inter, col="black", lwd=3)
  
}

title("Delta Ailines Routes")
dev.off()
#----------------------------------------------------------------------------------
#AA plots
flight_aa <-flights[flights$UNIQUE_CARRIER=="AA",]
#we only flight_aause the top 20 passagers air routes

# we calculate annual passerger for each route
aa_year_passengers <- aggregate(flight_aa$PASSENGERS,by=list(flight_aa$ORIGIN,flight_aa$DEST),sum,na.rm=TRUE)
colnames(aa_year_passengers) <- c("ORIGIN","DEST","PASSENGERS")
aa_year_passengers <- aa_year_passengers[order(aa_year_passengers$PASSENGERS,decreasing=TRUE)[1:200],]

jpeg("aa.jpg",width=1920,height=1080)
map("world", col="#f2f2f2", fill=TRUE, bg="white", 
    lwd=0.05, xlim=xlim, ylim=ylim)

for (i in (1:length(aa_year_passengers$ORIGIN))) {
  
  air1 <- airports[as.character(airports$AIRPORT) == as.character(aa_year_passengers[i,]$ORIGIN),]
  air2 <- airports[as.character(airports$AIRPORT) == as.character(aa_year_passengers[i,]$DEST),]
  
  inter <- gcIntermediate(c(air1[1,]$LONGITUDE, air1[1,]$LATITUDE), c(air2[1,]$LONGITUDE, air2[1,]$LATITUDE), n=100, addStartEnd=TRUE)
  
  lines(inter, col="black", lwd=3)
  
}
title("American Ailines Routes")
dev.off()
#---------------------------------------------------------------------------------------------------------
#UA plots
flight_ua <-flights[flights$UNIQUE_CARRIER=="UA",]
#we only flight_aause the top 20 passagers air routes

# we calculate annual passerger for each route
ua_year_passengers <- aggregate(flight_ua$PASSENGERS,by=list(flight_ua$ORIGIN,flight_ua$DEST),sum,na.rm=TRUE)
colnames(ua_year_passengers) <- c("ORIGIN","DEST","PASSENGERS")
ua_year_passengers <- ua_year_passengers[order(ua_year_passengers$PASSENGERS,decreasing=TRUE)[1:200],]
jpeg("ua.jpg",width=1920,height=1080)
map("world", col="#f2f2f2", fill=TRUE, bg="white", 
    lwd=0.05, xlim=xlim, ylim=ylim)

#colnames(year_passengers) <- c("ORGIN","DEST","PASSENGERS")

#year_passengers_routes <-cbind(character(length(year_passengers$DEST)/2),character(length(year_passengers$DEST)/2),
#                                    numeric(length(year_passengers$DEST)/2))
#colnames(year_passengers_routes) <- c("airport1","airport2","PASSENGERS")

for (i in (1:length(ua_year_passengers$ORIGIN))) {
  
  air1 <- airports[as.character(airports$AIRPORT) == as.character(ua_year_passengers[i,]$ORIGIN),]
  air2 <- airports[as.character(airports$AIRPORT) == as.character(ua_year_passengers[i,]$DEST),]
  
  inter <- gcIntermediate(c(air1[1,]$LONGITUDE, air1[1,]$LATITUDE), c(air2[1,]$LONGITUDE, air2[1,]$LATITUDE), n=100, addStartEnd=TRUE)
  
  lines(inter, col="black", lwd=3)
  
}
title("United Ailines Routes")
dev.off()

#------------------------------
#now we plot top3

flight_all <-flights[flights$UNIQUE_CARRIER=="DL" | flights$UNIQUE_CARRIER=="UA" | flights$UNIQUE_CARRIER=="AA",]

all_year_passengers <- aggregate(flight_all$PASSENGERS,by=list(flight_all$ORIGIN,flight_all$DEST,flight_all$UNIQUE_CARRIER),sum,na.rm=TRUE)

colnames(all_year_passengers) <- c("ORIGIN","DEST","UNIQUE_CARRIER","PASSENGERS")
all_year_passengers <- all_year_passengers[order(all_year_passengers$PASSENGERS,decreasing=TRUE)[1:2000],]

jpeg("all.jpg",width=1920,height=1080)
map("world", col="#f2f2f2", fill=TRUE, bg="white", 
    lwd=0.05, xlim=xlim, ylim=ylim)
for (i in (1:length(all_year_passengers$ORIGIN))) {
  
  air1 <- airports[as.character(airports$AIRPORT) == as.character(all_year_passengers[i,]$ORIGIN),]
  air2 <- airports[as.character(airports$AIRPORT) == as.character(all_year_passengers[i,]$DEST),]
  
  inter <- gcIntermediate(c(air1[1,]$LONGITUDE, air1[1,]$LATITUDE), c(air2[1,]$LONGITUDE, air2[1,]$LATITUDE), n=100, addStartEnd=TRUE)
  
  if (all_year_passengers[i,]$UNIQUE_CARRIER== "DL") line_color <- "black"
  if (all_year_passengers[i,]$UNIQUE_CARRIER== "AA") line_color <- "red"
  if (all_year_passengers[i,]$UNIQUE_CARRIER== "UA") line_color <- "green"
  
  
  lines(inter, col=line_color, lwd=2)
  
}

legend("bottomleft",c("DL","AA","UA"),col=c("black","red","green"),pch=c(1,1,1),lwd=3)

title("Routes")
dev.off()

#------------------------------------

# monthly passengers
dl_month_passengers <- aggregate(flight_dl$PASSENGERS,by=list(flight_dl$MONTH),sum)
aa_month_passengers <- aggregate(flight_aa$PASSENGERS,by=list(flight_aa$MONTH),sum)
ua_month_passengers <- aggregate(flight_ua$PASSENGERS,by=list(flight_ua$MONTH),sum)

colnames(dl_month_passengers)<-c("Month","Passengers")
colnames(aa_month_passengers)<-c("Month","Passengers")
colnames(ua_month_passengers)<-c("Month","Passengers")


jpeg("monthly_passengers.jpg",width=1920,height=1080)

plot_ly(x = dl_month_passengers$Month, y = dl_month_passengers$Passengers, name = "DL") %>%
  add_trace(x = aa_month_passengers$Month, y = aa_month_passengers$Passengers , name = "AA") %>%
  add_trace(x = ua_month_passengers$Month, y = ua_month_passengers$Passengers, name = "UA") %>%
  layout(xaxis = list(title = "Month"), yaxis = list(title = "Passengers"))

dev.off()


