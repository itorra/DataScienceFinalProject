require(gtable)
require(ggplot2)
library(gtable)
library(ggplot2)


yem <- read.csv("phish.csv",stringsAsFactors = FALSE,colClasses = c(NA,NA,NA,"Date","Date",NA))

head(yem)

barplot(table(weekdays(as.Date(yem$Debut))))


cover <- format(yem$Original.Artist)
cover2 <- rep("Cover",length(cover))
cover2[grepl("Phish",cover)] <- "Original"

fluffhead = data.frame( "time_played" = as.numeric(yem$Times.Played),
                        "current_gap" = as.numeric(yem$Current.Gap),
                        "years_since_debuted" = floor(as.numeric(as.Date("2017", format="%Y") - yem$Debut)/365.25),
                        "years_since_last_played" = floor(as.numeric(as.Date("2017", format="%Y") - yem$Last.Seen)/365.25),
                        "is_original" = cover2
)


barplot(table(cover2),main = "Covers Vs. Originals")

forbin = kmeans(fluffhead[c("time_played","current_gap","years_since_debuted","years_since_last_played")],3)

# Let's compare the clusters with the songs
table(forbin$cluster, fluffhead$is_original)

#and now we can plot it

ggplot(fluffhead, aes(years_since_debuted, years_since_last_played, color = forbin$cluster)) + geom_point()

plot(fluffhead[c("years_since_last_played","years_since_debuted")], col= forbin$cluster)
points(forbin$centers[,c("years_since_last_played","years_since_debuted")], col=3:5,pch=8,cex=2)


plot(fluffhead[c("time_played","current_gap","years_since_debuted","years_since_last_played")], col= forbin$cluster)
points(forbin$centers[,c("time_played","current_gap","years_since_debuted","years_since_last_played")], col=1:5,pch=8,cex=2)

plot(as.numeric(format(as.Date(yem$Debut), "%Y")),as.numeric(format(as.Date(yem$Last.Seen), "%Y")), xlab="Debut Year", ylab="Last Seen Year",main="Songs Left Behind",pch=20,xlim=c(1983,2016),ylim=c(1983,2016), col=ifelse(cover2=="Original", rgb(100,0,0,60,maxColorValue=255),rgb(0,0,100,60,maxColorValue=255)), cex=3)
legend("bottomright",legend =c("Original","Cover"),col=c(rgb(100,0,0,100,maxColorValue=255),rgb(0,0,100,100,maxColorValue=255)),pch=20)

plot(yem$Times.Played,yem$Current.Gap,col=ifelse(cover2=="Original","red","blue"),pch=20,cex=1,main="The bust outs",xlab="Time played",ylab="Current gap")
legend("topright",legend =c("Original","Cover"),col=c("red","blue"),pch=20)