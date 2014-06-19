#########################################################
#
# Author: Jeff Hemsley
# 
# Purpose: read in analytics data and make plots
#
# Issues:
#

options(scipen=99)
data.dir <- "C:/r/aDiss/dat/"
plot.dir <- "C:/r/aDiss/descriptivePlots/"


#####################################################################
#
# Daily Rate of tweets and retweets
#
#####################################################################
daily.analytic.data.file.name <- "dailyTweets.csv"
daily.analytic.data.file <- paste(data.dir, daily.analytic.data.file.name, sep="")
daily.analytic.data <- as.data.frame(read.csv(file=daily.analytic.data.file, check.names=T, stringsAsFactors=F))
colnames(daily.analytic.data)

o <- order(daily.analytic.data$year, daily.analytic.data$month, daily.analytic.data$day) 
daily.analytic.data <- daily.analytic.data[o,]

str(daily.analytic.data)
if (daily.analytic.data[1,2] < 10) {
  daily.analytic.data <- daily.analytic.data[-1,]
}

daily.analytic.data[c(1, dim(daily.analytic.data)[1]),]

daily.analytic.data$date <- strptime(paste(daily.analytic.data$year, daily.analytic.data$month, daily.analytic.data$day, sep="-"), "%Y-%m-%d", tz="GMT")
date.x <- seq.POSIXt (min(daily.analytic.data$date), max(daily.analytic.data$date), by="days")
date.y <- rep(0, length(date.x))

A <- as.Date(daily.analytic.data$date)
B <- as.Date(date.x)
B[!is.element(B,A)] # missing dates

class(daily.analytic.data$date)
plot(daily.analytic.data$date, daily.analytic.data$tweets, type="l")

par(mar = c(3.6, 3, 2, 1))  #c(5.1,4.1,4.1,2.1)
#plot(daily.analytic.data$date, daily.analytic.data$tweets
plot(date.x, date.y, ylim=c(0, max(daily.analytic.data$tweets))
    , type="n", bty="n", yaxt="n", main="", ylab="", xlab="")

lines(daily.analytic.data$date, daily.analytic.data$tweets, type="h")
axis(2, at=axTicks(2), labels=axTicks(2)/1000, las=2, tick=F, line=-1, cex=1.2)
mtext(text="1000s of tweets", side=2, line=1.7)
mtext(text="October 2011 - June 2012", side=1, line=2.2)
mtext(text="Daily Rate of Tweets", side=3, line=-0.8, cex=1.5)
mtext(text="and reweets", side=3, line=-1.8, cex=1.2, col=rgb(179, 0, 0, maxColorValue=255))

lines(daily.analytic.data$date, daily.analytic.data$retweets
  , type="h", lwd=1, col=rgb(179, 0, 0, maxColorValue=255))

sum(daily.analytic.data$tweets)
summary(daily.analytic.data$tweets)

sum(daily.analytic.data$retweets)
summary(daily.analytic.data$retweets)


o <- order(daily.analytic.data$tweets, decreasing=TRUE)

daily.analytic.data.ordered <- daily.analytic.data[o,]

df <- head(daily.analytic.data.ordered[,c("date", "tweets", "retweets")], n=10)

df <- tail(daily.analytic.data.ordered[,c("date", "tweets", "retweets")], n=10)
o <- order(df$date)
df[o,]


daily.analytic.data[50:60,]

boxplot
#####################################################################
#
# Retweet size freqencies
#
#####################################################################
rte.count.dist.file.name <- "rte_count_dist_upto_30k.csv"
rte.count.dist.file <- paste(data.dir, rte.count.dist.file.name, sep="")
rte.count.dist <- as.data.frame(read.csv(file=rte.count.dist.file, check.names=T, stringsAsFactors=F))
colnames(rte.count.dist)

o <- order(rte.count.dist$rte_size) 
rte.count.dist <- rte.count.dist[o,]

str(rte.count.dist)

rte.count.dist$max <- FALSE
tmp.index <- which.max(rte.count.dist$rte_size[rte.count.dist$count > 0])

rte.count.dist[(rte.count.dist$count > 0),][tmp.index,]$max <- TRUE

rte.size.dist <- rte.count.dist[1:which(rte.count.dist$max),1:2]
tail(rte.size.dist)
par(mar = c(3.6, 3.2, 2, 1))  #c(5.1,4.1,4.1,2.1)
plot(log10(rte.size.dist$rte_size), log10(rte.size.dist$count)
  , bty="n", main="", ylab="", xlab="")

# abline(v=2)
mtext(text="log10(frequency)", side=2, line=2.2)
mtext(text="log10(RTE Size)", side=1, line=2.2)
mtext(text="Log-log plot of RTE sizes", side=3, line=-0.8, cex=1.5)

summary(sort(rte.size.dist$rte_size))

sum(rte.count.dist$rte_size)
  


length(rte.size.dist$rte_size)
rte.size.dist$rte_size[90:120]



