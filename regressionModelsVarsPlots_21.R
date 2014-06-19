#########################################################
#
# Author: Jeff Hemsley
# 
# Purpose: code for my disertation
# Description: reads in data cleaned by processRTEs_vX.R
#              and fits a regression model.
#
#
# Regression notes for write up:
# Faraway, J. J. (2004). Linear models with R (Vol. 63). Chapman and Hall/CRC.
#
# Non constance variance can be delt with by transforming the responce 
# varaible, though this makes interpritation difficult. P. 56
# sqrt is good for count vars
#
# http://www.statmethods.net/stats/rdiagnostics.html
# http://people.duke.edu/~rnau/testing.htm
# http://www.basic.northwestern.edu/statguidefiles/linreg_ass_viol.html

include.files.dir <- log.file.path <- "c:/r/aDiss/processRTEs/"
# contains global variable settings
process.rtes.config.file.name <- "processRTEs_CONFIG_v0.R"
source(paste(include.files.dir, process.rtes.config.file.name, sep=""))

library(car)
library(faraway)
library(MASS)

regression.plot.dir <- "C:/r/aDiss/descriptivePlots/"
track.words.file <- "c:/r/aDiss/processRTEs/tracking.v8.R"
source(track.words.file)
track.8.words <- tolower(track.8.words)
length(track.8.words)


rdata.file <- "C:/r/aDiss/processRTEs/ConstructedRTEdata_201405151457.Rdata"
rdata.file <- "C:/r/aDiss/processRTEs/ConstructedRTEdata_201405201715.Rdata"
# has gini in it:
# rdata.file <- "C:/r/aDiss/processRTEs/ConstructedRTEdata_201406051526.Rdata"
load(rdata.file)
rte.meta.df$origin.user.lower <- tolower(rte.meta.df$origin.user)
colnames(rte.meta.df)

tmp.dates <- format(strptime(rte.meta.df$start.date, created.ts.format.string), "%d%b%Y")
rte.meta.df$Nov15 <- ifelse(tmp.dates == "15Nov2011", 1, 0)
rte.meta.df$Nov17 <- ifelse(tmp.dates == "17Nov2011", 1, 0)

track.list.users.index <- which(rte.meta.df$origin.user.lower %in% track.8.words)
occ.initiators <- unique(rte.meta.df$origin.user[track.list.users.index])
occ.rtes <- rte.meta.df[track.list.users.index,]
dim(occ.rtes) # 436, 34
colnames(occ.rtes)

#########################################################
#
# Functions
#
#########################################################
# # # #
# tri plot function for transformed vars
TransformTriPlot <- function(x, y, outliers, xlab, ylab, transform.var="log", fname="", log.y=TRUE, plot.file=T) {
  #outliers <- high.alpha.index
  #x <- rtes$occ.users + 2
  #y <- rtes$alpha
  #ylab <- xlab<-""
  #summary(x)
  #fname="test"
  #log.y <- F
  if (plot.file == TRUE) {
    plot.file.name.png <- paste(regression.plot.dir, "TriPlot_", fname, ".png", sep="")
    png(plot.file.name.png, width=625, height=230, pointsize=16)
  }
  
  tmp.col <- rep(rgb(64, 153, 255, maxColorValue=255), length(x))
  tmp.col[outliers] <- "red"
  par(mfrow=c(1,3))
  plot(x, y, col=tmp.col, bty="n", main=paste("Nontransformed\ncor=", round(cor(x,y), 3), sep=""), ylab=ylab, xlab=xlab)
  abline(lm(y~x), col="black", lwd=1)
  if (log.y == TRUE) {
    y <- log10(y)
    ylab <- paste("log(", ylab, ")", sep="")
  }
  
  if (transform.var == "log") {
    x <- log10(x)
  } else if (transform.var == "sqrt") {
    x <- sqrt(x)
    #y <- sqrt(y)
  }
  plot(x, y, col=tmp.col, bty="n", main=paste("Transformed\ncor=", round(cor(x,y), 3), sep=""), ylab=ylab, xlab=paste(transform.var, "(", xlab, ")", sep=""))
  abline(lm(y~x), col="black", lwd=1)
  x <- x[-outliers]
  y <- y[-outliers]
  plot(x, y, col=tmp.col[-outliers], bty="n", main=paste("Transformed\nno outliers\ncor=", round(cor(x,y), 3), sep=""), ylab=ylab, xlab=paste(transform.var, "(", xlab, ")", sep=""))
  abline(lm(y~x), col="black", lwd=1)
  par(mfrow=c(1,1))
  
  if (plot.file == TRUE) {
    dev.off()
  }
}


#########################################################
#
# REGRESSION:: variable plots & transformations
#
#########################################################
rtes <- occ.rtes



# remove any rtes that failed to get a closeness or alpha
closeness.na.index <- which(is.na(rtes$follow.prop.closeness))
alpha.na.index <- which(is.na(rtes$alpha))
rtes <- rtes[-(union(closeness.na.index, alpha.na.index)),]
#rtes.bak <- rtes
#rtes <- rtes.bak
#rtes <- rtes[-outs,]
rtes <- rtes[sample(1:dim(rtes)[1]),]

row.names(rtes) <- 1:nrow(rtes)
dim(rtes)


summary.main.text <- paste("Output Variable Correlation Matrix\nn = ", dim(rtes)[1], sep="")
plot.file.name.png <- paste(regression.plot.dir, dim(rtes)[1], "_NewALL_VarsCorrMat.png", sep="")
plot.file.name.pdf <- paste(regression.plot.dir, dim(rtes)[1], "_NewALL_VarsCorrMat.pdf", sep="")
png(plot.file.name.png, width=800, height=800, pointsize=18)
#pdf(plot.file.name.pdf, width=12, height=12, pointsize=10)

pairs(~follower.delta+alpha+follow.prop.closeness
  +window.size+peak.at+peak.rate
  +follower.win.delta+follower.mean+tweet.overlap+occ.users
  +occupy.time.marker+occ.peak.time.marker
  #+size+window.size+peak.at+peak.rate+longest.path+count.pre.peak
  #  +follower.sum+follower.mean+tweet.overlap+occ.users
  #  +occupy.time.marker+occ.peak.time.marker+Nov15+Nov17
  , data=rtes
  , lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel = panel.hist, text.panel=my.text.panel
  , lwd=2, pch=20, col=adjustAlpha("dodgerblue4", .3) # col=adjustAlpha("forestgreen", .3)
  , main="")
mtext(text=summary.main.text, side=3, line=1)
dev.off()


# # # #
# high alphas
high.alpha.index <- which(rtes$alpha > 2.3)
high.alpha.index <- intersect(which(rtes$occupy.time.marker > 150), high.alpha.index)
length(high.alpha.index)
late.set <- rtes[high.alpha.index,]
colnames(late.set)
late.set[,c("start.date")]
add.days <- late.set$occ.peak.time.marker - late.set$occupy.time.marker
tmp.date <- as.POSIXlt(late.set$start.date)
tmp.date$hour <- tmp.date$hour + (24 * add.days)
sort(tmp.date)


# # # #
# broken occupytime functions
occ.time.lh <- occ.time.rh <- rtes$occupy.time.marker
occ.time.lh <- occ.time.rh <- rtes$occ.peak.time.marker
occ.time.lh[-high.alpha.index] <- 0
occ.time.rh[high.alpha.index] <- 0
plot(table(rtes$occ.peak.time.marker))

#1 - 80, 81 - 150, 150 up
octm.1 <- ifelse(rtes$occ.peak.time.marker > 80, 0, rtes$occ.peak.time.marker)
octm.2 <- rtes$occ.peak.time.marker
octm.2[octm.2 < 81] <- 0
octm.2[octm.2 > 149] <- 0
octm.3 <- ifelse(rtes$occ.peak.time.marker < 150, 0, rtes$occ.peak.time.marker)

rtes[which(rtes$occ.peak.time.marker == 150),]

occ.t.lh <- ifelse(rtes$occ.peak.time.marker < 150, rtes$occ.peak.time.marker, 0)
occ.t.rh <- ifelse(rtes$occ.peak.time.marker < 150, 0, rtes$occ.peak.time.marker)

plot(occ.t.lh, occ.t.rh)

days.in <- 90
occ.first.80 <- ifelse(rtes$occ.peak.time.marker < days.in, rtes$occ.peak.time.marker, 0)
occ.after.80 <- ifelse(rtes$occ.peak.time.marker < days.in, 0, rtes$occ.peak.time.marker)
#cbind(occ.time.rh, occ.time.lh)

# # # #
# vector for high alpha colors 
tmp.col <- rep("black", dim(rtes)[1])
tmp.col[high.alpha.index] <- "red"
late.high.alphas <- rep(0, dim(rtes)[1])
late.high.alphas[high.alpha.index] <- 1
plot(rtes$occ.peak.time.marker, rtes$alpha, col=tmp.col, cex=w.size.L/2)

summary(rtes$follower.delta)
follower.delta.L <- log10(rtes$follower.delta)
follower.win.delta.L <- rtes$follower.win.delta
follower.win.delta.L <- follower.win.delta.L + abs(min(follower.win.delta.L)) + 1
follower.win.delta.L <- log10(follower.win.delta.L)
plot(sort(rtes$follower.win.delta))

alpha.L <- log10(rtes$alpha)
closeness.L <- log(rtes$follow.prop.closeness)
size.L <- log10(rtes$size)
w.size.L <- log10(rtes$window.size)
peak.at <- rtes$peak.at
peak.at[peak.at == 0] <- 1
peak.at.L <- log10(peak.at)
peak.rate.L <- log10(rtes$peak.rate)
mean.followers.L <- log10(rtes$follower.mean)
occ.users.sqrt <- sqrt(rtes$occ.users + 1)
occupy.time.marker.L <- log10(rtes$occupy.time.marker)
#plot(table(rtes$occupy.time.marker))
occ.peak.time <- log10(rtes$occ.peak.time.marker)
occ.peak.time.L <- log10(rtes$occ.peak.time.marker)
overlap.sqrt <- sqrt(rtes$tweet.overlap)


######################################
# 
# DECRIPTIVES PLOTS & SUMMARIES
#
######################################
# "rte.ids"                "origin.user"            "num.bots"               "occ.users"              "start.date"            
# "occupy.time.marker"     "occ.peak.time.marker"   "follower.start"         "follower.delta"         "follower.win.delta"    
# "follower.percent.delta" "follower.sum"           "follower.mean"          "size"                   "window.size"           
# "first.rt.lag"           "last.rt.at"             "peak.at"                "count.pre.peak"         "peak.rate"             
# "post.peak"              "post.window"            "occ.start.minute"       "occ.end.minute"         "alpha"                 
# "KS.p"                   "poission.p"             "wait.closeness"         "follow.prop.closeness"  "follow.prob.closeness" 
# "post.peak.closeness"    "longest.path"           "chains"                 "tweet.overlap"          "tweet.overlap.minutes" 
# "origin.user.lower"      "Nov15"                  "Nov17"  

colnames(rtes)
dim(rtes) # 429 RTEs

sum(rtes$window.size) # 76,419

sum(rtes$size) # 84,972

table(rtes$origin.user)

plot(table(rtes$window.size))
plot(table(rtes$alpha))
plot(table(rtes$follower.win.delta))
plot(table(rtes$peak.rate))
plot(table(rtes$occ.peak.time.marker))
summary(rtes$occ.peak.time.marker)

colnames(rtes)

plot(rtes$peak.at + 1), rtes$peak.rate)
plot(rtes$peak.at, rtes$occ.peak.time.marker)
plot(rtes$peak.rate, rtes$occ.peak.time.marker)

cor.test(rtes$peak.at, rtes$occ.peak.time.marker)
cor.test(rtes$peak.rate, rtes$occ.peak.time.marker)

plot(peak.at.L, peak.rate.L)
plot(peak.at.L, occ.peak.time.L)
plot(peak.rate.L, occ.peak.time.L)


library(scatterplot3d)
s3d <-scatterplot3d(peak.at.L, occ.peak.time.L, peak.rate.L, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot")
fit.3d <- lm(peak.rate.L ~ peak.at.L + occ.peak.time.L)
s3d$plane3d(fit.3d)


s3d <-scatterplot3d(peak.at.L, occ.peak.time.L, rtes$alpha, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot")
fit.3d <- lm(rtes$alpha ~ peak.at.L + occ.peak.time.L)
s3d$plane3d(fit.3d)


s3d <-scatterplot3d(peak.at.L, peak.rate.L, occ.peak.time.L, pch=16, highlight.3d=TRUE,
  type="h", main="3D Scatterplot")
fit.3d <- lm(occ.peak.time.L ~ peak.at.L + peak.rate.L + rtes$alpha)
fit.3d <- lm(occ.peak.time.L ~ peak.at.L + peak.rate.L + rtes$alpha)

fit.3d <- lm(closeness.L ~ poly(occ.peak.time.marker,2), data=rtes)

s3d$plane3d(fit.3d)


summary(fit.3d)

pairs(~alpha+peak.at+peak.rate+occ.peak.time.marker
  #+size+window.size+peak.at+peak.rate+longest.path+count.pre.peak
  #  +follower.sum+follower.mean+tweet.overlap+occ.users
  #  +occupy.time.marker+occ.peak.time.marker+Nov15+Nov17
  , data=rtes
  , lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel = panel.hist, text.panel=my.text.panel
  , lwd=2, pch=20, col=adjustAlpha("dodgerblue4", .3) # col=adjustAlpha("forestgreen", .3)
  , main="")

tmp.df <- data.frame(alpha=rtes$alpha, closeness=rtes$follow.prop.closeness, PeakTime=rtes$peak.at, PeakRate=rtes$peak.rate, OccupyDay=rtes$occ.peak.time.marker)
cor(tmp.df) # get correlations
as.matrix(cor(tmp.df))
cor.test(rtes$alpha, rtes$follow.prop.closeness)
cor.test(rtes$follower.delta, rtes$follow.prop.closeness)

summary(lm(rtes$follower.delta~rtes$follow.prop.closeness+rtes$size))


library(Hmisc)
my.corrs <- rcorr(as.matrix(tmp.df), type="pearson")
names(my.corrs)

my.corrs$r <- round(my.corrs$r, 3)
my.corrs$P <- round(my.corrs$P, 3)

corr.mat <- my.corrs$r
corr.mat[lower.tri(my.corrs$P)] <- my.corrs$P[lower.tri(my.corrs$P)]

par(bty="n")
boxplot(rtes$alpha, horizontal = T, col="gold", bty="n", cex=1.2, pch=8)

boxplot(rtes$alpha, horizontal = T, col="gold", bty="n", cex=1.2, pch=8
  , border="white")

#tmp.col <- rep(rgb(64, 153, 255, maxColorValue=255), length(rtes$alpha))
tmp.col <- rep("orange", length(rtes$alpha))
tmp.col[high.alpha.index] <- "red"


points(rtes$alpha[high.alpha.index], rep(1, length(rtes$alpha[high.alpha.index])), pch = 1, col="red")
stripchart(rtes$alpha, pch=20, method="stack", col=tmp.col)


library(vioplot)
vioplot(rtes$alpha, names=c("alpha"), col="gold", drawRect=FALSE, horizontal = T)
title("Violin Plots of Miles Per Gallon")
colnames(rtes)

library(aplpack)

bagplot(rtes$occ.peak.time.marker, rtes$alpha, xlab="Occ Day", ylab="Alpha",
  main="Bagplot of Alpha and Occupy Day", cex=.75, col.baghull="gray85", col.loophull="gray95", show.whiskers=F) 



bagplot(rtes$follow.prop.closeness, rtes$alpha, xlab="Occ Day", ylab="Alpha",
  main="Bagplot of Alpha and Occupy Day", cex=.75, col.baghull="gray85", col.loophull="gray95", show.whiskers=F) 









######################################
# ALPHA
#
TransformTriPlot(rtes$follow.prop.closeness, rtes$alpha, high.alpha.index, xlab="closeness", ylab="alpha", transform="log", fname="AlphaClose", log.y=F)
TransformTriPlot(rtes$size, rtes$alpha, high.alpha.index, xlab="size", ylab="alpha", transform="log", fname="AlphaSize", log.y=F)
TransformTriPlot(rtes$window.size, rtes$alpha, high.alpha.index, xlab="window size", ylab="alpha", transform="log", fname="AlphaWinSize", log.y=F)
TransformTriPlot(rtes$peak.at + 1, rtes$alpha, high.alpha.index, xlab="peak time", ylab="alpha", transform="log", fname="AlphaPeakAt", log.y=F)
TransformTriPlot(rtes$peak.at + 1, rtes$alpha, high.alpha.index, xlab="peak time", ylab="alpha", transform="sqrt", fname="AlphaPeakAtSqrt", log.y=F)
TransformTriPlot(rtes$peak.rate, rtes$alpha, high.alpha.index, xlab="peak rate", ylab="alpha", transform="log", fname="AlphaPeakRate", log.y=F)

TransformTriPlot(rtes$occ.users + 2, rtes$alpha, high.alpha.index, xlab="occupy users", ylab="alpha", transform="log", fname="AlphaOccUsers", log.y=F)
TransformTriPlot(rtes$occ.users + 1, rtes$alpha, high.alpha.index, xlab="occupy users", ylab="alpha", transform="sqrt", fname="AlphaOccUsersSqrt", log.y=F)
TransformTriPlot(rtes$follower.mean, rtes$alpha, high.alpha.index, xlab="mean followers", ylab="alpha", transform="log", fname="AlphaMeanFollow", log.y=F)

TransformTriPlot(rtes$tweet.overlap + 1, rtes$alpha, high.alpha.index, xlab="tweet.overlap", ylab="alpha", transform="log", fname="AlphaOverlap", log.y=F)
TransformTriPlot(rtes$tweet.overlap + 1, rtes$alpha, high.alpha.index, xlab="tweet.overlap", ylab="alpha", transform="sqrt", fname="AlphaOverlapSqrt", log.y=F)

TransformTriPlot(rtes$occupy.time.marker, rtes$alpha, high.alpha.index, xlab="occupy Day", ylab="alpha", transform="log", fname="AlphaOccTime", log.y=F)
TransformTriPlot(rtes$occupy.time.marker, rtes$alpha, high.alpha.index, xlab="occupy Day", ylab="alpha", transform="sqrt", fname="AlphaOccTimeSqrt", log.y=F)

TransformTriPlot(rtes$occ.peak.time.marker, rtes$alpha, high.alpha.index, xlab="occupy Peak Day", ylab="alpha", transform="log", fname="AlphaOccPeakTime", log.y=F)
TransformTriPlot(rtes$occ.peak.time.marker, rtes$alpha, high.alpha.index, xlab="occupy Peak Day", ylab="alpha", transform="sqrt", fname="AlphaOccPeakTimeSqrt", log.y=F)

######################################
# FOLLOWER DELTA
#

plot(log10(rtes$follow.prop.closeness), log10(rtes$follower.win.delta))
hist(rtes$follower.win.delta)
plot(table(rtes$follower.win.delta))

follow.delta <- rtes$follower.win.delta + 1 + abs(min(rtes$follower.win.delta))
summary(follow.delta)
TransformTriPlot(rtes$alpha, follow.delta, high.alpha.index, xlab="closeness", ylab="follower.win.delta", transform="log", fname="followClose", log.y=T)
TransformTriPlot(rtes$follow.prop.closeness, follow.delta, high.alpha.index, xlab="closeness", ylab="follower.win.delta", transform="log", fname="followClose", log.y=T)
TransformTriPlot(rtes$size, follow.delta, high.alpha.index, xlab="size", ylab="follower.win.delta", transform="log", fname="followSize", log.y=T)
TransformTriPlot(rtes$window.size, follow.delta, high.alpha.index, xlab="window size", ylab="follower.win.delta", transform="log", fname="followWinSize", log.y=T)
TransformTriPlot(rtes$peak.at + 1, follow.delta, high.alpha.index, xlab="peak time", ylab="follower.win.delta", transform="log", fname="followPeakAt", log.y=T)
TransformTriPlot(rtes$peak.at + 1, follow.delta, high.alpha.index, xlab="peak time", ylab="follower.win.delta", transform="sqrt", fname="followPeakAtSqrt", log.y=T)
TransformTriPlot(rtes$peak.rate, follow.delta, high.alpha.index, xlab="peak rate", ylab="follower.win.delta", transform="log", fname="followPeakRate", log.y=T)

TransformTriPlot(rtes$occ.users + 2, follow.delta, high.alpha.index, xlab="occupy users", ylab="follower.win.delta", transform="log", fname="followOccUsers", log.y=T)
TransformTriPlot(rtes$occ.users + 1, follow.delta, high.alpha.index, xlab="occupy users", ylab="follower.win.delta", transform="sqrt", fname="followOccUsersSqrt", log.y=T)
TransformTriPlot(rtes$follower.mean, follow.delta, high.alpha.index, xlab="mean followers", ylab="follower.win.delta", transform="log", fname="followMeanFollow", log.y=T)

TransformTriPlot(rtes$tweet.overlap + 1, follow.delta, high.alpha.index, xlab="tweet.overlap", ylab="follower.win.delta", transform="log", fname="followOverlap", log.y=T)
TransformTriPlot(rtes$tweet.overlap + 1, follow.delta, high.alpha.index, xlab="tweet.overlap", ylab="follower.win.delta", transform="sqrt", fname="followOverlapSqrt", log.y=T)

TransformTriPlot(rtes$occupy.time.marker, follow.delta, high.alpha.index, xlab="occupy Day", ylab="follower.win.delta", transform="log", fname="followOccTime", log.y=T)
TransformTriPlot(rtes$occupy.time.marker, follow.delta, high.alpha.index, xlab="occupy Day", ylab="follower.win.delta", transform="sqrt", fname="followOccTimeSqrt", log.y=T)

TransformTriPlot(rtes$occ.peak.time.marker, follow.delta, high.alpha.index, xlab="occupy Peak Day", ylab="follower.win.delta", transform="log", fname="followOccPeakTime", log.y=T)
TransformTriPlot(rtes$occ.peak.time.marker, follow.delta, high.alpha.index, xlab="occupy Peak Day", ylab="follower.win.delta", transform="sqrt", fname="followOccPeakTimeSqrt", log.y=T)




#####################################################



summary(fit)
fit.coef <- fit$coefficients
fit.ci <- confint(fit)
ci.low <- fit.ci[,1]
ci.hi <- fit.ci[,2]
conf.int.df <- cbind(ci.low, fit.coef, ci.hi)

var.index <- 6
x.min <- min(conf.int.df[var.index,])
x.max <- max(conf.int.df[var.index,])
ci.x <- c(conf.int.df[var.index,1], conf.int.df[var.index,3], conf.int.df[var.index,3], conf.int.df[var.index,1])
ci.y <- c(1,1,2,2)
coeff.x <- c(conf.int.df[var.index,2], conf.int.df[var.index,2])
coeff.y <- c(.9,2.1)
x <- seq(from=x.min - (x.min * .25) , to=x.max + (x.max * .25), length=10)
y <- seq(from=.5, to=2.5, length=10)
par(mar=c(3,8,1,1))
plot(x, y, type="n", bty="n", main="", ylab="", yaxt="n", xlab="")
polygon(ci.x, ci.y, col="orange", border="orange")
lines(coeff.x, coeff.y, lwd=2)
mtext(text=names(fit.coef[var.index]), side=2, las=1, line=0)


av.Plots(fit)

influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

qqPlot(fit.1, main="QQ Plot")
fit.out <- influence(fit.1)
qqnorml(fit.out$coef[,4])
halfnorm(cooks.distance(fit.1), 7)
prplot(fit.1,1)
prplot(fit.1,2)
prplot(fit.1,3)
prplot(fit.1,4)
prplot(fit.1,5)
prplot(fit.1,6)
prplot(fit.1,7)






fit <- lm(follow.delta^.19 ~ alpha + peak.rate.L + peak.at.L + follower.start.L + mean.followers.L + overlap.sqrt + poly(occ.peak.time.marker,2), data=rtes)

hist(follow.delta)
hist(rtes$alpha)
hist(rtes$peak.rate)
hist(rtes$peak.at)
hist(rtes$follower.start)
plot(sort(rtes$follower.start))
hist(rtes$follower.mean)

x <- 1:length(rtes$follower.mean)
y <- sort(rtes$follower.mean)
y <- sort(rtes$peak.rate)
plot(x, y)
plot(log10(x), log10(y))


# "rte.ids"                "origin.user"            "num.bots"              
# "occ.users"              "start.date"             "occupy.time.marker"    
# "follower.start"         "follower.delta"         "follower.percent.delta"
# "follower.sum"           "follower.mean"          "size"                  
# "window.size"            "first.rt.lag"           "last.rt.at"            
# "peak.at"                "count.pre.peak"         "peak.rate"             
# "post.peak"              "post.window"            "occ.start.minute"      
# "occ.end.minute"         "alpha"                  "KS.p"                  
# "poission.p"             "wait.closeness"         "follow.prop.closeness" 
# "follow.prob.closeness"  "post.peak.closeness"    "longest.path"          
# "chains"                 "tweet.overlap"          "tweet.overlap.minutes" 
# "origin.user.lower"      "Nov15" "Nov17"


cor.test(rtes$follow.prop.closeness, rtes$follower.mean)


occ.user.table <- sort(table(rtes$origin.user))
occ.user.names <- names(occ.user.table)
occ.user.length <- length(occ.user.names)

for (i in 1:occ.user.length) {
  
  
}

cor.test(rtes$size, rtes$follower.delta)
cor.test(rtes$peak.rate, rtes$follower.delta)
cor.test(rtes$peak.rate, rtes$size)
cor.test(rtes$peak.rate, rtes$follow.prop.closeness)
cor.test(rtes$peak.rate, rtes$follower.mean)
cor.test(rtes$follower.mean, rtes$follow.prop.closeness)

cor.test(rtes$follower.win.delta, rtes$follow.prop.closeness)
plot(log10(rtes$follower.win.delta + 20), log10(rtes$follow.prop.closeness + 1))



colnames(rtes)
closeness <- round(rtes$follow.prop.closeness, 2)
hist(closeness)
summary(rtes$follow.prop.closeness)
plot(sort(rtes$follow.prop.closeness))

blue <- rgb(80, 165, 255, maxColorValue=255)
hist(rtes$size, breaks=20, col=blue, border=blue, main="Histogram of RTE sizes", xlab="size (number of tetweets)")



colnames(rtes)


followers


