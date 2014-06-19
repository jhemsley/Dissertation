
options(scipen=99)
include.files.dir <- log.file.path <- "c:/r/aDiss/processRTEs/"
# contains global variable settings
process.rtes.config.file.name <- "processRTEs_CONFIG_v0.R"
source(paste(include.files.dir, process.rtes.config.file.name, sep=""))

library(car)
library(faraway)
library(MASS)

regression.plot.dir <- "C:/r/aDiss/descriptivePlots/"
track.words.file <- "c:/r/aDiss/processRTEs/tracking.v8.R"
source(track.words.file) # we get the list of occupy users from this
track.8.words <- tolower(track.8.words)
length(track.8.words)


rdata.file <- "C:/r/aDiss/processRTEs/ConstructedRTEdata_201405151457.Rdata"
rdata.file <- "C:/r/aDiss/processRTEs/ConstructedRTEdata_201405201715.Rdata"
load(rdata.file)
dim(rte.meta.df)
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
row.names(rtes) <- 1:nrow(rtes)


# # # #
# high alphas
high.alpha.index <- which(rtes$alpha > 2.3)
high.alpha.index <- intersect(which(rtes$occupy.time.marker > 150), high.alpha.index)
length(high.alpha.index)
sort(rtes$start.date[high.alpha.index])
round(as.numeric((difftime(rtes$start.date[high.alpha.index], OWS.start.date, units = "days"))),0)
sort(rtes$start.date[rtes$occupy.time.marker > 150])
rtes$start.date[rtes$occ.peak.time.marker == 150]


# # # #
# broken stick occupytime functions
#occ.time.lh <- occ.time.rh <- rtes$occupy.time.marker
occ.time.lh <- occ.time.rh <- rtes$occ.peak.time.marker
occ.time.lh[-high.alpha.index] <- 0
occ.time.rh[high.alpha.index] <- 0
#1 - 80, 81 - 150, 150 up
octm.1 <- ifelse(rtes$occ.peak.time.marker > 80, 0, rtes$occ.peak.time.marker)
octm.2 <- rtes$occ.peak.time.marker
octm.2[octm.2 < 81] <- 0
octm.2[octm.2 > 149] <- 0
octm.3 <- ifelse(rtes$occ.peak.time.marker < 150, 0, rtes$occ.peak.time.marker)
occ.t.lh <- ifelse(rtes$occ.peak.time.marker < 150, rtes$occ.peak.time.marker, 0)
occ.t.rh <- ifelse(rtes$occ.peak.time.marker < 150, 0, rtes$occ.peak.time.marker)


f.delta.all <- rtes$follower.delta
f.delta.all.L <- log10(f.delta.all)
follow.delta <- rtes$follower.win.delta + 1 + abs(min(rtes$follower.win.delta))
follow.delta.L <- log10(follow.delta)
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
occ.peak.time <- log10(rtes$occ.peak.time.marker)
overlap.sqrt <- sqrt(rtes$tweet.overlap)
overlap.L <- log10(rtes$tweet.overlap + 2)


#########################################################
# vector for high alpha colors 
# PLOTTING
#########################################################
tmp.col <- rep("black", dim(rtes)[1])
tmp.col[high.alpha.index] <- "red"
late.high.alphas <- rep(0, dim(rtes)[1])
late.high.alphas[high.alpha.index] <- 1

summary(rtes$follower.win.delta)
summary(rtes$follower.delta)

TransformTriPlot(rtes$alpha, f.delta.all, high.alpha.index, xlab="alpha", ylab="followers", transform="log", fname="followClose", log.y=T, plot.file=F)


#########################################################
#
# REGRESSION:: ALPHA AS DEPENDENT VAR
# Original: lm(formula = alpha.L
#            ~ follow.prop.closeness + size.L + peak.at.L  
#            + Nov15 + occ.users.L + high.alphas
#            + occupy.time.marker.cube, data = rte.df)
#
#########################################################

fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + peak.at.L + mean.followers.L + overlap.sqrt + occ.t.lh + occ.t.rh, data=rtes)
fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + peak.at.L + mean.followers.L + overlap.sqrt + occ.t.lh + occ.t.rh, data=rtes)
fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + peak.at.L + mean.followers.L + overlap.sqrt + octm.1 + octm.2 + octm.3, data=rtes)
fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + mean.followers.L + overlap.sqrt + octm.1 + octm.2 + octm.3, data=rtes)
fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + peak.at.L + mean.followers.L + overlap.sqrt + poly(occ.peak.time.marker,2) + Nov15 + Nov17, data=rtes)
fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + peak.at.L + mean.followers.L + overlap.sqrt + occ.first.80 + occ.after.80, data=rtes)



fit <- lm(follow.delta^.2 ~ alpha + peak.rate.L + peak.at.L + mean.followers.L + overlap.sqrt + poly(occ.peak.time.marker,2), data=rtes)


follower.start.L <- log10(rtes$follower.start)


fit <- lm(follow.delta^.19 ~ alpha + peak.rate.L + peak.at.L + follower.start.L + mean.followers.L + overlap.sqrt + poly(occ.peak.time.marker,2), data=rtes)

summary(fit)
par(mfrow=c(2,2))
plot(fit)
mtext(text=fit$call, side=3, line=2, cex=.9, col="red", adj=1)

##############
#
# 3 diagnostic plots
#
plot.file.name.png <- paste(regression.plot.dir, "RegressionDiagPlots_Followers.png", sep="")
#png(plot.file.name.png, width=625, height=350, pointsize=16)
plot.file.name.pdf <- paste(regression.plot.dir, "RegressionDiagPlots_Followers.pdf", sep="")
pdf(plot.file.name.pdf, width=6.25, height=3.50, pointsize=16, useDingbats = FALSE)
  par(mfrow=c(1,2))
  par(mar=c(4.2,4,2.5,2))
  plot(fitted(fit), residuals(fit), cex=.7, col=rgb(64, 153, 255, maxColorValue=255), xlab="fitted", ylab="residuals", main="Fitted & Residuals")
  # lines(lowess(residuals(fit) ~ fitted(fit)), col="red")
  abline(h=0)
  mtext(text="Model 2", side=1, line=2, adj=-.5)
  qqPlot(fit, main="QQ Plot", lwd=1, cex=.7, col=rgb(64, 153, 255, maxColorValue=255))
dev.off()  

plot.file.name.png <- paste(regression.plot.dir, "RegressionLeveragePlots_Followers.png", sep="")
png(plot.file.name.png, width=625, height=350, pointsize=16)

leveragePlots(fit, layout=c(2,4)) # 625 x 330 pixles
dev.off()  


##############
#
# Model diagnostics
#

vifit <- vif(fit) # variance inflation factors. Independent vars significantly correlated. 
data.frame(names(vifit), vifit)
sqrt(vifit) > 2 # Multicoliniarity (independents). problem?
durbinWatsonTest(fit) # autocorrelation (residuals): p < 0.05 residuals significantly correlated, else p > 0.05 no evidence of correlation.


boxcox(fit, lambda=seq(0,1,by=.01), plotit=T)
confint(fit)

outlierTest(fit) # Bonferonni p-value for most extreme obs

sresid <- studres(fit)
hist(sresid, freq=FALSE, main="Distribution of\nStudentized Residuals", col="orange")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit, lwd=2) 




fit.influence <- influence(fit)
sum(fit.influence$hat) # should equal the number of y and x vars in the model
summary(fit.influence)
