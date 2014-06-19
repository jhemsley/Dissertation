#########################################################
#
# Author: Jeff Hemsley
# 
# Purpose: code for my disertation
# Description: This file is intended aas a highlevel file
#              for processing raw retweet data to make 
#              retweet event (RTE) data.
#
# Issues:
# add distribution plots at end
# add occupy time delta
# think about changing how you do window to be more like it was
# think about outliers
#

options(scipen=99)
include.files.dir <- log.file.path <- "c:/r/aDiss/processRTEs/"
plot.rtes.dir <- "C:/r/aDiss/processRTEs/RTE_Plots/"
plot.rtes.dir <- "C:/r/aDiss/descriptivePlots/allRTEs/"
plot.rtes.dir <- "C:/r/aDiss/descriptivePlots/RTEplots/"
#plot.rtes.dir <- "C:/r/aDiss/descriptivePlots/rawSigs/"
all.tweets.dir <- "C:/r/aDiss/processRTEs/"

#http://www.r-bloggers.com/gini-index-and-lorenz-curve-with-r/
install.packages("ineq")
library(ineq)
ineq(,type="Gini")


# contains global variable settings
process.rtes.config.file.name <- "processRTEs_CONFIG_v0.R"
source(paste(include.files.dir, process.rtes.config.file.name, sep=""))


# contains functions to build RTE cascade tress
data.functions.file.name <- "data_functions_v1.R"
source(paste(include.files.dir, data.functions.file.name, sep=""))

# contains functions to build RTE cascade tress
tree.builder.functions.file.name <- "tree_builder_functions_v4.R"
source(paste(include.files.dir, tree.builder.functions.file.name, sep=""))


# the following is the wait time distribution of retweets in the raw data file
sim.param.data.dir <- "c:/r/aDiss/simulation/simParameterData/"
SimParm.WaitTimes.file.name <- "WaitTimeDist_seconds984544.csv"
SimParm.WaitTimes <- readSimParamWaitTimes (sim.param.data.dir, SimParm.WaitTimes.file.name, log=TRUE)

bot.account.list.file <- paste(include.files.dir, "OccupyBotAccountList.txt", sep="") 
bot.account.list <- read.delim(file=bot.account.list.file, stringsAsFactors=F, row.names=NULL)
bot.account.list <- as.vector(bot.account.list[,1])



all.tweets.rdata.file <- paste(all.tweets.dir, "all.tweets.Rdata", sep="")
load(all.tweets.rdata.file)

#max.follower.proportion <- max(all.tweets$follower.proportion)
  

#####################################################################
#
# ONLY INCLUDE TWEETS THAT ORIGINATED FROM AN OCCUPY ACCOUNT
#
#####################################################################

use.occupy.tweets.only <- FALSE
if (use.occupy.tweets.only) {
  # alt.names <- unique(rt.dat$rt_screen_name[grep("arrests", rt.dat$rt_screen_name, ignore.case = TRUE)])
  # "AnonymousPress" "AnonymousIRC"   "AnonymousWiki"  "anonymouSabu"
  # "BankTransferDay" "gtbank"          "MrBankole"       "BankyW"
  # "OccupyArrests"
  occupy.origin.index <- grep("(OWS|Occupy|BankTransferDay)", all.tweets$rt_screen_name, ignore.case = TRUE)
  length(occupy.origin.index)
  occupy.originators <- sort(unique(all.tweets$rt_screen_name[occupy.origin.index]))
  length(occupy.originators)
  
  occ.users.only <- all.tweets[occupy.origin.index,]
  #all.tweets <- all.tweets[occupy.origin.index,]
}

dim(all.tweets)
#toss.df <- all.tweets


#####################################################################
#
# NOW WE HAVE A SET OF RTES, SOME OF WHICH WILL BE TOO SMALL TO USE
# SO ZAP THOSE AND FIND THE FINAL LIST OF USEABLE RTES. NOTE THAT TO 
# FIND A POWERLAW WE NEED AT LEAST 100 DATA POINTS. THIS WORKS OUT
# A BIT WEIRD SINCE THAT MEANS 100 X VALUES, AND WE KNOW OUR WINDOW 
# GIVES US THAT MUCH. BUT IF A RTE HAS FEWER THAN 100 VALUES, WE CUT 
# IT OFF ANYWAY. NEED TO COME UP WITH A JUSTIFICATION OTHER THAN THAT
# I THINK IT ISN'T ENOUGH.
#
#####################################################################
rte.id.table <- sort(table(all.tweets$find_rt_id), decreasing=T)
plot(rte.id.table, type ="h", bty="n", main="Sizes of RTEs", col="brown", ylab="size")
length(rte.id.table)

dim(all.tweets)

rte.ids <- as.numeric(names(rte.id.table))
num.rtes <- length(rte.ids)
num.rtes
#colnames(all.tweets)
# "find_rt_id", "id", "created_ts", "screen_name", "followers"
# , "rt_id", "rt_created_ts", "rt_screen_name", "rt_followers"
# , "time.delta", "follower.proportion"

rte.id.table[1:10]
rte.ids[1:10]

#####################################################################
#####################################################################
#
# HERE IS A FIRST PASS TO LOCATE THE PEAK
#
#####################################################################
#####################################################################

gc()
rte.meta.df <- data.frame(rte.ids=rte.ids)
rte.meta.df$origin.user <- NA
rte.meta.df$num.bots  <- NA
rte.meta.df$occ.users <- NA
rte.meta.df$start.date <- OWS.start.date
rte.meta.df$occupy.time.marker <- NA
rte.meta.df$occ.peak.time.marker <- NA
rte.meta.df$follower.start <- NA
rte.meta.df$follower.delta <- NA
rte.meta.df$follower.win.delta <- NA
rte.meta.df$follower.percent.delta <- NA
rte.meta.df$follower.sum <- NA
rte.meta.df$follower.mean <- NA
# signature data
rte.meta.df$size <- 0
rte.meta.df$window.size <- 0
rte.meta.df$first.rt.lag <- 0
rte.meta.df$last.rt.at <- 0
rte.meta.df$peak.at <- 0
rte.meta.df$count.pre.peak <- 0
rte.meta.df$peak.rate <- 0
rte.meta.df$post.peak <- 0
rte.meta.df$post.window <- 0
rte.meta.df$occ.start.minute <- 0
rte.meta.df$occ.end.minute <- 0
rte.meta.df$alpha <- NA
rte.meta.df$KS.p <- NA
#rte.meta.df$max.vol <- NA
#rte.meta.df$pre.peak.size <- NA
#rte.meta.df$post.peak.size <- NA
rte.meta.df$poission.p <- NA
# cascade data
rte.meta.df$wait.closeness <- NA
rte.meta.df$follow.prop.closeness <- NA
rte.meta.df$follow.prob.closeness <- NA
rte.meta.df$post.peak.closeness <- NA
rte.meta.df$longest.path <- NA
rte.meta.df$chains <- NA
rte.meta.df$gini <- NA

for (i in 1:num.rtes) {
  
  # i <- 1
  #i <- 819
  rte.id <- rte.ids[i]
  rte.index <- which(all.tweets$find_rt_id == rte.id)
  rte <- all.tweets[rte.index, ]
  #colnames(rte)
  #rte[1,]
  
  rte.meta.df$origin.user[i] <- rte$screen_name[1]
  # rte[1:3,c(3,4,6,7,8,11)]
  
  rte.wait.table <- table(round(rte$time.minutes,0))
  abs.max.index <- which.max(as.numeric(rte.wait.table))
  max.at <- as.numeric(names(rte.wait.table)[abs.max.index])
  #summary(rte$time.minutes)
  rts.before.peak.index <- which(rte$time.minutes < max.at)
  rts.after.window.index <- which(rte$time.minutes > (max.at + 121))
  out.of.window.index <- union(rts.before.peak.index, rts.after.window.index)
  in.window.index <- intersect(which(rte$time.minutes >= max.at), which(rte$time.minutes <= (max.at + 120)))
  
  #rte$time.minutes[rts.after.window.index]
  #rte$rt_followers[rts.after.window.index]
  
  rte.meta.df$size[i] <- dim(rte)[1]
  rte.meta.df$window.size[i] <- length(in.window.index)
  rte.meta.df$first.rt.lag[i] <- rte$time.minutes[2] - rte$time.minutes[1]
  rte.meta.df$last.rt.at[i] <- max(rte$time.minutes)
  rte.meta.df$peak.at[i] <- max.at
  rte.meta.df$count.pre.peak[i] <- length(rts.before.peak.index)
  rte.meta.df$peak.rate[i] <- as.numeric(rte.wait.table[abs.max.index])  
  rte.meta.df$post.peak[i] <- length(which(rte$time.minutes > max.at))
  rte.meta.df$post.window[i] <- length(rts.after.window.index)
  rte.meta.df$occ.start.minute[i] <- rte$occupy.time.minutes[1]
  rte.meta.df$occ.end.minute[i] <- rte$occupy.time.minutes[1] + max.at + 121
  
  followers.at.start <- rte$followers[1]
  followers.at.end <- rte$rt_followers[dim(rte)[1]]
  follower.delta <- followers.at.end - followers.at.start
  follower.percent.delta <- follower.delta/followers.at.start
  
  tmp.followers <- rte$rt_followers[in.window.index]
  tmp.followers <- tmp.followers[!is.na(tmp.followers)]
  rte.meta.df$follower.win.delta[i] <- tmp.followers[length(tmp.followers)] - tmp.followers[1]
  
  #rte.meta.df$origin.user[i] <- rte$screen_name[1]
  rte.meta.df$num.bots[i] <- length(which(rte$screen_name %in% bot.account.list))
  rte.meta.df$occ.users[i] <- length(grep("(OWS|Occupy)", rte$screen_name, ignore.case = FALSE)) - 1
  rte.meta.df$start.date[i] <- rte$created_ts[1]
  rte.meta.df$occupy.time.marker[i] <- rte$occupy.time.marker[1]
  rte.meta.df$occ.peak.time.marker[i] <- rte$occupy.time.marker[1] + max.at - 1
  rte.meta.df$follower.start[i] <- followers.at.start
  rte.meta.df$follower.delta[i] <- follower.delta
  rte.meta.df$follower.percent.delta[i] <- follower.percent.delta
  rte.meta.df$follower.sum[i] <- sum(rte$followers)
  rte.meta.df$follower.mean[i] <- mean(rte$followers)
  
  rm(rte.index, rte.wait.table, tmp.followers, out.of.window.index, rts.before.peak.index, rts.after.window.index, abs.max.index, max.at, followers.at.start, followers.at.end, follower.delta, follower.percent.delta)
}
plot(rte.meta.df$follower.win.delta, rte.meta.df$follower.delta)
summary(rte.meta.df$follower.win.delta)
summary(rte.meta.df$follower.delta)
summary(rte.meta.df$peak.rate)

rte.meta.df[819,]
which(is.na(rte.meta.df$follower.win.delta))

rte.meta.df$size[1:10]
rm(rte.id.table)
rte.meta.df.bak <- rte.meta.df

#plot(rte.meta.df$occ.peak.time.marker - rte.meta.df$occupy.time.marker)

cat("rte data: ", dim(rte.meta.df)[1], "\n")
quantile(rte.meta.df$first.rt.lag, p = c(.75, .80, .85, .90, .95, .97, .99))
quantile(rte.meta.df$peak.at, p = c(.75, .80, .85, .90, .95, .97, .99))
quantile(rte.meta.df$last.rt.at, p = c(.50, .75, .80, .85, .90,.95))
dim(rte.meta.df)
plot(rte.meta.df$peak.at, rte.meta.df$last.rt.at)

plot.set.name.plots <- paste(plot.rtes.dir, "AllRTE_durationsInMinutes.png", sep="")
png(plot.set.name.plots, width=600, height=300)
#plot(log10(sort(rte.meta.df$last.rt.at)), bty="n", pch=".", ylab="minutes", main="Time from initial tweet\nto last retweet")
hist(rte.meta.df$last.rt.at, col="orange", xlab="Minutes", main="Time from initial tweet\nto last retweet")
dev.off()

plot.set.name.plots <- paste(plot.rtes.dir, "AllRTE_PeakRates.png", sep="")
png(plot.set.name.plots, width=600, height=300)
#plot(sort(log10(rte.meta.df$peak.rate)), bty="n", pch=".", ylab="minutes", main="Time from initial tweet\nto last retweet")
hist(log10(rte.meta.df$peak.rate), col="orange", xlab="Log10 Peak Rates", main="RTE peak rate")
dev.off()

plot.set.name.plots <- paste(plot.rtes.dir, "AllRTE_PeakAt.png", sep="")
png(plot.set.name.plots, width=600, height=300)
#plot(sort(log10(rte.meta.df$peak.at)), bty="n", pch=".", ylab="minutes", main="Time from initial tweet\nto last retweet")
hist(log10(rte.meta.df$peak.at), col="orange", xlab="Log10 Peak at in minutes", main="RTE peak time")
dev.off()

plot.set.name.plots <- paste(plot.rtes.dir, "AllRTE_FirstRT_LagTime.png", sep="")
png(plot.set.name.plots, width=600, height=300)
#plot(sort(rte.meta.df$first.rt.lag), bty="n", pch=".", ylab="minutes", main="Time from initial tweet\nto last retweet")
hist(log10(rte.meta.df$first.rt.lag), col="orange", xlab="Log10 minutes", main="RTE delay in minutes to first RT")
dev.off()

o <- order(rte.meta.df$size, decreasing=T)
rte.meta.df <- rte.meta.df[o,]
rm(o)


plot.set.name.plots <- paste(plot.rtes.dir, "AllRTE_StartStopOccupyMinutes.png", sep="")
png(plot.set.name.plots, width=600, height=600)

x <- seq(from=min(rte.meta.df$occ.start.minute), to=max(rte.meta.df$occ.end.minute), length.out=length(rte.meta.df$occ.start.minute))
y <- 1:length(rte.meta.df$occ.start.minute)
plot(x, y, bty="n", type="n", ylab="RTE Index", xlab="minutes (from start of Occupy)")
mtext(text="Start and end times of RTEs\nin minutes from start of Occupy", side=3, cex=1.2)

for (i in 1:length(rte.meta.df$occ.start.minute)) {
  # i <- 1
  x <- c(rte.meta.df$occ.start.minute[i], rte.meta.df$occ.end.minute[i])
  y <- c(i, i)
  lines(x,y)
}
dev.off()
rm(x, y)
rm(i)
#long.first.rt.lag.index <- which(rte.meta.df$first.rt.lag > 15) # 15 minute lag
#small.peak.index <- which(rte.meta.df$peak.rate < 10)

long.first.rt.lag.index <- which(rte.meta.df$first.rt.lag > 60 * 24 * 3) # 3 day lag
small.peak.index <- which(rte.meta.df$peak.rate < 10) # no peak

unuseable.rte.index <- union(long.first.rt.lag.index, small.peak.index)
rte.meta.df <- rte.meta.df[-unuseable.rte.index,]
cat("rte data after long lag (", length(long.first.rt.lag.index), ") and no peak (", length(small.peak.index), "): ", dim(rte.meta.df)[1], "\n", sep="")

#
super.late.peaks <- which(rte.meta.df$peak.at > 120)
num.super.late.peaks <- length(super.late.peaks)
for (i in 1:num.super.late.peaks) {

  
  # i <- 1 + i
  rte.id <- rte.meta.df$rte.ids[super.late.peaks[i]]
  rte.index <- which(all.tweets$find_rt_id == rte.id)
  rte <- all.tweets[rte.index, ]
    
  tweet.minutes.table <- table(rte$time.minutes)
  start.date <- rte$created_ts[1]
  
  tweet.minutes.x <- as.numeric(names(tweet.minutes.table))
  tweet.dates.x <- rep(as.POSIXlt(start.date), length(tweet.minutes.x))
  tweet.dates.x$min <- tweet.dates.x$min + tweet.minutes.x
  tweet.dates.y <- as.numeric(tweet.minutes.table)
  start.peak.dates <- tweet.dates.x[c(1,which.max(tweet.dates.y))]
  x.values <- seq(from=tweet.dates.x[1], max(tweet.dates.x), length.out=10)
  y.values <- as.integer(round(seq(from=0, max(tweet.dates.y), length.out=10),0))
  
  main <- paste(rte$screen_name[1], "size:", dim(rte)[1], "\nID", rte.id
    , "\nstart:", start.peak.dates[1], "\npeak:", rte.meta.df$peak.at[super.late.peaks[i]], "min later")
  
  plot.set.name.plots <- paste(plot.rtes.dir, "LatePeakRTE_", rte$screen_name[1], "_", rte.id, ".png", sep="")

  png(plot.set.name.plots, width=600, height=300)
  plot(x.values, y.values, type="n", bty="n", col=rgb(179, 0, 0, maxColorValue=255)
    , xlab="time", ylab="retweets")
  lines(tweet.dates.x, tweet.dates.y, type="h", lwd=2, col=rgb(179, 0, 0, maxColorValue=255))
  mtext(text=main, side=3, line=-2, cex=1.2)
  dev.off()
}

# remove late peaks?
#rte.meta.df <- rte.meta.df[-super.late.peaks,]
cat("rte data after late peaks (", length(super.late.peaks), "): ", dim(rte.meta.df)[1], "\n", sep="")

quantile(rte.meta.df$first.rt.lag, p = c(.75, .80, .85, .90, .95, .97, .99))
quantile(rte.meta.df$peak.at, p = c(.75, .80, .85, .90, .95, .97, .99))
quantile(rte.meta.df$last.rt.at, p = c(.50, .75, .80, .85, .90,.95))
rm(long.first.rt.lag.index, small.peak.index, unuseable.rte.index, num.super.late.peaks, super.late.peaks)
rm(rte.id, rte.index, rte, tweet.minutes.table, start.date
  , tweet.minutes.x, tweet.dates.x, tweet.dates.y, start.peak.dates
  , x.values, y.values, main, plot.set.name.plots)
#####################################################################
#####################################################################
#
# FIND OVERLAP IN TWEETS BY THE SAME USER.
#
#####################################################################
#####################################################################

gc()

unique.users.table <- table(rte.meta.df$origin.user)
unique.users <- names(unique.users.table)
unique.users.index <- which(as.numeric(unique.users.table) > 1)
unique.users <- unique.users[unique.users.index]
num.overlap.check <- length(unique.users)

rte.meta.df$tweet.overlap <- 0
rte.meta.df$tweet.overlap.minutes <- 0

for (i in 1:num.overlap.check) {
  
  # i <- 1 + 1
  dood <- unique.users[i]
  
  doods.tweets.index <- which(rte.meta.df$origin.user == dood)
  num.doods.tweets <- length(doods.tweets.index)
  
  doods.tweets <- rte.meta.df[doods.tweets.index, c("occ.start.minute", "occ.end.minute")]
  #cat(dood, doods.tweets[,2] - doods.tweets[,1], "\n")
  
  for (j in 1:num.doods.tweets) {
    # j <- 1
    tweet.overlap.minutes <- 0
    tweet.overlap <- 0
    
    tweet.1.range <- doods.tweets[(j),1]:doods.tweets[(j),2]
    for (k in 1:num.doods.tweets) {
      # k <- 1 + k
      if (j != k) {
        tweet.2.range <- doods.tweets[(k),1]:doods.tweets[(k),2]
        overlap.minutes <- intersect(tweet.1.range, tweet.2.range)
        if (length(overlap.minutes) > 0) {
          tweet.overlap <- tweet.overlap + 1
          tweet.overlap.minutes <- tweet.overlap.minutes + length(overlap.minutes)
        }
      }
    }
    
    rte.meta.df$tweet.overlap[doods.tweets.index[j]] <- tweet.overlap
    rte.meta.df$tweet.overlap.minutes[doods.tweets.index[j]] <- sum(tweet.overlap.minutes)
  }
}

rm(dood, doods.tweets.index, num.doods.tweets, doods.tweets, tweet.overlap.minutes, tweet.overlap, tweet.1.range, tweet.2.range, overlap.minutes, i, j, k)
gc()
rte.meta.df[which.max(rte.meta.df$tweet.overlap),]
plot(sort(rte.meta.df$tweet.overlap))
plot(rte.meta.df$tweet.overlap.minutes)
rm(unique.users.table, num.overlap.check, unique.users.index, unique.users)



#####################################################################
#####################################################################
#
# HERE I GO ABOUT ANALYZING THE RTES TO GET A SIGNATURE AND INFER THE
# CASCADE TREE FOR EACH. I ALSO CALCULATE OTHER ATTRIBUTES AND SAVE 
# ALL THIS OFF AS RTE META DATA. 
#
#####################################################################
#####################################################################
gc()




#rte.meta.df <- createRTEmetaDataObject(num.rtes)
#rte.meta.df <- rte.metadata.df
dim(rte.meta.df)
num.rtes <- dim(rte.meta.df)[1]
dim(all.tweets)

wait.time.alpha = 2

for (i in 1:num.rtes) {
  # i <- 1 + i + 30
  # i <- 3367 - 2
  
  rte.id <- rte.meta.df$rte.ids[i]
  # rte.id <- rustyrockets.rte.id
    
  rte.index <- which(all.tweets$find_rt_id == rte.id)
  rte <- all.tweets[rte.index, ]
    
  cat(i, ": ", rte$screen_name[1], ", RTE ID: ", rte.id, ", event size ", dim(rte)[1], "\n",  sep="")
  
  #colnames(rte.meta.df)
  cat("... signature")
  signature <- createSignatureObject (rte, y.bump=1, window.size.in.minutes = 120)
  if (is.null(signature)) {
    cat("\n  --> WARNING: Unable to make a sginature for this item.\n")
    next()
  }
  
  
  
  # note that this will not match: rte.meta.df$peak.at[i] == signature$peak.at) 
  # the sig is designed to plot but the meta data is in minutes
  rte.meta.df$alpha[i] <- signature$alpha
  rte.meta.df$KS.p[i] <- signature$KS.p
  rte.meta.df$poission.p[i] <- signature$poission.p
    
  out.of.window.index <- which(rte$time.minutes > signature$adjusted.max.minutes)
  if (length(out.of.window.index) > 0) {
    rte <- rte[-out.of.window.index,]
    cat(" (removing ", length(out.of.window.index), ") ")
    #max(rte$time.delta)
  }
  
  rte.meta.df$window.size[i] <- dim(rte)[1]
  
  #cat("MAX MIN: ", max(rte$time.delta), "\n")
  #plot(g.wait.time)
  #g.wait.time <- createWaitTimeTreeGraph(rte, wait.time.alpha = wait.time.alpha)
  #rte.meta.df$wait.closeness[i] <- g.wait.time$closeness.out
    
  #is.connected(g.wait.time$g)
  cat(" g.combined.follower.proportion... ")
  g.combined.follower.proportion <- createCombinedTreeGraph (rte, use.follower.proportion = TRUE, wait.time.alpha = wait.time.alpha)
  rte.meta.df$follow.prop.closeness[i] <- g.combined.follower.proportion$closeness.out
  cat(". Plots:\n")
  
  #g.combined.follower.probability <- createCombinedTreeGraph (rte, use.follower.proportion = FALSE, wait.time.alpha = wait.time.alpha)
  #rte.meta.df$follow.prob.closeness[i] <- g.combined.follower.probability$closeness.out
  
  tmp.g <- g.combined.follower.proportion$g
  origin.user.name <- V(tmp.g)$name[1]
  
  g.path.length <- shortest.paths(tmp.g, origin.user.name, V(tmp.g)[2:vcount(tmp.g)])
  g.path.length <- g.path.length[is.finite(g.path.length)]
  
  rte.meta.df$longest.path[i] <- max(g.path.length) 
  rte.meta.df$chains[i] = length(which(g.path.length > 1))
  #V(tmp.g)$size <- 2
  #V(tmp.g)$label <- ""
  #plot(tmp.g, edge.arrow.size=0, edge.arrow.width=0)
  tmp.degrees <- degree(tmp.g)
  #, v=V(graph), mode = c("all", "out", "in", "total"),
  #  loops = TRUE, normalized = FALSE
  tmp.gini <- ineq(tmp.degrees,type="Gini")
  rte.meta.df$gini[i] <- tmp.gini
  
  #post.peak.rte <- rte[(rte$time.minutes >= signature$peak.at),]
  #post.peak.g.wait.time <- createWaitTimeTreeGraph(post.peak.rte)
  #post.peak.g <- createCombinedTreeGraph (post.peak.rte, use.follower.proportion = TRUE)
  
  root.file.name <- paste("rte_", round(tmp.gini, 3), "_", rte$screen_name[1], "_", signature$max.vol, "p_", rte.meta.df$size[i], "n_", rte.id, sep="")
  
  sig.plot.file.name <- paste(plot.rtes.dir, root.file.name, "_sig.png", sep="")
  tree.plot.file.name <- paste(plot.rtes.dir, root.file.name, "_tree.png", sep="")
  tree.basic.plot.file.name <- paste(plot.rtes.dir, root.file.name, "_treeBasic.png", sep="")
  time.tree.plot.file.name <- paste(plot.rtes.dir, root.file.name, "_timeTree.png", sep="")
  

  png(sig.plot.file.name, width=600, height=300)
    cat(" - ", sig.plot.file.name, "\n")
    par(mar=c(5.1,4.1,4.1,2.1))
    #plotSignature(signature, rte, tweet.rate.col=rgb(64, 153, 255, maxColorValue=255)
    #, power.law.line.col="black", follower.delta.col=rgb(179, 0, 0, maxColorValue=255))
    plotSignature(signature, rte, tweet.rate.col=rgb(64, 153, 255, maxColorValue=255)
    , power.law.line.col=NULL, follower.delta.col=NULL)
  dev.off()
  
  png(time.tree.plot.file.name, width=600, height=600)
    cat(" - ", time.tree.plot.file.name, "\n")
    par(mar=c(.75,0,2,1))
    NewTreePlot(g.combined.follower.proportion, main="Wait & follower proportions", use.time.axis=TRUE, node.col="lightskyblue")
  dev.off()

  png(tree.plot.file.name, width=600, height=600)
    cat(" - ", tree.plot.file.name, "\n")
    par(mar=c(.75,0,2,1))
    NewTreePlot(g.combined.follower.proportion, main="Wait & follower proportions", use.time.axis=FALSE, node.col="lightskyblue")
  dev.off()
  
  #png(plot.tree.basic.name.plot, width=600, height=600)
  #  par(mar=c(.75,0,2,1))
  #  NewTreePlot(g.wait.time, main="Wait time tree", use.time.axis=TRUE, node.col="lightskyblue")
  #dev.off()
  
  #cat("nets done\n")
  #rte.meta.df[i,] <- metaDataReportLine(rte, g.wait.time, g.combined.follower.proportion, g.combined.follower.probability, post.peak.g)
  #cat(", sig p-val ", round(signature$KS.p, 4), "\n",  sep="")
  rm(rte, signature)
  gc()
}


now <- format(Sys.time(), "%Y%m%d%H%M")
results.df.out.file.name <- paste("ConstructedRTEdata_", now, ".csv", sep="")
results.df.out.file <- paste(all.tweets.dir, results.df.out.file.name, sep="")
write.table(rte.meta.df, file=results.df.out.file, sep=",", append=F)

rte.meta.df <- rte.meta.df[sample(1:dim(rte.meta.df)[1]),]
rdata.file <- paste(all.tweets.dir, "ConstructedRTEdata_", now, ".Rdata", sep="")
save(rte.meta.df, file=rdata.file)





colnames(rte.meta.df)

plot(sort(rte.meta.df$follower.start))
plot(sort(rte.meta.df$size))

### num rtes per user
rtes.by.user <- sort(as.numeric(table(rte.meta.df$origin.user)), decreasing=T)
x <- 1:length(rtes.by.user)
plot(log10(x), log10(rtes.by.user))


y <- sort(rte.meta.df$follower.start, decreasing=T)
x <- 1:length(rte.meta.df$follower.start)
plot(x, y)




colnames(rte.meta.df)

summary(occ.users.only$followers)
million.followers.index <- which(occ.users.only$followers > 1000000)
occ.users.only[million.followers.index, ]

hist(rte.meta.df$follow.prop.closeness)
hist(rte.meta.df$longest.path)
hist(rte.meta.df$chains)




followers.y <- occ.users.only$followers
followers.y <- followers.y[followers.y > 0]
followers.y <- sort(followers.y)
summary(followers.y)
followers.x <- 1:length(followers.y)

plot(followers.x, followers.y)
rustyrockets.rte.id <- occ.users.only$find_rt_id[which(occ.users.only$screen_name == "rustyrockets")]


#rdata.file <- paste(log.file.path, "ConstructedRTEdata_201404300819.Rdata", sep="")

#load(rdata.file)
dim(rte.meta.df)
colnames(rte.meta.df)
rte.meta.df$origin.user

occupy.origin.index <- grep("(OWS|Occupy|BankTransferDay)", rte.meta.df$origin.user, ignore.case = TRUE)
length(occupy.origin.index)

length(unique(rte.meta.df$origin.user[occupy.origin.index]))

sum(rte.meta.df$size[occupy.origin.index])

75000 * 1.1

occupy.originators <- sort(unique(all.tweets$rt_screen_name[occupy.origin.index]))
length(occupy.originators)


colnames(rte.meta.df)
[1] "rte.ids"                "origin.user"            "num.bots"               "occ.users"              "start.date"             "occupy.time.marker"    
[7] "follower.start"         "follower.delta"         "follower.percent.delta" "follower.sum"           "follower.mean"          "size"                  
[13] "first.rt.lag"           "last.rt.at"             "peak.at"                "count.pre.peak"         "peak.rate"              "post.peak"             
[19] "post.window"            "occ.start.minute"       "occ.end.minute"         "alpha"                  "KS.p"                   "poission.p"            
[25] "wait.closeness"         "follow.prop.closeness"  "follow.prob.closeness"  "post.peak.closeness"    "longest.path"           "chains"                
[31] "tweet.overlap"          "tweet.overlap.minutes" 





user.table <- rev(sort(table(rte.meta.df$origin.user)))


for (i in 1:length(user.table)) {
  
  # i <- 1
  user.name <- names(user.table)[i]
  
  YourAnonNews.index <- which(rte.meta.df$origin.user == user.name)
  YourAnonNews.df <- rte.meta.df[YourAnonNews.index,]
    
  if (dim(YourAnonNews.df)[1] < 10) {
    next()
  }
  YourAnonNews.df$real.end.minute <- YourAnonNews.df$occ.start.minute + YourAnonNews.df$last.rt.at
  
  size <- log10(YourAnonNews.df$size)
  #size <- YourAnonNews.df$follower.delta
  x.min <- min(YourAnonNews.df$occ.start.minute)
  x.max <- max(YourAnonNews.df$real.end.minute)
  y.min <- 1
  y.max <- dim(YourAnonNews.df)[1]
  rgb.palette <- colorRampPalette(c("orange", "red"), space = "rgb")
  mypalette <- rgb.palette(max(YourAnonNews.df$tweet.overlap) + 1)
  
  plot.overlap.name <- paste(plot.rtes.dir, "Overlap_", user.name, "_", length(YourAnonNews.index), "tweets.png", sep="")
  png(plot.overlap.name, width=600, height=300)
  
  plot(c(x.min,x.max), c(y.min,y.max), type="n", bty="n"
    , main=paste("Tweet Start and End\nfor ", user.name, sep=""), ylab="users tweet count", xlab="time in minutes from occupy start")
  
  for (i in 1:length(YourAnonNews.df$occ.end.minute)) {
    # i <- 1
    x <- c(YourAnonNews.df$occ.start.minute[i], YourAnonNews.df$real.end.minute[i])
    
    y <- c(i, i)
    #lines(x,y, lwd=3, col=mypalette[YourAnonNews.df$tweet.overlap[i] + 1])
    lines(x,y, lwd=3, col=rgb(1,0,0,.4))
    #points(x[1],y[1], cex=size[i])
  }
  dev.off()
  
  user.index <- which(all.tweets$rt_screen_name == user.name)
  user.df <- all.tweets[user.index,]
  zap.index <- which(is.na(user.df$rt_followers))
  user.df <- user.df[-zap.index,]
  
  plot.user.followers.name <- paste(plot.rtes.dir, "FollowerGrowth_", user.name, "_", dim(user.df)[1], "retweets.png", sep="")
  png(plot.user.followers.name, width=600, height=300)
  
  plot(user.df$created_ts, user.df$rt_followers, pch=".", cex=2, bty="n", xlab="time", ylab="followers", main=paste(user.name, " follower count\nfor each retweet"))
  dev.off()
}



rev(user.table)

OccupyWallSt.index <- which(rte.meta.df$origin.user == "OccupyWallSt")


for (j in 1:length(user.table)) {
  
OccupyWallSt.index <- which(rte.meta.df$origin.user == names(user.table)[j])
OccupyWallSt.rte.ids <- rte.meta.df$rte.ids[OccupyWallSt.index]

for (i in 1:length(OccupyWallSt.rte.ids)) {
  # i <- 1
  rte.id <- OccupyWallSt.rte.ids[i]
  rte.index <- which(all.tweets$find_rt_id == rte.id)
  rte <- all.tweets[rte.index, ]
  initial.user <- rte$rt_screen_name[1]
  
  rte.time.table <- table(rte$time.minutes[-1])
  #plot(rte.time.table)
  
  rt_followers <- rte$rt_followers[-1]
  rt_followers <- rt_followers - min(rt_followers)
  rt_followers <- rt_followers/max(rt_followers)
  rt_followers <- rt_followers * max(rte.time.table) * .75
  
  followers <- log10(rte$followers[-1])/2
    
  max.at <- which.max(rte.time.table)
  max.at <- as.numeric(names(rte.time.table[max.at]))
  x.range <- c(max.at, max.at + 60)
  rte.time.x <- as.numeric(names(rte.time.table))
  rte.rate.y <- as.numeric(rte.time.table)
  zap.index <- c(which(rte.time.x < x.range[1]), which(rte.time.x > x.range[2]))
  if (length(zap.index > 0)) {
    rte.time.x <- rte.time.x[-zap.index]
    rte.rate.y <- rte.rate.y[-zap.index]
  }
  
  followers.x <- rte$time.minutes
  followers.y <- rte$rt_followers
  
  zap.index <- c(which(rte$time.minutes < x.range[1]), which(rte$time.minutes > x.range[2]))
  if (length(zap.index > 0)) {
    followers.x <- followers.x[-zap.index]
    followers.y <- followers.y[-zap.index]
  }
  followers.y <- followers.y - min(followers.y)
  followers.y <- max(rte.time.table) * (followers.y/max(followers.y)) * .75
  
    
  plot.OccupyWallSt.rte.name <- paste(plot.rtes.dir, initial.user, "_", dim(rte)[1], "_", rte.id, "_followDelta.png", sep="")
  png(plot.OccupyWallSt.rte.name, width=600, height=300)
  par(xpd=NA)
  plot(rte.time.x, rte.rate.y, bty="n", type="h", col=rgb(64, 153, 255, maxColorValue=255), lwd=2
      , ylab="retweets", xlab="minutes", ylim=c(0, max(rte.time.table)))
  points(followers.x, followers.y, cex=followers, col=rgb(179, 0, 0, maxColorValue=255))
  
  #plot(rte.time.table, bty="n", col=rgb(64, 153, 255, maxColorValue=255), lwd=2
  #  , ylab="retweets", xlab="minutes")
  mtext(paste(rte$rt_screen_name[1], ", ", rte.id, sep=""), side=3, line=2, cex=1.1)
  mtext("Retweets per minute for RTE", side=3, line=.9)
  mtext(paste("size: ", dim(rte)[1]), side=3, line=-.2)
  
  #points(rte$time.minutes[-1], rt_followers, cex=followers, col=rgb(179, 0, 0, maxColorValue=255))
  
  dev.off()
}
}
















#rte.meta.df$start.date <- strptime(rte.meta.df$start.date, created.ts.format.string, tz="GMT")
#o <- order(rte.meta.df$start.date)
#rte.meta.df <- rte.meta.df[o,]
#rte.meta.df$overlap <- 0
#rte.meta.df$occ.day.count <- 0

plot(log10(rte.meta.df$follower.delta), log10(rte.meta.df$follower.start))
cor.test(log10(rte.meta.df$follower.delta + 40), log10(rte.meta.df$follower.start + 10))
cor.test(rte.meta.df$follower.delta, rte.meta.df$follower.start)
plot(rte.meta.df$follower.delta, rte.meta.df$follower.start)

summary(rte.meta.df$tweet.overlap)



colnames(rte.meta.df)
tmp.index <- which(rte.meta.df$alpha > 2.3)
rte.meta.df$afterlife.percent[tmp.index]

rte.meta.df[tmp.index[8],]

which(rte.meta.df$poission.p > 0.01)
rte.meta.df[208,]
# plotFix()

# add some dummies for 
# Tuesday November 15 : Occupy Wall Street' At about 1am, NYPD began to clear Zuccotti Park. 
# Thursday November 17 : More than 30,000 demonstrated in and around Zuccotti Park, Union Square, Foley Square, the Brooklyn Bridge,
tmp.dates <- format(strptime(rte.meta.df$start.date, created.ts.format.string), "%d%b%Y")
rte.meta.df$Nov15 <- ifelse(tmp.dates == "15Nov2011", 1, 0)
rte.meta.df$Nov17 <- ifelse(tmp.dates == "17Nov2011", 1, 0)

plot(table(rte.meta.df$occupy.time.marker))

colnames(rte.meta.df)
zap.index <- which(is.na(rte.meta.df$follow.prob.closeness))
rte.meta.df <- rte.meta.df[-zap.index,]
num.no.trees <- length(zap.index)
good.alphas.index <- which(rte.meta.df$KS.p < 0.05)

cat("Out of ", num.rtes, " RTEs, got ", length(good.alphas.index), " with useable alphas, lost ", num.no.trees, " for no signatures\n", sep="")


rte.pval.colors <- rep("firebrick3", length(rte.meta.df$KS.p))
rte.pval.colors[good.alphas.index] <- "forestgreen"
plot(rte.meta.df$size, rte.meta.df$KS.p, col=rte.pval.colors, bty="n", ylab="p-value"
  , main="", pch="*")
mtext(text=paste("Size by alpha estimate p-values\nuseable signatures: ", length(good.alphas.index), sep=""), side=3, cex=1.3)

useable.rte.meta.df <- rte.meta.df[good.alphas.index,]
colnames(useable.rte.meta.df)
base.working <- "C:/r/aDiss/processRTEs/"
#load.file.name <- "rte.data.495"
#rte.data.495 <- useable.rte.meta.df
#save(rte.data.495, file=paste(base.working, load.file.name, ".Rdata", sep=""))


#too.small.index <- which(useable.rte.meta.df$max.vol < 10)
#useable.rte.meta.df <- useable.rte.meta.df[too.small.index, ]
#high.alphas.index <- which(useable.rte.meta.df$alpha > 2.3)
#useable.rte.meta.df <- useable.rte.meta.df[-high.alphas.index,]


summary.main.text <- paste("Output Variable Correlation Matrix\nn = ", dim(useable.rte.meta.df)[1], sep="")
plot.set.name.plots <- paste(plot.rtes.dir, dim(useable.rte.meta.df)[1], "_summary_correlations.png", sep="")
png(plot.set.name.plots, width=800, height=800, pointsize=18)
#plot.set.name.plots <- paste(plot.rtes.dir, "summary_correlations.pdf", sep="")
#pdf(plot.set.name.plots, width=7.5, height=7.5)
#pairs(~follower.delta+follower.percent.delta+size+occupy.time.marker+alpha+peak.at+wait.closeness+follow.prop.closeness+follow.prob.closeness
pairs(~follower.delta+alpha+follow.prop.closeness+wait.closeness+occupy.time.marker+size+peak.at+max.vol+follower.sum+follower.mean
  , data=useable.rte.meta.df
  , lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel = panel.hist, text.panel=my.text.panel
  , pch=20, col=adjustAlpha("forestgreen", .3)
  , main="")
mtext(text=summary.main.text, side=3, line=1)
dev.off()

write.table(useable.rte.meta.df, file = paste(plot.rtes.dir, dim(useable.rte.meta.df)[1], "_rte_data.csv", sep=""), sep=",", row.names = FALSE)
rte.data.496 <- useable.rte.meta.df
save(rte.data.496, file=paste(base.working, "rte.data.496", ".Rdata", sep=""))
gc()
# add.1.useable.rte.meta.df <- useable.rte.meta.df
# add.point1.useable.rte.meta.df <- useable.rte.meta.df
# full.set.rte.met.df <- useable.rte.meta.df


all.tweets$rt_screen_name



colnames(all.tweets)
o <- order(all.tweets$created_ts)
all.tweets <- all.tweets[o,]
all.OccupyWallSt.index <- which(all.tweets$rt_screen_name == "OccupyWallSt")
all.rt_followers <- all.tweets$rt_followers[all.OccupyWallSt.index]
zap <- which(is.na(all.rt_followers))
all.rt_followers <- all.rt_followers[-zap]
plot(all.rt_followers, pch=".")





zap <- unique(c(which(is.na(rte.meta.df$longest.path)), which(is.infinite(rte.meta.df$longest.path))))
gini <- rte.meta.df$gini[-zap]
chains.vec <- rte.meta.df$chains[-zap]
close.vec <- rte.meta.df$follow.prop.closeness[-zap]
long.vec <- rte.meta.df$longest.path[-zap]

rte.meta.df[50,]

cor.test(gini, long.vec)
plot(gini, long.vec)
cor.test(gini, chains.vec)
plot(gini, long.vec)
cor.test(gini, close.vec)
plot(gini, close.vec)

cor.test(rtes$chains, rtes$gini)
cor.test(rtes$longest.path, rtes$gini)
cor.test(rtes$chains, rtes$follow.prop.closeness)
cor.test(rtes$longest.path, rtes$follow.prop.closeness)



