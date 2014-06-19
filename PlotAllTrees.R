

#plot trees by time
#calculate longest chain



#########################################################
#
# Author: Jeff Hemsley
# 
# Purpose: code for my disertation
# Description: This file experiments with bu8ilding trees
#              out of twitter RTE data.
#
# Issues:
# add distribution plots at end
# add origin tweeters follower count
# add occupy time delta
# think about changing how you do window to be more like it was
# think about outliers
#

rte.cutoff.size <- 105
use.occupy.tweets.only <- TRUE
signature.window <- 60 * 60 * 2
#signature.window <- 60

# first read in some of the general config info
base.dir <- log.file.path <- "c:/r/aDiss/simulation/"
tree.functions.name <- "tree_builder_functions_v2.R"
tree.function.file <- paste(base.dir, tree.functions.name, sep="")
source(tree.function.file)




#####################################################################
#
# OPEN TWEET DATA FILE
#
#####################################################################
#temp.rts.file.name <- "test3_rts_tweet_dat.csv"
temp.rts.file.name <- "rts_tweet_dat.csv"
dat.dir <- "C:/r/aDiss/dat/older/"
rt.dat.file <- paste(dat.dir, temp.rts.file.name, sep="")
rt.dat <- as.data.frame(read.csv(file=rt.dat.file, check.names=T, stringsAsFactors=F))
# "find_rt_id", "id", "created_ts", "screen_name", "followers", "rt_id", "rt_created_ts", "rt_screen_name", "rt_followers"
#cols.to.keep <- c("created_ts", "screen_name", "followers", "rt_created_ts", "rt_screen_name", "rt_followers")
o <- order(rt.dat$rt_id, rt.dat$id) 
rt.dat <- rt.dat[o,]


#####################################################################
#
# CONVERT DATE STRINGS TO DATE OBJECTS. 
# This has to be done early becuase the tweet date and retweet date are
# in different string formats and by converting both to date objects early
# we don't have problems when we go to create origin tweets later
# also, we can zap dates now that don't format.
#
#####################################################################
rt.dat <- convertStringDateFields(rt.dat)


#####################################################################
#
# FIND ALL RTES BY ID AND CREATE ORIGIN TWEETS FOR ALL RTES IN 
# A NEW DATA OBJECT
#
#####################################################################
origin.tweets <- makeOriginTweetsFromRetweets(rt.dat)


#####################################################################
#
# ADD ORIGIN TWEETS TO TWEET DATA AND ORDER BY ORIGIN TWEET ID
# THEN BY TWEET ID
#
#####################################################################
all.tweets <- rbind(origin.tweets, rt.dat)
dim(all.tweets)

o <- order(all.tweets$find_rt_id, all.tweets$id) 
all.tweets <- all.tweets[o,]

date.test.index <- c(which(is.na(all.tweets$created_ts)), which(is.na(all.tweets$rt_created_ts)))
if (length(date.test.index) > 0) { stop("Date formatting error")}


#####################################################################
#
# ADD COLUMN TO TWEET DATASET (WAIT_TIME) THAT CONTAINS THE TIME
# DIFF IN SECONDS FOR ALL TWEETS FROM THEIR ORIGEN TWEET. REMOVE ANY NAs.
# count and remove negative values
# Add a column for occupy time marker, which is the difference in days
# from when OWS started "2011-09-17 GMT" to when the tweet was sent.
# Remove any tweets with negitive values
#
#####################################################################
all.tweets <- createDateDiffColumns(all.tweets)



#####################################################################
#
# ADD COLUMN FOR FOLLOWER PROB AND FOLLOWER PROPORTION
#
#####################################################################
all.tweets <- createFollowerProbabilityColumn(all.tweets)

max.follower.proportion <- max(all.tweets$follower.proportion)
  

#####################################################################
#
# ONLY INCLUDE TWEETS THAT ORIGINATED FROM AN OCCUPY ACCOUNT
#
#####################################################################

use.occupy.tweets.only <- TRUE
if (use.occupy.tweets.only) {
  # alt.names <- unique(rt.dat$rt_screen_name[grep("arrests", rt.dat$rt_screen_name, ignore.case = TRUE)])
  # "AnonymousPress" "AnonymousIRC"   "AnonymousWiki"  "anonymouSabu"
  # "BankTransferDay" "gtbank"          "MrBankole"       "BankyW"
  # "OccupyArrests"
  occupy.origin.index <- grep("(OWS|Occupy|BankTransferDay)", all.tweets$rt_screen_name, ignore.case = TRUE)
  length(occupy.origin.index)
  occupy.originators <- sort(unique(all.tweets$rt_screen_name[occupy.origin.index]))
  length(occupy.originators)
  
  all.tweets <- all.tweets[occupy.origin.index,]
}

dim(all.tweets)

#####################################################################
#
# SINCE WE HAVE TIME DIFFS FOR EACH TWEET WE CAN DROP ALL TWEETS 
# THAT OCCOUR WELL OUT OF THE WINDOW
#
#####################################################################

max.seconds.from.origin <- dim(SimParm.WaitTimes)[1]
out.of.window.tweets.index <- c(which(all.tweets$time.delta > max.seconds.from.origin), which(all.tweets$time.delta < 0))
#out.of.window.tweets <- all.tweets[out.of.window.tweets.index,]
#dim(out.of.window.tweets)
all.tweets <- all.tweets[-out.of.window.tweets.index,]
dim(all.tweets)

#####################################################################
#
# NOW WE HAVE A SET OF RTES, SOME OF WHICH WILL BE TOO SMALL TO USE
# SO ZAP THOSE AND FIND THE FINAL LIST OF USEABLE RTES
#
#####################################################################
rte.id.table <- sort(table(all.tweets$find_rt_id))
plot(rte.id.table[1:200])
length(rte.id.table)

dim(all.tweets)

rtes.too.small <- which(rte.id.table < rte.cutoff.size)
length(rtes.too.small)

rte.ids <- as.numeric(names(rte.id.table)[-rtes.too.small])
rte.ids <- rev(rte.ids)
num.rtes <- length(rte.ids)
num.rtes
#colnames(all.tweets)
# "find_rt_id", "id", "created_ts", "screen_name", "followers"
# , "rt_id", "rt_created_ts", "rt_screen_name", "rt_followers"
# , "time.delta", "follower.proportion"

range(all.tweets$time.delta)
summary(log10(all.tweets$time.delta + 1))
obs <- length(all.tweets$time.delta)

tmp.time.delta <- log10(all.tweets$time.delta + 1) + (rnorm(obs)/10)
tmp.time.delta <- round(tmp.time.delta)
if (min(tmp.time.delta) < 1) {
  tmp.time.delta <- abs(min(tmp.time.delta)) + 1 + tmp.time.delta
}
summary(tmp.time.delta)

# an earthy range
num.cols <- max(tmp.time.delta)
tmp.cols <- rainbow(num.cols, start=0, end=.16, s=seq(.7,1,length=num.cols), v=seq(.7,1,length=num.cols))
pie(rep(1,num.cols), col=tmp.cols, labels=paste(1:num.cols,tmp.cols))

par(mar=c(1,1,1,1), bg="black")
colnames(all.tweets)
o <- order(all.tweets$id)
plot(all.tweets$time.delta[o], col=tmp.cols, pch="-", cex=1, bty="n", xaxt="n", yaxt="n", main="", ylab="", xlab="")



#####################################################################
#####################################################################
#
# HERE I GO ABOUT INFERING THE CASCADE TREE FOR EACH RTE
# 
#
#####################################################################
#####################################################################
gc()

colnames(all.tweets)
  
pre.peak.slots <- 20
post.peak.slots <- 120
x <- -pre.peak.slots:post.peak.slots

num.loops <- num.rtes

if (!use.occupy.tweets.only) {
  num.loops <- 1200
  stop.sample.at <- 1000
  sampled.rte.ids <- sample(rte.ids, size=num.loops, replace=F)
  use.sample <- TRUE
  plot.dir <- "c:/r/jobtalk/sigplots/"
}

plot.each.sig <- FALSE
plot.trees <- TRUE
min.rte.size <- 100
rte.size.vec <- rep(NA, num.loops)

df.chains <- data.frame(size = rep(0, num.loops), longest.path = rep(0, num.loops), chains = rep(0, num.loops), closeness.out = rep(0, num.loops))

plot.dir <- "C:/r/aDiss/descriptivePlots/trees/"

for (i in 1:num.loops) {
  # i <- 1 + i + 30
  # i <- 3740
  
  rte.id <- rte.ids[i]

  rte.index <- which(all.tweets$find_rt_id == rte.id)
  rte <- all.tweets[rte.index, ]

  if (dim(rte)[1] < min.rte.size) {
    next
  }
  
  #cat(i, ": RTE ID: ", rte.id, ", event size ", dim(rte)[1], " ", follower.delta, " ", follower.percent.delta, "\n",  sep="")
  cat(i, ": RTE ID: ", rte.id, ", event size ", dim(rte)[1], "\n",  sep="")
  
  signature <- createSignatureObject (rte)
  names(signature)
  if (is.null(signature)) {
    next()
  }
  
  out.of.window.index <- which(rte$time.minutes > signature$adjusted.max.minutes)
  if (length(out.of.window.index) > 0) {
    rte <- rte[-out.of.window.index,]
    max(rte$time.delta)
  }
  
  rte.size.vec[i] <- dim(rte)[1]
  
  #cat("MAX MIN: ", max(rte$time.delta), "\n")
  #plot(g.wait.time)
  g.wait.time <- createWaitTimeTreeGraph(rte)
  g.combined.follower.proportion <- createCombinedTreeGraph (rte, use.follower.proportion = TRUE)
  g.combined.follower.probability <- createCombinedTreeGraph (rte, use.follower.proportion = FALSE)
  
  #plot.set.name.plots <- paste(plot.dir, "RTE_", rte.id, ".png", sep="")
  #png(plot.set.name.plots, width=1024, height=1024)

  par(mar=c(.5, .5, .5, .5)) ##  c(bottom, left, top, right)
  #par(mfrow=c(2, 1))
  
  #NewTreePlot(g.wait.time, main="", use.time.axis=FALSE, node.col="tan") 
  tmp <- NewTreePlot(g.combined.follower.proportion, main=""
    , use.time.axis=TRUE, node.col="lightskyblue") 
  
  df.chains$size[i] <- tmp$size[1]
  df.chains$longest.path[i] <- tmp$longest.path[1]
  df.chains$chains[i] <- tmp$chains[1]
  df.chains$closeness.out[i] <- tmp$closeness.out[1]
  #NewTreePlot(g.combined.follower.probability, main="", use.time.axis=FALSE, node.col="tan") 
    
  dev.off()
}

summary(df.chains$size)
summary(df.chains$longest.path)
summary(df.chains$chains)
summary(df.chains$closeness.out)

plot(sort(df.chains$longest.path, decreasing=T), main="Longest path")
plot(sort(df.chains$chains, decreasing=T), main="Number of chains")

plot(jitter(df.chains$longest.path), jitter(df.chains$chains))

pairs(~size+longest.path+chains+closeness.out, data=df.chains
  , lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel = panel.hist, text.panel=my.text.panel
  , pch=20, col=adjustAlpha("forestgreen", .3)
  , main="Measures of chains")



colnames(rtes)

tmp.closeness <- rtes$follow.prop.closeness
tmp.longpath <- rtes$longest.path
tmp.chains <- rtes$chains


zap <- unique(c(which(is.infinite(rte.meta.df$longest.path)), which(is.na(rte.meta.df$longest.path)), which(is.na(rte.meta.df$follow.prop.closeness)), which(is.na(rte.meta.df$chains))))
tmp.closeness <- rte.meta.df$follow.prop.closeness[-zap]
tmp.longpath <- rte.meta.df$longest.path[-zap]
tmp.chains <- rte.meta.df$chains[-zap]




par(mfrow=c(1,2))
plot(tmp.closeness, tmp.longpath, col=blue, cex=.7, bty="n", main="", ylab="longest path", xlab="closeness")
abline(lm(tmp.longpath~tmp.closeness), col="black", lwd=1)
ct <- cor.test(tmp.closeness, tmp.longpath)
mtext(text=paste("Correlation: ", round(ct$estimate, 2), " (df: 427)", sep=""), side=3, line=1.5)
mtext(text=paste("95% CI: ", round(ct$conf.int[1],2), ", ", round(ct$conf.int[2],2), sep=""), side=3, line =0)


plot(tmp.closeness, tmp.chains, col=blue, cex=.7, bty="n", main="", ylab="longest path", xlab="closeness")
abline(lm(tmp.chains~tmp.closeness), col="black", lwd=1)
ct <- cor.test(tmp.closeness, tmp.chains)
mtext(text=paste("Correlation: ", round(ct$estimate, 2), " (df: 427)", sep=""), side=3, line=1.5)
mtext(text=paste("95% CI: ", round(ct$conf.int[1],2), ", ", round(ct$conf.int[2],2), sep=""), side=3, line =0)
