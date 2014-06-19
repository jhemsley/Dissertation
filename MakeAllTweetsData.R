#########################################################
#
# Author: Jeff Hemsley
# 
# Purpose: code for my disertation
# Description: This file adds fields to 
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


# contains global variable settings
process.rtes.config.file.name <- "processRTEs_CONFIG_v0.R"
source(paste(include.files.dir, process.rtes.config.file.name, sep=""))


# contains functions to build RTE cascade tress
data.functions.file.name <- "data_functions_v1.R"
source(paste(include.files.dir, data.functions.file.name, sep=""))

# contains functions to build RTE cascade tress
tree.builder.functions.file.name <- "tree_builder_functions_v4.R"
source(paste(include.files.dir, tree.builder.functions.file.name, sep=""))

# first read in some of the general config info
#base.dir <- log.file.path <- "c:/r/aDiss/simulation/"
#tree.functions.name <- "tree_builder_functions_v2.R"
#tree.function.file <- paste(base.dir, tree.functions.name, sep="")
#source(tree.function.file)


#####################################################################
#
# OPEN TWEET DATA FILE
#
#####################################################################
temp.rts.file.name <- "test3_rts_tweet_dat.csv"
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
colnames(rt.dat)


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

dim(origin.tweets)
dim(rt.dat)

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
colnames(all.tweets)

plot(table(all.tweets$occupy.time.marker), bty="n", 
  main=paste("Tweet volume by days into occupy", format(OWS.start.date, "%m-%d-%Y"), sep="\n")
  , xlab="days", ylab="frequency")



#####################################################################
#
# ADD COLUMN FOR FOLLOWER PROB AND FOLLOWER PROPORTION
#
#####################################################################
all.tweets <- createFollowerProbabilityColumn(all.tweets)


all.tweets.rdata.file <- paste(all.tweets.dir, "all.tweets.Rdata", sep="")
save(all.tweets, file=all.tweets.rdata.file)


load(all.tweets.rdata.file)




