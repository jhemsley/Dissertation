



# simulated.RTE.file.name <- "simulated_RETs.csv"

#config.file.name <- "RTE_sim_config_v10.R"
# config.file <- paste(base.dir, config.file.name, sep="")
#source(config.file)

#sim.functions.name <- "RTE_Sim_Functions_v10.R"
#sim.function.file <- paste(base.dir, sim.functions.name, sep="")
#source(sim.function.file)

#SimParm.WaitTimes.file.name <- "SimParm_WaitTimes_871641.csv"
#SimParm.WaitTimes.file.name <- "SimParm_WaitTimes_seconds_870237.csv"


# users who were retweeted
#
#SimParm.UsrRT.file.name <- "SimParm_UsrRT_1387849.csv"
#SimParm.UsrRT <- readSimParamUsrRT (sim.param.data.dir, SimParm.UsrRT.file.name, log=TRUE)

#
# second file is the users who RT
#
#SimParm.UsrTweets.file.name <- "SimParm_UsrTweets_1387849.csv"
#SimParm.UsrTweets <- readSimParamTweetUsers (sim.param.data.dir, SimParm.UsrTweets.file.name, log=TRUE)
#SimParm.UsrTweets.num <- dim(SimParm.UsrTweets)[1]

############################################################################
#
# read in the parameter file of wait times and return the df
#
readSimParamWaitTimes <- function (sim.param.data.dir, SimParm.WaitTimes.file.name, log=FALSE) {
  SimParm.WaitTimes.file <- paste(sim.param.data.dir, SimParm.WaitTimes.file.name, sep="")
  SimParm.WaitTimes <- as.data.frame(read.csv(file=SimParm.WaitTimes.file, check.names=T, stringsAsFactors=F))
  SimParm.WaitTimes$rank <- 1:(dim(SimParm.WaitTimes)[1])
  
  SimParm.WaitTimes
}


createRTEmetaDataObject <- function(n) {
  
  rte.meta.df <- data.frame(
    rte.id = rep(NA, n)
    , origin.user = rep(NA, n)
    , num.bots = rep(NA, n)
    , occ.users = rep(NA, n)
    , size = rep(NA, n)
    #, start.date = rep(all.tweets$created_ts[1], num.rtes)
    , start.date = rep(OWS.start.date, n)
    , occupy.time.marker = rep(NA, n)
    , follower.start = rep(NA, n)
    , follower.delta = rep(NA, n)
    , follower.percent.delta = rep(NA, n)
    , follower.sum = rep(NA, n)
    , follower.mean = rep(NA, n)
    # signature data
    , alpha = rep(NA, n)
    , KS.p = rep(NA, n)
    , peak.at = rep(NA, n)
    , max.vol = rep(NA, n)
    , pre.peak.size = rep(NA, n)
    , post.peak.size = rep(NA, n)    
    , poission.p = rep(NA, n)
    # cascade data
    , wait.closeness = rep(NA, n)
    , follow.prop.closeness = rep(NA, n)
    , follow.prob.closeness = rep(NA, n)
    , post.peak.closeness = rep(NA, n)
    , longest.path = rep(NA, n)
    , chains = rep(NA, n)
    
    
  )
  
  rte.meta.df
}

metaDataReportLine <- function(rte, g.wait.time, g.combined.follower.proportion, g.combined.follower.prbability, post.peak.g) {
  
  rte.meta.data <- createRTEmetaDataObject(1)
  
  followers.at.start <- rte$followers[1]
  followers.at.end <- rte$rt_followers[dim(rte)[1]]
  follower.delta <- followers.at.end - followers.at.start
  follower.percent.delta <- follower.delta/followers.at.start
  rte.meta.data$rte.id[1] <- rte$find_rt_id[1]
  rte.meta.data$origin.user[1] <- rte$screen_name[1]
  rte.meta.data$num.bots[1] <- length(which(rte$screen_name %in% bot.account.list))
  rte.meta.data$occ.users[1] <- length(grep("(OWS|Occupy)", rte$screen_name, ignore.case = FALSE)) - 1
  rte.meta.data$size[1] <- dim(rte)[1]
  rte.meta.data$start.date[1] <- rte$created_ts[1]
  rte.meta.data$occupy.time.marker[1] <- rte$occupy.time.marker[1]
  rte.meta.data$follower.start[1] <- followers.at.start
  rte.meta.data$follower.delta[1] <- follower.delta
  rte.meta.data$follower.percent.delta[1] <- follower.percent.delta
  rte.meta.data$follower.sum[1] <- sum(rte$followers)
  rte.meta.data$follower.mean[1] <- mean(rte$followers)
  rte.meta.data$alpha[1] <- signature$alpha
  rte.meta.data$KS.p[1] <- signature$KS.p
  rte.meta.data$peak.at[1] <- signature$peak.at
  rte.meta.data$max.vol[1] <- signature$max.vol
  rte.meta.data$pre.peak.size[1] <- signature$pre.peak.size
  rte.meta.data$post.peak.size[1] <- signature$post.peak.size
  rte.meta.data$poission.p[1] <- signature$poission.p  
  rte.meta.data$wait.closeness[1] <- g.wait.time$closeness.out
  rte.meta.data$follow.prop.closeness[1] <- g.combined.follower.proportion$closeness.out
  rte.meta.data$follow.prob.closeness[1] <- g.combined.follower.prbability$closeness.out
  rte.meta.data$post.peak.closeness[1] <- post.peak.g$closeness.out
  
  rte.meta.data
}


#####################################################################
#
# CONVERT DATE STRINGS TO DATE OBJECTS. 
# This has to be done early becuase the tweet date and retweet date are
# in different string formats and by converting both to date objects early
# we don't have problems when we go to create origin tweets later
# also, we can zap dates now that don't format.
#
#####################################################################
convertStringDateFields <- function(retweet.data){
  retweet.data$created_ts <- strptime(retweet.data$created_ts, created.ts.format.string, tz="GMT")
  bad.date.index <- which(is.na(retweet.data$created_ts))
  bad.date.index.length <- length(bad.date.index)
  
  if (bad.date.index.length > 0) {
    print("Number of bad created_ts dates:", bad.date.index.length, "\n")
  }
  
  retweet.data$rt_created_ts <- strptime(retweet.data$rt_created_ts, created.at.format.string, tz="GMT")
  bad.date.index <- which(is.na(rt.dat$created_ts))
  bad.date.index.length <- length(bad.date.index)
  
  if (bad.date.index.length > 0) {
    print("Number of bad created_ts dates:", bad.date.index.length, "\n")
  }
  
  retweet.data
}

#####################################################################
#
# FIND ALL RTES BY ID AND CREATE ORIGIN TWEETS FOR ALL RTES IN 
# A NEW DATA OBJECT
# The original data is just the retweets, but we need a separate tweet
# for the origin tweet of each RTE. Fortunately, each retweet in 
# the dataset contains information about the origin tweet. This
# function extracts that info for each RTE and builds a data frame of
# origin tweets. 
#
#####################################################################
makeOriginTweetsFromRetweets <- function (retweet.data, verbose = FALSE) {
  
  # retweet.data <- rt.dat
  rte.id.table <- sort(table(retweet.data$find_rt_id))
  rte.ids <- names(rte.id.table)
  num.rtes <- length(rte.ids)
  
  # here I am just creating an object to hold the origin tweets
  origin.tweets <- retweet.data[1:num.rtes,]
  #str(origin.tweets)
  #origin.tweets$time.delta <- 0
  
  # "find_rt_id", "id", "created_ts", "screen_name", "followers"
  # , "rt_id", "rt_created_ts", "rt_screen_name", "rt_followers"
  cols.tweet.set <- c("id", "created_ts", "screen_name", "followers")
  cols.rt.set <- c("rt_id", "rt_created_ts", "rt_screen_name", "rt_followers")
  
  for (i in 1:num.rtes) {
    #i <- 1
    rte.index <- match(as.numeric(rte.ids[i]), retweet.data$find_rt_id)
    rte <- retweet.data[rte.index, ]
    
    #str(rte)
    
    origin.tweets[i,] <- rte[1,]
    origin.tweets$find_rt_id[i] <- rte$find_rt_id[1]
    origin.tweets[i,cols.tweet.set] <- rte[1,cols.rt.set]
    origin.tweets[i,c("rt_id", "rt_followers")] <- NA
    
    if (verbose) {
      cat(i, rte.ids[i], "\n")
    }
    #origin.tweets[1,]
  }
  
  #str(origin.tweets)
  origin.tweets
}


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
createDateDiffColumns <- function (retweet.data, test.negative.retweets = TRUE, verbose = FALSE) {
  #retweet.data <- all.tweets
  #retweet.data <- rt.dat
  
  retweet.data$time.delta   <- round(as.numeric(difftime(retweet.data$created_ts, retweet.data$rt_created_ts, units = "secs")), 0)
  retweet.data$time.minutes <- round(as.numeric(difftime(retweet.data$created_ts, retweet.data$rt_created_ts, units = "min")),0)
  retweet.data$occupy.time.marker <- round(as.numeric(difftime(retweet.data$created_ts, OWS.start.date, units = "days")),0)
  retweet.data$occupy.time.minutes <- round(as.numeric(difftime(retweet.data$created_ts, OWS.start.date, units = "min")),0)
  #plot(table(retweet.data$occupy.time.marker))
  #plot(table(retweet.data$time.delta))
  
  
  date.test.index <- c(which(is.na(retweet.data$time.delta)), which(is.na(retweet.data$time.minutes)))
  if (length(date.test.index) > 0) { 
    cat("Date diffing caused NAs for", length(date.test.index), "tweets, which will be dropped\n")
    retweet.data <- retweet.data[-date.test.index,]
  }
  
  if (test.negative.retweets) {
    negative.time.tweets.index <- which(retweet.data$time.delta < 0)
    if (length(negative.time.tweets.index) > 0) { 
      cat("Number of tweets with negative value date diffs:", length(negative.time.tweets.index), ". These will be dropped\n")
      retweet.data <- retweet.data[-negative.time.tweets.index,]
    }
  }
  
  pre.occupy.tweets <- which (retweet.data$occupy.time.marker < 0)
  if (length(pre.occupy.tweets) > 0) { 
    cat("Number of tweets posted before Occupy (", as.character(OWS.start.date), "): ", length(pre.occupy.tweets), ". These will be dropped\n")
    retweet.data <- retweet.data[-pre.occupy.tweets,]
  }
  
  retweet.data
}


#####################################################################
#
# ADD COLUMN FOR FOLLOWER PROB AND FOLLOWER PROPORTION
#
#####################################################################
createFollowerProbabilityColumn <- function(retweet.data) {
  
  count.of.all.followers <- sum(as.numeric(retweet.data$followers))
  retweet.data$follower.proportion <- retweet.data$followers / count.of.all.followers
  # just scale it for neatness
  retweet.data$follower.proportion <- retweet.data$follower.proportion/max(retweet.data$follower.proportion)
  #summary(all.tweets$follower.proportion)
  #retweet.data$follower.proportion <- scaleToSignif(retweet.data$follower.proportion)
  
  follower.table <- table(retweet.data$followers)
  followers.count.frequency <- as.numeric(follower.table)
  followers.count <- as.numeric(names(follower.table))
  followers.count.probability <- followers.count.frequency/sum(followers.count.frequency)
  
  where.follower.count.is.index <- match(retweet.data$followers, followers.count)
  retweet.data$followers.count.probability <- followers.count.probability[where.follower.count.is.index]
  retweet.data$followers.count.probability <- scaleToSignif(retweet.data$followers.count.probability)
  
  retweet.data$followers.count.probability
  
  retweet.data
}
