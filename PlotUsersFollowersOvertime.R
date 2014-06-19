

colnames(all.tweets)

user.rt_screen_name <- "OccupyWallStNYC"
user.index <- which(all.tweets$rt_screen_name == user.rt_screen_name)
user.df <- all.tweets[user.index,]
zap.index <- which(is.na(user.df$rt_followers))
user.df <- user.df[-zap.index,]

dim(user.df)

user.df$rt_followers

plot.user.followers.name <- paste(plot.rtes.dir, "FollowerGrowth_", user.rt_screen_name, "_", dim(user.df)[1], "retweets.png", sep="")
png(plot.user.followers.name, width=600, height=300)

plot(user.df$created_ts, user.df$rt_followers, pch=".", bty="n", xlab="time", ylab="followers", main=paste(user.rt_screen_name, " follower count\nfor each retweet"))
dev.off()
class(user.df$created_ts)
