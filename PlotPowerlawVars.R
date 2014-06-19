#
# all.tweets power-laws
#

# Suppose I make a network plot and size the nodes by how many 
# followers someone has or how often they tweet or are retweeted
# in the data. Unless I account for the fact that the distribution
# of these variables follows a power-law (see figure 2.10)...

blue <- rgb(80,165,255, maxColorValue=255)
red <- rgb(179,0,0, maxColorValue=255)
orange <- rgb(255, 150, 70, maxColorValue=255)

colnames(all.tweets)

rt.id.table <- sort(table(all.tweets$rt_id))
screen.name.table <- sort(table(all.tweets$screen_name))
rt.screen.name.table <- sort(table(all.tweets$rt_screen_name))

y <- sort(as.numeric(rt.screen.name.table), decreasing=T)
x <- 1:length(y)
plot(log10(x),log10(y))

##########################################################################################
# mean followers per user
followers.df <- aggregate(rt_followers~rt_screen_name, all.tweets, max)
# followers.df <- aggregate(followers~screen_name, all.tweets, max)
colnames(followers.df) <- c("user", "followers")
o <- order(followers.df$followers, decreasing=T)
followers.df$followers <- round(followers.df$followers, 0)
followers.df <- followers.df[o,]


##########################################################################################
# plot distribution of tweets
tweets.df <- aggregate(id~screen_name, all.tweets, length)
colnames(tweets.df) <- c("user", "tweets")
o <- order(tweets.df$tweets, decreasing=T)
tweets.df <- tweets.df[o,]



##########################################################################################
# plot distribution of RE tweets
retweets.df <- aggregate(rt_id~rt_screen_name, all.tweets, length)
colnames(retweets.df) <- c("user", "retweets")
o <- order(retweets.df$retweets, decreasing=T)
retweets.df <- retweets.df[o,]


##########################################################################################
#
# Now give us six plots, 2 of each, one on top the other
#
par(mfrow=c(2,3))

# top set
users <- 1:length(followers.df$followers)
followers <- followers.df$followers
plot(users, followers, type="h", col=blue, bty="n", main="Followers distribution")

users <- 1:length(tweets.df$tweets)
tweets <- tweets.df$tweets
plot(users, tweets, type="h", col=red, bty="n", main="User tweet frequency")

users <- 1:length(retweets.df$retweets)
retweets <- retweets.df$retweets
plot(users, retweets, type="h", col=orange, bty="n", main="User retweeted frequency")

# matching bottom set
users <- 1:length(followers.df$followers)
plot(log10(users), log10(followers), col=blue, bty="n", cex=.75, main="Log-log followers distribution")

users <- 1:length(tweets.df$tweets)
plot(log10(users), log10(tweets), col=red, bty="n", cex=.75, main="Log-log User tweet frequency")

users <- 1:length(retweets.df$retweets)
plot(log10(users), log10(retweets), col=orange, bty="n", cex=.75, main="Log-log user retweeted frequency")






