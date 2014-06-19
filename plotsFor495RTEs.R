include.files.dir <- log.file.path <- "c:/r/aDiss/processRTEs/"
plot.rtes.dir <- "C:/r/aDiss/processRTEs/RTE_Plots/"

# contains global variable settings
process.rtes.config.file.name <- "processRTEs_CONFIG_v0.R"
source(paste(include.files.dir, process.rtes.config.file.name, sep=""))


library(car)
library(faraway)

descriptives.dir <- "C:/r/aDiss/descriptivePlots/"
base.working <- "C:/r/aDiss/processRTEs/"

load.file.name <- "rte.data.495"
#load.file.name <- "rte.data.496"
#save(rte.data.495, file=paste(base.working, "rte.data.495", ".Rdata", sep=""))
load(file=paste(base.working, load.file.name, ".Rdata", sep=""))
rte.df <- rte.data.495

dim(rte.data.495) # 495
colnames(rte.data.495)
plot(rte.data.495$KS.p)

# followers
hist(rte.data.495$follower.delta, col="orange", xlab="change in followers", main="Histogram: change in followers")
plot(sort(log10(rte.data.495$follower.delta)))
which(rte.data.495$follower.delta < 0)

# alpha
hist(rte.data.495$alpha, col="orange", xlab="estimated alphas", main="Histogram: RTE alphas")

# closeness
hist(rte.data.495$follow.prop.closeness, col="orange", xlab="closeness values", main="Histogram: RTE path closeness")

# closeness
hist(rte.data.495$size, col="orange", xlab="RTE sizes", main="Histogram: RTE sizes")

# occ.users
hist(rte.data.495$occ.users, col="orange", xlab="Number of other Occupy users", main="Histogram: number of Occupy Users")


follower.delta+alpha+follow.prop.closeness+wait.closeness+occupy.time.marker+size+peak.at+max.vol+follower.sum+follower.mean

alpha + follow.prop.closeness + size + peak.at + occ.users + occ.day.count + Nov15

pairs(~alpha + follow.prop.closeness + size + peak.at + occ.users + occ.day.count + Nov15
  , data=rte.data.495
  , lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel = panel.hist, text.panel=my.text.panel
  , pch=20, col=adjustAlpha("forestgreen", .3)
  , main="")






qqnorm(log(rte.data.495$size))
qqline(log(rte.data.495$size), col = 2)



plot(table(rte.data.495$follower.delta), col="orange", bty="n"
  , ylab="frequency", xlab="change in followers", main="Histo")

unique(rte.data.495$origin.user)

user.table <- table(rte.data.495$origin.user)
length(user.table)

sum(rte.data.495$size)


colnames(rte.df)



row.names(rte.df) <- 1:nrow(rte.df)
colnames(rte.df)
dim(rte.df)

sum(rte.df$size)
rte.user.table <- sort(table(rte.df$origin.user), decreasing=T)

#load(file=paste(base.working, "ConstructedRTEdata_201404300819.Rdata", sep="")) 
colnames(rte.meta.df)
dim(rte.meta.df)


which(rte.df$rte.id %in% rte.meta.df$rte.id)
index.occ.rtes <- which(rte.meta.df$rte.id %in% rte.df$rte.id)
index.occ.rtes.2 <- which(is.element(rte.meta.df$rte.id, rte.df$rte.id))

rte.meta.df[9,]

index.occ.rtes[1:10]
index.occ.rtes.2[1:10]

occ.rte.meta.df <- rte.meta.df[,]  
table(occ.rte.meta.df$origin.user)

which(rte.df$rte.id == 126502991200137216)
rte.df[2,]

which(rte.meta.df$rte.id == 126502991200137216)
rte.meta.df[9,]

A <- 1:10
B <- 7:8
is.element (B,A)
is.element (A, B) # big one first






plot.set.name.plots <- paste(descriptives.dir, "RTE495_Sizes.png", sep="")
png(plot.set.name.plots, width=600, height=300)
#plot(sort(log10(rte.meta.df$peak.rate)), bty="n", pch=".", ylab="minutes", main="Time from initial tweet\nto last retweet")
hist(rte.df$size, col="orange", xlab="RTE Size", main="Occupy RTE Sizes")
dev.off()



colnames(all.tweets)
