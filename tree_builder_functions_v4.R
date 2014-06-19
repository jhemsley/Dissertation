


#####################################################################
#
# Function takes a RTE dataframe and returns a signature object
#
#####################################################################

createSignatureObject <- function(rte, y.bump=1, window.size.in.minutes = 120) {
  
  #window.size.in.minutes <- window.size.in.seconds / 60
  # all the +1s are becuase we can have zero seconds, but we don't have a zero index
  #max.x <- max(rte$time.minutes) + 1
  max.x <- max(max(rte$time.minutes) + 1, 120)
  x <- 1:max.x
  y <- rep(0, length(x))
  
  rte.wait.table <- table(round(rte$time.minutes,0))
  y.values.index <- as.numeric(names(rte.wait.table)) + 1
  
  if (length(as.numeric(rte.wait.table)) != length(y[y.values.index])) stop("signature:: length(as.numeric(rte.wait.table)) != length(y[y.values.index])")
  
  y[y.values.index] <- as.numeric(rte.wait.table)
   
  peak.at <- which.max(y)
  
  if (peak.at > window.size.in.minutes * 2) { 
    warning.msg <- paste("createSignatureObject:: (", rte$find_rt_id[1], ") event peak after ", (window.size.in.minutes * 2), " minutes", sep="")
    warning(warning.msg)
    return(NULL)
  }
  
  adjusted.max.minutes <- min((peak.at + window.size.in.minutes), max.x)
  
  x <- x[1:adjusted.max.minutes]
  y <- y[1:adjusted.max.minutes]
  y.length <- length(y)
  #plot(x,y, type="h") 
  
  # we jitter to break ties so we get valid p-values, also
  # can't have negative or zero values in y when estimateing alpha
  y.tmp <- jitter(y[peak.at:y.length])
  y.tmp <- abs(min(y.tmp)) + y.tmp + y.bump
  # summary(y.tmp)
  
  plfit.sig.powerlaw.fit <- power.law.fit(y.tmp, xmin=1, start=1, implementation="plfit")
  plfit.sig.powerlaw.fit.alpha <- plfit.sig.powerlaw.fit$alpha
  
  Rmle.sig.powerlaw.fit <- power.law.fit(y.tmp, implementation="R.mle")
  Rmle.sig.powerlaw.fit.alpha <- attributes(summary(Rmle.sig.powerlaw.fit))$coef[1]
  KS.p <- plfit.sig.powerlaw.fit$KS.p
  
  if (powerlaw.fit.implementation == "plfit") {
    alpha <- round(plfit.sig.powerlaw.fit.alpha,2)
  } else {
    alpha <- round(Rmle.sig.powerlaw.fit.alpha,2)
  }
  
    
  x.powerlaw <- x[peak.at:y.length]
  y.powerlaw <- y[peak.at:y.length]
    
  y.powerlaw <- max(y) * (x^(-alpha))
  y.powerlaw <- y.powerlaw[1:length(x.powerlaw)]
  
  #lines(x.powerlaw, y.powerlaw)
  
  if (length(y.powerlaw) != length(x.powerlaw)) stop("signature:: length(y.powerlaw) != length(x.powerlaw)")
  
  poission.answer <- poissonTest(y)
  pval <- as.numeric(poission.answer[3])
  
  #plot(x,y)
  
  pre.peak.size <- 0
  post.peak.size = sum(y) - y[peak.at]
  if (peak.at > 1) {
    pre.peak.size = sum(y[1:(peak.at - 1)])
    post.peak.size = sum(y[(peak.at + 1):length(y)])
  }
    
  signature <- list(x=x, y=y
    , peak.at=peak.at
    , max.vol=y[peak.at]
    , adjusted.max.minutes=adjusted.max.minutes
    , alpha=alpha
    , KS.p=KS.p
    , poission.p = pval
    , pre.peak.size = pre.peak.size
    , post.peak.size = post.peak.size
    , x.powerlaw=x.powerlaw
    , y.powerlaw=y.powerlaw    
  )
  signature
}

OriginalWaitTimeProbabilityMatrix <- function (rte) {
  # make a matrix of time deltas for each user in the rte.
  # note that this mat contains the difference in time from each user
  # to each other user
  time.diff.mat <- outer(rte$time.delta, rte$time.delta, FUN="-")
  time.diff.mat <- t(time.diff.mat)
  colnames(time.diff.mat) <- rownames(time.diff.mat) <- rte$screen_name
  #dim(time.diff.mat)
  numcols <- dim(time.diff.mat)[1]
  
  # for any values less than zero use NA
  time.diff.mat[time.diff.mat < 0] <- NA
  # now make a long vector of probabilities using the non-NA values
  # note that I am using the wait time distribution of the real data for this
  #which(as.vector(time.diff.mat) > 14401)
  
  tmp.time.probs <- SimParm.WaitTimes$probability[as.vector(time.diff.mat) + 1]
  
  tmp.time.probs[is.na(tmp.time.probs)] <- 0
  
  if (any(is.na(tmp.time.probs))) { stop ("waitTimeProbabilityMatrix:: NAs introduced into wait time vector.")}
  #length(tmp.time.probs)
  
  # turn the long vector into a mat of probs
  delay.prob.mat <- matrix(tmp.time.probs, numcols, numcols)
  #dim(delay.prob.mat)
  #here is the problem right here. when I set this to zero it zaps the only value in someones column
  
  diag(delay.prob.mat) <- 0
  
  colnames(delay.prob.mat) <- rownames(delay.prob.mat) <- rte$screen_name
  
  if (any(is.na(delay.prob.mat))) { stop("waitTimeProbabilityMatrix:: NAs in matrix")}
  
  delay.prob.mat
}

waitTimeProbabilityMatrix <- function (rte, wait.time.alpha = 2) {
  # make a matrix of time deltas for each user in the rte.
  # note that this mat contains the difference in time from each user
  # to each other user
  
  #time.diffs <- ceiling(rte$time.delta / 60)
  
  time.diffs <- rte$time.delta / 60
  time.diff.mat <- outer(time.diffs, time.diffs, FUN="-")
  time.diff.mat <- t(time.diff.mat)
  colnames(time.diff.mat) <- rownames(time.diff.mat) <- rte$screen_name
  #dim(time.diff.mat)
  numcols <- dim(time.diff.mat)[1]
  
  # note that lower half of mat is expected to have zeros or negative numbers
  # set lower half to zeros
  time.diff.mat[lower.tri(time.diff.mat)] <- NA
  # in this model users retweet from someone else and do not self-retweet
  # so set diag to NA
  diag(time.diff.mat) <- NA
  
  # these are the wait time models used by Gomez-Rodriguez et al., 2010
  # Each cell contains the probability that column user retweeted row user based on time.
  # wait.time.alpha <- 2
  #delay.prob.mat <- time.diff.mat
  #delay.prob.mat <- 1/(time.diff.mat^wait.time.alpha)
  delay.prob.mat <- exp(-time.diff.mat/wait.time.alpha)
  
  #plot(sort(as.vector(delay.prob.mat[upper.tri(delay.prob.mat)])), main=wait.time.alpha)
  
  # now we are setting the probability of self-retweeting and retweeting from someone later to zero
  delay.prob.mat[lower.tri(delay.prob.mat)] <- 0
  diag(delay.prob.mat) <- 0
    
  colnames(delay.prob.mat) <- rownames(delay.prob.mat) <- rte$screen_name
  
  if (any(is.na(delay.prob.mat))) { stop("waitTimeProbabilityMatrix:: NAs in matrix")}
  
  delay.prob.mat
}


senderEffectMatrix <- function(rte, proportion = FALSE) {
  # now we also want to make a sender effects matrix based on 
  # users followers.
  
  if (proportion) {
    receiver.follower.mat <- sapply(rte$follower.proportion, rep, length(rte$follower.proportion))
  } else {
    receiver.follower.mat <- sapply(rte$followers.count.probability, rep, length(rte$followers.count.probability))
  }
  
  sender.follower.mat <- t(receiver.follower.mat)
  
  colnames(sender.follower.mat) <- rownames(sender.follower.mat) <- rte$screen_name
  
  if (any(is.na(sender.follower.mat))) { stop("senderEffectMatrix:: NAs in matrix")}
  
  sender.follower.mat
}

# function takes a probability matrix and returns a matrix with
# all cells zero except where the max row value is for each column
cascadeTreeMatrix <- function(mat) {
  # mat <- delay.prob.mat
  # mat <- edge.prob.mat
  
  numcols <- dim(mat)[1]
  new.mat <- matrix(0, numcols, numcols)
  colnames(new.mat) <- rownames(new.mat) <- colnames(mat)
  
  for (j in 1:numcols) {
    # j <- 1
    if (max(mat[,j]) != 0) {
      max.index <- which.max(mat[,j] )
      #new.mat[max.index,j] <- mat[max.index,j]
      new.mat[max.index,j] <- 1
    } else {
      if (colnames(mat)[1] != colnames(mat)[j]) {
        cat("(", colnames(mat)[1], ") No predecessor for: ", colnames(mat)[j], "\n", sep="")
      }
    }
  }
  
  which(colSums(new.mat) == 0)
  #new.mat[combined.tree.mat > 0] <- 1
  
  if (any(is.na(new.mat))) { stop("cascadeTreeMatrix:: NAs in matrix")}
  
  new.mat
}


# makes a graph object that represents the most probable
# cascade tree using just wait times in the RTE
createWaitTimeTreeGraph <- function (rte, wait.time.alpha = 2) {
  delay.prob.mat <- waitTimeProbabilityMatrix(rte, wait.time.alpha)
  wait.time.tree.mat <- cascadeTreeMatrix(delay.prob.mat)
  
  # which(colSums(wait.time.tree.mat) == 0)
  
  g <- graph.adjacency(wait.time.tree.mat)
  
  V(g)$time.delta <- rte$time.delta
  V(g)$time.minutes <- rte$time.minutes
  V(g)$followers <- rte$followers
  V(g)$follower.proportion <- rte$follower.proportion
  
  #colnames(rte)
  #g$rte.id <- rte$find_rt_id[1]
  #g$closeness.out <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[1], normalized = TRUE)$centralization, 4)
  g.c <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[1], normalized = TRUE)$centralization, 4)
  
  #g
  
  cascade <- list(g=g, rte.id=rte$find_rt_id[1], closeness.out=round(centralization.closeness (g, mode = c("out", "in", "all", "total")[1], normalized = TRUE)$centralization, 4))
  cascade
}



createCombinedTreeGraph <- function (rte, use.follower.proportion = FALSE, wait.time.alpha = 2) {
  delay.prob.mat <- waitTimeProbabilityMatrix(rte, wait.time.alpha)
  #wait.time.tree.mat <- cascadeTreeMatrix(delay.prob.mat)
  
  # the next matrix is a combined probabilities matrix
  # we get this by doing a cell to cell multiplication of the
  # of the delay diffs and the followers measurment    
  if (use.follower.proportion) {
    sender.follower.mat <- senderEffectMatrix(rte, proportion=TRUE)
    
  } else {
    sender.follower.mat <- senderEffectMatrix(rte)
  }
  
  edge.prob.mat <- delay.prob.mat * sender.follower.mat
  combined.tree.mat <- cascadeTreeMatrix(edge.prob.mat)
  
  
  if (any(is.na(combined.tree.mat))) { stop("cascadeTreeMatrix:: NAs in matrix")}
  
  #which(colSums(combined.tree.mat) == 0)
  
  
  
  g <- graph.adjacency(combined.tree.mat)
  #comps <- decompose.graph(g, min.vertices=2)
  #is.connected(g)
  
  #length(rte$time.delta)
  
  V(g)$time.delta <- rte$time.delta
  V(g)$time.minutes <- rte$time.minutes
  V(g)$followers <- rte$followers
  V(g)$follower.proportion <- rte$follower.proportion
  
  #g$rte.id <- rte$find_rt_id[1]
  #g$closeness.out <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[1], normalized = TRUE)$centralization, 4)
  
  #g
  
  cascade <- list(g=g, rte.id=rte$find_rt_id[1], closeness.out=round(centralization.closeness (g, mode = c("out", "in", "all", "total")[1], normalized = TRUE)$centralization, 4))
  cascade
}


# rm(g,l)
plotTree <- function(cascade, main, use.time.axis=FALSE) {
  #name (v/c)
  #time.delta (v/n)
  #time.minutes (v/n)
  #plot(V(g)$follower.proportion)
  
  #names(g)
  g <- cascade$g
  #is.connected(g)
  
  min.node.size <- 1
  max.node.size <- 6
  rte.id <- cascade$rte.id
  closeness.out <- cascade$closeness.out
  origin.user.name <- V(g)$name[1]
  time.vec <- V(g)$time.delta
  
  #g <- simplify(g, remove.loops = TRUE)
  E(g)$width <- .5
  E(g)$color <- "grey30"
    
  #node.size <- 3 + (rte$follower.proportion/max(rte$follower.proportion) * 5)
  #max.local.follower.proportion <- max(V(g)$follower.proportion)
  #node.size <- min.node.size + (V(g)$follower.proportion/max(V(g)$follower.proportion) * (max.node.size - min.node.size))
  #node.size <- round(node.size, 0)
  V(g)$size <- min.node.size
  # length(which(V(g)$followers == 0))
  # if a follower count is zero we get a -inf and that breaks things
  node.size <- min.node.size + round(log10(V(g)$followers + 1),0)
  # node.size <- 1 + node.size/10
  
  V(g)$size <- node.size 
  V(g)$label <- ""
  #V(g)$color <- "white"
  
  general.node.color <- jblue # adjustAlpha("royalblue3", .7) # "deepskyblue3"
  occupy.node.color <- jred # adjustAlpha("orangered3", .7)
  V(g)$color <- general.node.color
  V(g)$color[grep("(OWS|Occupy)", V(g)$name, ignore.case = FALSE)] <- occupy.node.color
  V(g)[V(g)$name == origin.user.name]$color <- "red"
  
  V(g)$frame.color <- NA
  V(g)[V(g)$name == origin.user.name]$frame.color <- "black"
  
  
  #comps <- decompose.graph(g, min.vertices=2)
  
  if (is.connected(g)) {  
    l <- layout.reingold.tilford(g, root=V(g)[V(g)$name == origin.user.name])
    
    V(g)$y <- l[,1]
    
    if (!use.time.axis) {
      #V(g)$y <- l[,2]
      V(g)$x <- l[,2]
      V(g)$x <- abs(V(g)$x - max(V(g)$x))
    } else {
      #y <- abs(time.vec - max(time.vec))
      #V(g)$y <- y
      #V(g)$x <- y
      V(g)$x <- time.vec
    }
    
  } else {
    #l <- layout.fruchterman.reingold(g)
    l <- layout.kamada.kawai(g)
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
  }
  
  if (any(is.nan(l))) { stop("plotTree:: layout returned NANs")}
  
  # par(mar=c(0, 0, 0, 0))
  plot.igraph(g, edge.arrow.size=.2, edge.arrow.width=1, main="")
  
  mtext(text=main, side=3, line=2.1, cex=1.1)
  mtext(text=paste(rte.id, origin.user.name), side=3, line=.9)
  mtext(text=paste("closeness:", closeness.out), side=3, line=-.2)
  
  if (use.time.axis) {
    y.ax <- axTicks(2)
    y.ax <- y.ax + abs(min(y.ax))
    y.ax <- y.ax/max(y.ax)
    #y.labs <- rev(round(max(time.vec) * y.ax, 0))
    y.labs <- round(max(time.vec) * y.ax, 0)
    
    axis(1, axTicks(2), labels=y.labs, cex.axis=.8)
    mtext(text="time in seconds", side=1, line=2)
  }
  
}

NewTreePlot <- function(cascade, main, use.time.axis=TRUE, node.col="lightskyblue", min.node.size=1) {
  
  #cascade <- g.combined.follower.proportion
  g <- cascade$g
  
  min.node.size <- min.node.size
  max.node.size <- 20
  rte.id <- cascade$rte.id
  closeness.out <- cascade$closeness.out
  origin.user.name <- V(g)$name[1]
    
  g.path.length <-  shortest.paths(g, origin.user.name, V(g)[2:vcount(g)])
  V(g)$path <- c(0, as.numeric(g.path.length))
  V(g)$path[!is.finite(V(g)$path)] <- 0
    
  time.vec <- V(g)$time.delta
  
  V(g)$label <- ""
  E(g)$width <- .5
  
  V(g)$size <- min.node.size
  node.size <- min.node.size + round(log10(V(g)$followers),0)
  V(g)$size <- node.size 
  
  #V(g)$color <- "white"
  V(g)$frame.color <- "black"
  V(g)$color <- node.col
  V(g)$color[grep("(OWS|Occupy)", V(g)$name, ignore.case = TRUE)] <- "firebrick3"
  V(g)[V(g)$name == origin.user.name]$frame.color <- "red"
  #V(g)[V(g)$name == origin.user.name]$size <- 10
  
  #rgb.palette <- colorRampPalette(c( "dimgrey", "red"), space = "rgb")
  #my.cols <- rgb.palette(max(V(g)$path))
  
  #E(g)$color <- my.cols[]
  
  E(g)$color <- "dimgrey"
  E(g)[to(V(g)$path > 1)]$color <- "red"
  
  comps <- decompose.graph(g, min.vertices=2)
  
  if (is.connected(g)) {  
    l <- layout.reingold.tilford(g, root=V(g)[V(g)$name == origin.user.name])
    #V(g)$x <- l[,1]
    V(g)$y <- l[,1]
           
    if (!use.time.axis) {
      #V(g)$y <- l[,2]
      V(g)$x <- l[,2]
    } else {
      #V(g)$y <- max(time.vec) - time.vec
      #V(g)$y <- V(g)$y / 60
      #V(g)$x <- max(time.vec) - time.vec
      V(g)$x <- time.vec
      V(g)$x <- V(g)$x / 60
      
      V(g)$y <- max(V(g)$y) - V(g)$y
     
    }
    
  } else {
    l <- layout.fruchterman.reingold(g)
    # l <- layout.kamada.kawai(g)
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
  }
  
  m <- cbind(V(g)$x, V(g)$y)
  l.norm <- layout.norm(m, -1, 1, -1, 1)
  x <- l.norm[1,1]
  y <- l.norm[1,2]
  
  
  if (any(is.nan(l))) { stop("plotTree:: layout returned NANs")}
  
  plot.igraph(g, edge.arrow.size=.2, edge.arrow.width=1, main="")
  points(x, y, col="gold", cex=4, pch=8)
  
  mtext(text=paste("Closeness:", closeness.out), side=3, line=.9, cex=1.1)
  mtext(text=paste("Longest path", max(V(g)$path)), side=3, line=-.1, cex=1.1)
  mtext(text=paste(rte.id, origin.user.name), side=3, line=-1, cex=.9)
  
  time.lables <- pretty(V(g)$y, 5)
  time.lables.y <- rev(seq(from=-1, to=1, length=length(time.lables)))
  time.lables.x <- rep(1.1, length(time.lables))
  
  time.lables <- pretty(V(g)$x, 5)
  time.lables.x <- rev(seq(from=-1, to=1, length=length(time.lables)))
  time.lables.y <- rep(-1.1, length(time.lables))
    
  #text(x=time.lables.x, y=time.lables.y, labels=time.lables, adj=0)
  text(x=time.lables.x, y=time.lables.y, labels=time.lables, adj=c(0,0))
  
  text(x=-.9, y=-.9, labels="minutes", adj=.5)
    
  data.frame(size = vcount(g), longest.path = max(V(g)$path), chains = length(which(V(g)$path > 1)), closeness.out=closeness.out)
}




# names(signature)
plotSignature <- function (signature, rte, tweet.rate.col=rgb(64, 153, 255, maxColorValue=255) # tweet.rate.col="darkgoldenrod1"
  , power.law.line.col="blue1", follower.delta.col="brown1") {
  
  origin.user <- rte$rt_screen_name[1]
  origin.follower.change.time <- rte$time.minutes[-1]
  zap.these.index <- which(origin.follower.change.time > (max(signature$x) + 1))
  if (length(zap.these.index) > 0) {
    origin.follower.change.time <- origin.follower.change.time[-zap.these.index]
  }
  # retweeters.followers <- round(.5 + (log10(rte$followers[-1])/3), 2)
  retweeters.followers <- rte$followers[-1]
  retweeters.followers <- log10(retweeters.followers) + .25
  
  origin.follower.change <- rte$rt_followers[-1]
  if (length(zap.these.index) > 0) {
    origin.follower.change <- origin.follower.change[-zap.these.index]
  }
  origin.follower.change <- origin.follower.change - min(origin.follower.change)
  
  # this line has to be before the final tranformation to get the right, in-window change in followers
  follower.delta <- origin.follower.change[length(origin.follower.change)] - origin.follower.change[1]
  
  origin.follower.change <- max(signature$y) * (origin.follower.change/max(origin.follower.change))
  
  follower.delta.col.vec <- rep(follower.delta.col, length(origin.follower.change))
  
  names.to.check <- rte$screen_name[-1]
  names.to.check <- names.to.check[-zap.these.index]
  retweet.bot.index <- which(names.to.check %in% bot.account.list)
  num.bots <- length(retweet.bot.index)
  if (num.bots > 0) {
    follower.delta.col.vec[retweet.bot.index] <- "black"
  }
  
  tmp.col <- tweet.rate.col
  
  if (max(signature$y) < 10) {
    tmp.col <- "red"
  } else if (max(signature$y) > 100) {
    tmp.col <- "green"
  }
  
  if (is.null(follower.delta.col)) {
    main.1 <- origin.user      
    main.2 <- paste("n: ", dim(rte)[1], ", alpha: ", signature$alpha, " (p=", round(signature$KS.p, 3), ")", sep="")
  } else {
    main.1 <- paste(origin.user, ", alpha ", signature$alpha, " (p=", round(signature$KS.p, 3), ")", sep="")
    main.2 <- paste("Delta Followers ", follower.delta, ", n=", dim(rte)[1], sep="")
  }
  plot(signature$x, signature$y, type="l", bty="n", lwd=3, col=tweet.rate.col
    , main="", xlab="time in minutes", ylab="tweet volume")
  
  mtext(max(signature$y), 3, adj=-.08, las=0, padj=-0.5, cex=2.5, col=tmp.col)
  mtext(text=rte$rt_created_ts[1], side=1, line=2.5, adj=-.1, las=0, padj=-0.5, cex=.8)
  
  mtext(text=main.1, side=3, line=2)
  mtext(text=main.2, side=3, line=.8)
  
  big.timers <- which(rte$followers > 100000)
  
  if (length(big.timers > 0)) {
    if (big.timers[1] == 1) {
      big.timers <- big.timers[-1]
    }
  }
  
  if (length(big.timers > 0)) {
      
    for (j in 1:length(big.timers)) {
      # j <- 1
      big.x <- rep(rte$time.minutes[big.timers[j]], 2)
      big.y <- c(0, (max(signature$y) * .25))
      lines(big.x, big.y)
      text(big.x[1], (max(signature$y) * .5), labels=rte$screen_name[big.timers[j]], srt=90)
    }
  }
  
  
  if (!is.null(power.law.line.col)) {
    lines(signature$x.powerlaw, signature$y.powerlaw, col=power.law.line.col, lwd=2)
    x.text <- max(signature$x)
    y.text <- max(signature$y) * .35
    
    #text(x=x.text, y=y.text, labels="Blue: estimated powerlaw\nRed: delta origin followers", adj=1)  
  }
  
  if (!is.null(follower.delta.col)) {
    points(origin.follower.change.time, origin.follower.change
      , col=follower.delta.col.vec, pch=8, cex=retweeters.followers)
  }
}

plotBotPlot <- function(rte) {
  
  retweet.bot.index <- which(rte$screen_name %in% bot.account.list)
  num.bots <- length(retweet.bot.index)
  
  bot.mins <- as.numeric(names(table(rte$time.minutes[retweet.bot.index])))
  bot.count <- as.numeric(table(rte$time.minutes[retweet.bot.index]))
  sub.main <- paste("Mean bot time: ", mean(bot.mins), " minutes", sep="")
  plot(table(rte$time.minutes), bty="n", col="cadetblue"
    , ylab="tweet vol", xlab="minutes"
    , main=paste("Bot Plot (", num.bots, ")", sep=""))
  mtext(text=sub.main, side=3, line=0)
  points(bot.mins, bot.count, col="red", pch=19, cex=2)
  
}
