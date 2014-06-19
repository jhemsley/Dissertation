
options(scipen=99)

#library(PerformanceAnalytics) # for correlation plot
library(igraph)
library(qgraph)
#library(ellipse)
#library(logging)
#basicConfig(level='FINEST')

jblue <- "dodgerblue4" # rgb(80, 165, 255, maxColorValue=255)
jred <- "firebrick4" # rgb(179, 0, 0, maxColorValue=255)
jgray <- "gray25" # rgb(120, 120, 120, maxColorValue=255)


rte.cutoff.size <- 105
use.occupy.tweets.only <- TRUE
#signature.window <- 60 * 60 * 2
#signature.window <- 60
window.size.in.seconds <- 60 * 60 * 2

powerlaw.fit.implementation <- c("plfit", "R.mle")[2]

created.at.format.string <- "%a %b %d %H:%M:%S +0000 %Y"  #  Sat Jan 28 23:41:29 +0000 2012
alt.date.format <- "%a %b %d %Y %H:%M:%S"            #  Sun Oct 23 2011 06:24:58 GMT+0000 (UTC)
created.ts.format.string <- "%Y-%m-%d %H:%M:%S"           #  2012-01-28 23:41:50

OWS.start.date <- "Sat Sep 17 00:00:00 +0000 2011"
OWS.start.date <- strptime(OWS.start.date, created.at.format.string, tz="GMT")




occupy.time.marker.units <- c("auto", "secs", "mins", "hours", "days", "weeks")[5]



# function to scale up vectors such that the first significant
# digit is in the 1/10 colmun 
scaleToSignif <- function (vec, dig=1) {
  max.value <- max(vec)
  max.value.signif <- signif(max.value, dig)
  raise.to <- nchar(max.value.signif) - 2 - dig
  vec <- vec * 10^raise.to
  vec
}


# if p > 0.05 We conclude that there is no evidence against the null hypothesis that the true distribution is Poisson
# with a mean of about mu:::: given a 'real' ~Poisson, we get high p-values, like .5. So low p-values suggest the dist is not Poission
poissonTest <- function(y) {
  mu <- mean(y)
  D <- sum((y - mu)^2)/mu
  deg.freedom <- length(y) - 1
  pval <- 1 - pchisq(D, deg.freedom)
  ans <- c(D, deg.freedom, pval)
  names(ans) <- c("D", "df", "p-value")
  ans
}


adjustAlpha <- function(col, alpha = 0.5) {
  if (alpha <= 1) {
    alpha <- alpha * 255
  }
  arg <- col2rgb(col)
  rgb(arg[1], arg[2], arg[3], alpha=alpha, maxColorValue=255)
}



# panel.smooth function is built in.
# panel.cor puts correlation in upper panels, size proportional to correlation
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor.test(x, y)$estimate
  p.val <- cor.test(x, y)$p.value
  
  stars <- as.character(symnum(p.val, cutpoints=c(0,0.001,0.01,0.05,1),   
    symbols=c('***', '**', '*', '' ), legend=F))
  
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  cex.cor <- .6 + (4*abs(r))
  text(0.5, 0.33, txt, cex = cex.cor)
  text(0.5, 0.66, stars, cex = .5 + cex.cor, col="brown3")
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  #xpd <- par("xpd")
  #par(xpd=NA)
  par(usr = c(usr[1:2], 0, 1.5) )
  #par(usr = c(0, 1, 0, 1) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y.offset <- 0.02
  y <- h$counts
  y <- (y/max(y)) + y.offset
  #rect(breaks[-nB], y.offset, breaks[-1], y, col = "dodgerblue4", border="dodgerblue4")
  rect(breaks[-nB], y.offset, breaks[-1], y, col = "orange2", border="orange2")
}

my.text.panel <- function(x, label.pos, lab, cex = cex.labels, font = font.labels) {
  usr <- par("usr"); on.exit(par(usr))
  #print(usr)
  #par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, lab, cex=1, srt=-45) 
}