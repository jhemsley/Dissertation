#
#
#
# Note: memory requirements are
# vcount * vcount * attributes
# 41295 * 41295 * 8 /1024 / 1024 / 1024
#
#

library("igraph")
library("rgl")
#library(VGAM)
# time to do some matrix editing
dir.path <- "c:/r/rt_nets/"
dir.path.dat <- "c:/r/rt_nets/dat/"
dir.path.plots <- "C:/r/aDiss/rt_net_plots/" # "c:/r/rt_nets/"

force.new.layouts <- TRUE
single.component <- FALSE
straight.mutual.edges <- FALSE
highlight.mutual.edges <- TRUE
simplify.edges <- TRUE
names.on.bb <- TRUE


left.plot.text.cex.header <- 1.7
left.plot.text.cex <- .8
right.plot.text <- .6
text.offset <- .8
left.text.start.line <- 3
right.text.start.line <- 3

frame.plot.width <- 630
frame.plot.height <- 500
plot.width <- 630
plot.height <- 700

network.plot.bg.color <- "white"
network.plot.fg.color <- "black"

plot.black.bg <- FALSE

if (plot.black.bg) {
  network.plot.bg.color <- "black"
  network.plot.fg.color <- "white"
}

plot.to.pdf <- FALSE
for.pdf.printing <- FALSE

if (plot.to.pdf) {
  plot.to.pdf <- TRUE
  network.plot.bg.color <- "white"
  network.plot.fg.color <- "black"
  single.component <- TRUE
  straight.mutual.edges <- TRUE
}

plot.mut.backbone <- FALSE
plot.DrL.default.set <- FALSE
plot.DrL.experimentation <- FALSE
plot.kamada.kawai <- TRUE
plot.fruchterman.reingold <- FALSE
plot.fixed.backbone <- FALSE
plot.reingold.tilford <- FALSE
plot.bipartite <- FALSE
plot.community.detection <- FALSE
fruchterman.reingold.3d <- FALSE
plot.overview <- FALSE

include.script <- paste(dir.path, "network.data.frame.R", sep="")
source(include.script)
colnames(network.meta.stuff.df)

dim(network.meta.stuff.df)
tmp.opts <- par()

par(mfrow=c(3,3))

num.nets <- dim(network.meta.stuff.df)[1]
for (i in 1:num.nets) {
  # i <- 1
  tmp.col <- c(network.meta.stuff.df$white.bg.node.col[i]
              , network.meta.stuff.df$white.bg.link.col[i]
              , network.meta.stuff.df$black.bg.node.col[i]
              , network.meta.stuff.df$black.bg.link.col[i])
  pie(rep(10,4), main=network.meta.stuff.df$net.name[i], col=tmp.col
      , labels = c("white.bg.node", "white.bg.link", "black.bg.node", "black.bg.link")
  )
}

par(tmp.opts)

comparitive.network.meta.stuff.df <- data.frame(
  start.date=rep(NA, num.nets)
  , end.date=rep(NA, num.nets)
  , num.nodes=rep(0, num.nets)
  , num.edges=rep(0, num.nets)
  , components=rep(0, num.nets)
  , largest.component=rep(0, num.nets)
  , second.component=rep(0, num.nets)
  , clusters=rep(0, num.nets)
  , largest.cluster=rep(0, num.nets)
  , second.cluster=rep(0, num.nets)
  , diameter=rep(0, num.nets)
  , density=rep(0, num.nets)
  , degree.cent=rep(0, num.nets)
  , close.cent=rep(0, num.nets)
  , bet.cent=rep(0, num.nets)
  , evec.cent=rep(0, num.nets)
  )

comparitive.network.meta.stuff.df <- cbind(network.meta.stuff.df, comparitive.network.meta.stuff.df)
colnames(comparitive.network.meta.stuff.df)
# rows in file, diamater, density, nodes, links, clusters, largets cluster  

network.choice <- 7
for (network.choice in 1:8) {
  network.name <- network.meta.stuff.df$net.name[network.choice]
  tweet.data.f.name <- network.meta.stuff.df$name.file[network.choice]
  black.bg.node.col <- network.meta.stuff.df$black.bg.node.col[network.choice]
  black.bg.link.col <- network.meta.stuff.df$black.bg.link.col[network.choice]
  white.bg.node.col <- network.meta.stuff.df$white.bg.node.col[network.choice]
  white.bg.link.col <- network.meta.stuff.df$white.bg.link.col[network.choice]
  plot.title.1 <- paste("#", network.name, " retweet network", sep="")
  
  bg.msg <- "w"
  mutual.link.col <- white.bg.link.col
  if (network.plot.bg.color == "black") {
    mutual.link.col <- black.bg.link.col
    bg.msg <- "b"
  }
  
  if (highlight.mutual.edges) {
    tmp.col <- col2rgb(mutual.link.col, alpha=T)
    mutual.link.col <- rgb2hsv(tmp.col[1], tmp.col[2], tmp.col[3])
    if (network.plot.bg.color == "black") {
      #mutual.link.col <- "white"
      mutual.link.col <- hsv(mutual.link.col[1], .06, .99)
    } else {
      #mutual.link.col <- "black"
      mutual.link.col <- hsv(mutual.link.col[1], .11, .11)
    }
    #adjustcolor(col, alpha.f = .5)
  }
    
  #pie(rep(10,2), col=c(mutual.link.col, black.bg.link.col))
  
  cat(network.name, tweet.data.f.name, "\n", sep=" ")
  
  tweet.data.file <- paste(dir.path.dat, tweet.data.f.name, sep="")
    
  #  should have these feilds
  # "row.names"                   "id"                          "created_ts"                 
  # "user.screen_name"            "rt.id_str"                   "rt.created_at"              
  # "rt.user.screen_name"         "rt.user.followers_count"     "rt.user.friends_count"      
  # "rt.user.following"           "rt.user.follow_request_sent" "text" 
  tweet.data <- data.frame(read.delim(file=tweet.data.file, sep='\t', stringsAsFactors=F, quote="", row.names=NULL))
  class(tweet.data)
    
  colnames(tweet.data)
  dim(tweet.data)
  tweet.data[1,] 
  
  #### 
  #
  # Fix dates as dates
  #
  # NOTE that the date formats for rt dates are different than for tweet dates
  #
  #tweet.data.new <- data.frame(id=tweet.data$id)
  tmp.created_ts <- as.POSIXct(strptime(tweet.data$created_ts, "%a %b %e %Y %H:%M:%S GMT%z (UTC)"), tz="UTC")
  tmp.created_ts[1:4]
  tweet.data$created_ts <- tmp.created_ts
  tweet.data$created_ts[1:4]
  
  # sort data set by date time
  o <- order(tmp.created_ts)
  tweet.data <- tweet.data[o,]
    
  tmp.rt.created_at <- as.POSIXct(strptime(tweet.data$rt.created_at, "%a %b %e %H:%M:%S %z %Y"), tz="UTC")
  tmp.rt.created_at[1:4]
  tweet.data$rt.created_at <- tmp.rt.created_at
  tweet.data$rt.created_at[1:4]
  
  tmp.start.date <- min(tweet.data$created_ts)
  zap <- which(difftime(tweet.data$created_ts, tmp.start.date, units="days") > 7)
  length(zap)
  
  tweet.data <- tweet.data[-zap,]
  
  
  comparitive.network.meta.stuff.df$start.date[network.choice] <- min(tweet.data$rt.created_at)
  comparitive.network.meta.stuff.df$end.date[network.choice] <- max(tweet.data$rt.created_at)
  network.start.date <- min(tweet.data$rt.created_at)
  network.end.date <- max(tweet.data$rt.created_at)  
  cat(as.character(network.start.date), as.character(network.end.date), "\n", sep=" ")
  
  
  #####################################
  #
  #  Network
  #
  ####################################
  #  should have these feilds
  # "row.names"                   "id"                          "created_ts"                 
  # "user.screen_name"            "rt.id_str"                   "rt.created_at"              
  # "rt.user.screen_name"         "rt.user.followers_count"     "rt.user.friends_count"      
  # "rt.user.following"           "rt.user.follow_request_sent" "text" 
  tweet.data$user.screen_name[2]

  
  rt.network.edgelist <- data.frame(from=tweet.data$user.screen_name, to=tweet.data$rt.user.screen_name)  
  dim(rt.network.edgelist)
  
  g <- graph.data.frame(rt.network.edgelist, directed=TRUE)
  el <- get.edgelist(g) # need this here for later. Don't simplify network before calling this
  num.nodes <- vcount(graph=g)
  
  if (simplify.edges) {
    E(g)$weight <- 1
    g <- simplify(g, edge.attr.comb="sum")
    summary(E(g)$weight)
  }
  
  comparitive.network.meta.stuff.df$num.nodes[network.choice] <- vcount(graph=g)
  comparitive.network.meta.stuff.df$num.edges[network.choice] <- ecount(graph=g)

  comps <- decompose.graph(g, min.vertices=2)
  comparitive.network.meta.stuff.df$components[network.choice] <- length(comps)
  comparitive.network.meta.stuff.df$largest.component[network.choice] <- sort(sapply(comps, diameter), decreasing=T)[1]
  
  network.cluster.object <- clusters(g, mode="strong") # weak clusters are roughly the same as components
  network.cluster.object$no
  if (network.cluster.object$no > 1) {
    comparitive.network.meta.stuff.df$second.component[network.choice] <- sort(sapply(comps, diameter), decreasing=T)[2]
  }
  comparitive.network.meta.stuff.df$clusters[network.choice] <- network.cluster.object$no
  comparitive.network.meta.stuff.df$largest.cluster[network.choice] <- sort(network.cluster.object$csize, decreasing=T)[1]
  if (network.cluster.object$no > 1) {
    comparitive.network.meta.stuff.df$second.cluster[network.choice] <- sort(network.cluster.object$csize, decreasing=T)[2]
  }
  
  comparitive.network.meta.stuff.df$diameter[network.choice] <- diameter(g)
  comparitive.network.meta.stuff.df$density[network.choice] <- graph.density(g, loops=FALSE)
  comparitive.network.meta.stuff.df$degree.cent[network.choice] <- centralization.degree (g, mode = c("all", "out", "in", "total"),loops = TRUE, normalized = TRUE)$centralization 
  comparitive.network.meta.stuff.df$close.cent[network.choice] <- centralization.closeness (g, mode = c("out", "in", "all", "total"),normalized = TRUE)$centralization
  comparitive.network.meta.stuff.df$bet.cent[network.choice] <- centralization.betweenness (g, directed = TRUE, nobigint = TRUE,normalized = TRUE)$centralization
  comparitive.network.meta.stuff.df$evec.cent[network.choice] <- centralization.evcent (g, directed = FALSE, scale = TRUE,options = igraph.arpack.default, normalized = TRUE)$centralization
  
  comparitive.network.meta.stuff.df[8,]
  
  #plot(centralization.evcent (g, directed = FALSE, scale = TRUE,options = igraph.arpack.default, normalized = TRUE)$vector)
  
  ########################################
  #
  # Do single component plots?
  #
  ###############################
  
  
  #plot(sort(sapply(comps, diameter), decreasing=T), bty="n", col=white.bg.node.col
  #     , main="Components", xlab=network.name, ylab="weight")
  #dev.flush() 
    
  if (single.component) {
    g <- comps[[1]] # largest connected component
  }
  
  deg.evec <- centralization.evcent (g, directed = FALSE, scale = TRUE,options = igraph.arpack.default, normalized = TRUE)$vector
  
  g <- permute.vertices(g, rank(deg.evec, ties.method="first"))
  
  
  

  
  ########################################
  #
  # Data & plot for backbone network
  #
  ###############################
  # el <- get.edgelist(g) # this needs called right after the network is created in order to find weights
  
  mutual.edge.list <- el[is.mutual(graph.data.frame(rt.network.edgelist, directed=TRUE)), ]
    
  num.mutual.ties <- dim(mutual.edge.list)
  num.mutual.ties
  mutual.tie.actors <- sort(table(c(mutual.edge.list[,1], mutual.edge.list[,1])))
  mutual.tie.actor.names <- names(mutual.tie.actors)
  mutual.edge.index <- which(is.mutual(g)) # gives index for edges
  rt.backbone.edgelist <- cbind(from=mutual.edge.list[,1], to=mutual.edge.list[,2])
  g.b.b <- graph.data.frame(rt.backbone.edgelist, directed=TRUE)
  
  E(g.b.b)$weight <- 1
  g.b.b <- simplify(g.b.b, edge.attr.comb="sum")
  summary(E(g.b.b)$weight)
  
  edge.line.widths <- E(g.b.b)$weight - min(E(g.b.b)$weight)
  edge.line.widths <- edge.line.widths/max(edge.line.widths)
  edge.line.widths <- ((10 - .3) * edge.line.widths) + .3
  summary(edge.line.widths)
      
  E(g.b.b)$width <- edge.line.widths
  E(g.b.b)$color <- white.bg.link.col
    
  V(g.b.b)$label.color <- rgb(0,0,0,.8)
  
  if (names.on.bb) {
    V(g.b.b)$label <- V(g.b.b)$name  
  }
  
  reg.ex.string <- "(Occupy|OWS)"
  occ.folks.index <- grep(reg.ex.string, as.character(V(g.b.b)$name), ignore.case=TRUE)
  
  V(g.b.b)$label.cex <- 1  
  V(g.b.b)[occ.folks.index]$label.cex <- 1.5
  
  if (vcount(g.b.b) > 1000) {
    V(g.b.b)$label.cex <- .5
    V(g.b.b)[occ.folks.index]$label.cex <- 1
  }
  
  V(g.b.b)$frame.color <- NA # "white" # node.col # "white"
  V(g.b.b)$size <- 0
  V(g.b.b)$color <- rgb(0,0,0,0)
  
  if (plot.mut.backbone) {
    g.b.b.l <- layout.fruchterman.reingold(g.b.b, niter=500, area=vcount(g.b.b)^2, repulserad=vcount(g.b.b)^2.7, maxdelta = vcount(g.b.b), coolexp = 3)
    tmp.layout.name <- "fruchterman.reingold"
    string.roation.degs <- 0
    V(g.b.b)$x <- g.b.b.l[,1]
    V(g.b.b)$y <- g.b.b.l[,2]
    
    V(g.b.b)$norm.x <- layout.norm(g.b.b.l, -1, 1, -1, 1)[,1]
    V(g.b.b)$norm.y <- layout.norm(g.b.b.l, -1, 1, -1, 1)[,2]
    
    net.plot.f.name <- paste(dir.path.plots, network.name, "_mutual-ties_backbone_", tmp.layout.name, sep="")
    cat(network.name, " (", network.choice, "): Plotting back-bone to ", net.plot.f.name, ", as PDF: ", plot.to.pdf, ", at ", as.character(Sys.time()), "\n", sep="")
    plot.main.title <- paste ("Mutual-ties backbone for ", network.name, sep="")
    
    if (plot.to.pdf) {
      pdf(paste(net.plot.f.name, ".pdf", sep=""), width=8.5, height=11, title=net.plot.f.name, pointsize=1/300, paper="letter", compress=F)
    } else {
      png(paste(net.plot.f.name, ".png", sep=""), width=plot.width, height=plot.height)
    }
    
    par(mar=c(2,0,2,0), srt=string.roation.degs) ##  c(bottom, left, top, right)
    
    plot.time <- system.time(
      plot.igraph(g.b.b, edge.arrow.size=0, edge.arrow.width=0, edge.curved = F
                  , main=plot.main.title, xlab="")
    )
    mtext(text=tmp.layout.name, side=1, line=0)
    
    dev.off()
    cat("     device off at ", as.character(Sys.time()), "\n\n")
  }
  gc()
    
  

  ##############################################################################
  ##
  ## Setting plot visual values
  ##
  ##########################################
  V(g)$frame.color <- NA # "white" # node.col # "white"
  V(g)$label <- ""
  
  V(g)$size <- 1
  V(g)$color <- white.bg.node.col
  
  E(g)$curved <- .5
  E(g)$width <- .5
  E(g)$color <- white.bg.link.col
  
  if (network.plot.bg.color == "black") {
    V(g)$color <- black.bg.node.col
    E(g)$color <- black.bg.link.col
  }
  
  if (straight.mutual.edges) {
    E(g)[which(is.mutual(g))]$curved <- 0.01
    E(g)[which(is.mutual(g))]$color <- mutual.link.col
    E(g)[which(is.mutual(g))]$width <- 1
  }
  
  gc()
  
  
 
  
  #######################################################
  #
  # kamada.kawai setup
  #  
  
  far.appart.dudes <- ""
  if (names.on.bb) {
    far.appart.dudes.index <- farthest.nodes (g, directed = TRUE, unconnected = TRUE, weights = NULL)    
    far.appart.dudes <- paste("Farthest nodes: ", paste(V(g)[far.appart.dudes.index]$name, collapse=", "), sep="")
  }
  tmp.layout.name <- "kamada.kawai"
  
  larest.clusters <- sort(network.cluster.object$csize, decreasing=T)[1]
  if (network.cluster.object$no > 2) {
    larest.clusters <- paste(sort(network.cluster.object$csize, decreasing=T)[1:3], collapse=",")
  }
  
  color.message <- paste ("nodes: ", white.bg.node.col, ", links: ", white.bg.link.col, sep="")
  if (network.plot.bg.color == "black") { 
    color.message <- paste ("nodes: ", black.bg.node.col, ", links: ", black.bg.link.col, sep="")
  }
  
  left.plot.meta.vec <- c(
    paste("#", network.name, " retweet network", sep="")
    , paste("data: ", tweet.data.file, sep="")
    , paste("layout: ", tmp.layout.name, sep="")
    , paste("Plot size: ", plot.width, "x", plot.height, sep="")
    , color.message    
    , paste(network.start.date, " - ", network.end.date, sep="")
    , paste("Plot largest component: ", single.component, sep="")
    , paste("Straight mutual edges: ", straight.mutual.edges, sep="")
    , paste(vcount(g), " nodes, ", ecount(g), " edges, ", network.cluster.object$no, " clusters, 3 largest:", larest.clusters,  sep="")
    , paste(diameter(g), " diameter, ", graph.density(g, loops=FALSE), " density", sep="")
    , far.appart.dudes
  )
  
  # right side
  mutual.tie.actors.report <- sort(mutual.tie.actors, decreasing=T)
  mutual.tie.actors.report[1:5]
  mutual.ties.list <- min(length(mutual.tie.actors.report), 50)
  right.plot.meta.vec <- rep("", mutual.ties.list + 1)
  right.plot.meta.vec[1] <- "Straight links: reciprocol ties\n"
  #right.plot.meta.vec[2] <- paste(right.plot.meta, "Node sizes: num reciprocol ties\n", sep="")
  #right.plot.meta.vec[3] <- paste(right.plot.meta, "Largest to smallest (top 50):\n", sep="") 
  for (j in 2:(mutual.ties.list + 1)) {
    right.plot.meta.vec[j] <- paste(names(mutual.tie.actors.report)[j], ": ", mutual.tie.actors.report[j], sep="")
  }
  
  ##
  # kamada.kawai
  #
  if (plot.kamada.kawai) {
    
    net.plot.f.name <- paste(dir.path.plots, network.name, "_", tmp.layout.name, "_", plot.width, "x", plot.height, bg.msg, sep="")
    net.plot.f.name
    
    net.layout.file <- paste(dir.path.dat, network.name, "_", tmp.layout.name, "_", vcount(g), "n_", ecount(g), "e.csv", sep="")    
    found.saved.layout <- file.exists(net.layout.file)
    
    if (force.new.layouts || !found.saved.layout) {
      cat(network.name, " (", network.choice, "): starting kamada.kawai layout at ", as.character(Sys.time()), "\n")
      t <- system.time(
        l <- layout.kamada.kawai(g)
      )
      cat("   kamada.kawai elapsed time ", round(t[3], 2), ", at ", as.character(Sys.time()), "\n", sep="")
      
      write.table(l, file=net.layout.file, append=FALSE, quote=F, sep=",", eol="\n", na="NA", dec="."
                  , row.names=FALSE, col.names=TRUE, qmethod=c("escape", "double"), fileEncoding = "UTF8")
      cat("   Created saved layout file: ", net.layout.file, "\n", sep="")
      
    } else {
      l <- as.matrix(read.delim(file=net.layout.file, check.names=T, sep=',', header=T, stringsAsFactors=F))
    }
    
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
    
    cat(network.name, " (", network.choice, "): Plotting kamada.kawai to ", net.plot.f.name, ", as PDF: ", plot.to.pdf, ", at ", as.character(Sys.time()), "\n", sep="")   
    if (plot.to.pdf) {
      pdf(paste(net.plot.f.name, ".pdf", sep=""), width=8.5, height=11, title=net.plot.f.name, pointsize=1/300, paper="letter", compress=F)
    } else {
      png(paste(net.plot.f.name, ".png", sep=""), width=plot.width, height=plot.height)
    }
      par(mar=c(0,0,5,0)) ##  c(bottom, left, top, right)
      if (network.plot.bg.color == "black") { 
        par(bg="black") 
      }
      
      t2 <- system.time(
        plot.igraph(g, edge.arrow.size=0.0, edge.arrow.width=0.0)
      )
    
      left.plot.meta.vec.final <- c(left.plot.meta.vec
                            , paste("layout time: ", round(t[3], 2), " seconds", sep="")
                            , paste("Plot time: ", round(t2[3], 2), " seconds", sep="")
      )

    
      mtext(left.plot.meta.vec.final[1], side=3, line=left.text.start.line, cex=left.plot.text.cex.header, adj=.01, col=network.plot.fg.color)
      tmp.line <- left.text.start.line - 1.1 * left.plot.text.cex
      for (j in 2:length(left.plot.meta.vec.final)) {
        mtext(left.plot.meta.vec.final[j], side=3, line=tmp.line, cex=left.plot.text.cex, adj=.01, col=network.plot.fg.color)
        tmp.line <- tmp.line - text.offset
      }
      
      tmp.line <- right.text.start.line
      if (names.on.bb) {
        for (j in 1:mutual.ties.list) {
          mtext(right.plot.meta.vec[j], side=3, line=tmp.line, cex=right.plot.text, adj=0.99, col=network.plot.fg.color)
          tmp.line <- tmp.line - text.offset
        }
      }
    
    dev.off()
    cat("     device off at ", as.character(Sys.time()), "\n\n")
  } # end kamada.kawai

  
  
  
  
  #######################################################
  #
  # fruchterman.reingold setup
  #  
  
  if (plot.fruchterman.reingold) {
    
    net.plot.f.name <- paste(dir.path.plots, network.name, "_", tmp.layout.name, "_", plot.width, "x", plot.height, bg.msg, sep="")
    net.plot.f.name
    
    net.layout.file <- paste(dir.path.dat, network.name, "_", tmp.layout.name, "_", vcount(g), "n_", ecount(g), "e.csv", sep="")    
    found.saved.layout <- file.exists(net.layout.file)
    
    if (force.new.layouts || !found.saved.layout) {
      cat(network.name, " (", network.choice, "): starting kamada.kawai layout at ", as.character(Sys.time()), "\n")
      t <- system.time(
        l <- layout.kamada.kawai(g)
      )
      cat("   kamada.kawai elapsed time ", round(t[3], 2), ", at ", as.character(Sys.time()), "\n", sep="")
      
      write.table(l, file=net.layout.file, append=FALSE, quote=F, sep=",", eol="\n", na="NA", dec="."
                  , row.names=FALSE, col.names=TRUE, qmethod=c("escape", "double"), fileEncoding = "UTF8")
      cat("   Created saved layout file: ", net.layout.file, "\n", sep="")
      
    } else {
      l <- as.matrix(read.delim(file=net.layout.file, check.names=T, sep=',', header=T, stringsAsFactors=F))
    }
    
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
    
    cat(network.name, " (", network.choice, "): Plotting kamada.kawai to ", net.plot.f.name, ", as PDF: ", plot.to.pdf, ", at ", as.character(Sys.time()), "\n", sep="")   
    if (plot.to.pdf) {
      pdf(paste(net.plot.f.name, ".pdf", sep=""), width=8.5, height=11, title=net.plot.f.name, pointsize=1/300, paper="letter", compress=F)
    } else {
      png(paste(net.plot.f.name, ".png", sep=""), width=plot.width, height=plot.height)
    }
    par(mar=c(0,0,5,0)) ##  c(bottom, left, top, right)
    if (network.plot.bg.color == "black") { 
      par(bg="black") 
    }
    
    t2 <- system.time(
      plot.igraph(g, edge.arrow.size=0.0, edge.arrow.width=0.0)
    )
    
    left.plot.meta.vec.final <- c(left.plot.meta.vec
                                  , paste("layout time: ", round(t[3], 2), " seconds", sep="")
                                  , paste("Plot time: ", round(t2[3], 2), " seconds", sep="")
    )
    
    mtext(left.plot.meta.vec.final[1], side=3, line=2.5, cex=2, adj=.01, col=network.plot.fg.color)
    tmp.line <- 1
    for (j in 2:length(left.plot.meta.vec.final)) {
      mtext(left.plot.meta.vec.final[j], side=3, line=tmp.line, cex=1.3, adj=.01, col=network.plot.fg.color)
      tmp.line <- tmp.line - 1.2
    }
    
    tmp.line <- 0
    if (names.on.bb) {
      for (j in 1:mutual.ties.list) {
        mtext(right.plot.meta.vec[j], side=3, line=tmp.line, cex=.8, adj=0.99, col=network.plot.fg.color)
        tmp.line <- tmp.line - 1.2
      }
    }
    
    dev.off()
    cat("     device off at ", as.character(Sys.time()), "\n\n")
  } # end plot.fruchterman.reingold
  
  
  
  
  #################################################################
  #
  # reingold.tilford backbone
  #
  #
  ###############################################################
  if (plot.reingold.tilford) {
    tmp.layout.name <- "reingold.tilford"
    g.tmp <- g.b.b   
    V(g.tmp)$frame.color <- NA # "white" # node.col # "white"
    V(g.tmp)$label <- V(g.tmp)$name
    V(g.tmp)$label.color <- "red" # rgb(0,0,0,.8)
    V(g.tmp)$label.cex <- .5
    V(g.tmp)$size <- 0.1
    V(g.tmp)$color <- rgb(1,1,1,1)
    
    #V(g)[which(tolower(V(g)$name) == tolower(network.name))]$name
    root.user <- V(g.tmp)[which(tolower(V(g.tmp)$name) == tolower(network.name))]
    if (length(root.user) < 1) {
      root.user <- 1  
    }
    
    t <- system.time(
      #l <- layout.reingold.tilford(g.tmp, root=root.user)
      l <- layout.reingold.tilford(g.tmp)
    )
    cat("   reingold.tilford elapsed time ", round(t[3], 2), ", at ", as.character(Sys.time()), "\n", sep="")
    
    V(g.tmp)$x <- l[,1]
    V(g.tmp)$y <- l[,2]   
    
    string.roation.degs <- 45
    
    net.plot.f.name <- paste(dir.path.plots, network.name, "_", tmp.layout.name, "_", plot.width, "x", plot.height, bg.msg, sep="")
    net.plot.f.name
    cat(network.name, " (", network.choice, "): Plotting reingold.tilford to ", net.plot.f.name, ", as PDF: ", plot.to.pdf, ", at ", as.character(Sys.time()), "\n", sep="")   
    if (plot.to.pdf) {
      pdf(paste(net.plot.f.name, ".pdf", sep=""), width=8.5, height=11, title=net.plot.f.name, pointsize=1/300, paper="letter", compress=F)
    } else {
      png(paste(net.plot.f.name, ".png", sep=""), width=plot.width, height=plot.height)
    }
    
    
    par(mar=c(2,0,2,0), srt=string.roation.degs, xpd=NA) ##  c(bottom, left, top, right)
    
    plot.time <- system.time(
      plot.igraph(g.tmp, edge.arrow.size=0, edge.arrow.width=0, edge.curved = F, main="", xlab=tmp.layout.name)  # label.degree=-pi/4
    )
    mtext(plot.title.1, side=3, line=0, cex=2, adj=.01, col=network.plot.fg.color)
    
    mtext(text=paste("Root user: ", root.user, sep=""), side=3, line=2.5, cex=1, adj=.01, col=network.plot.fg.color)
    mtext(text=tmp.layout.name, side=1, line=0)
  
    
    dev.off()
    
  }
  
  
  
  #################################################################
  #
  # experiment with fixed backbone
  #
  #
  ###############################################################
  
  #seed <- matrix(runif(vcount(g) * 2), ncol = 2)
  #summary(seed)
  
  if (plot.fixed.backbone && straight.mutual.edges) {
    
    V(g)$fixed <- FALSE
    V(g)$x <- 0
    V(g)$y <- 0
      
    # Complicated, but, get g$name in g.b.b$name for the index of the items in
    # g$name that match in g.b.b$name 
    g.in.g.b.b.index <- which (V(g)$name %in% V(g.b.b)$name)
    # now, get the match of g$name in g.b.b$name and remove NAs
    g.in.g.b.b.match <- match(V(g)$name, V(g.b.b)$name)
    g.in.g.b.b.match <- g.in.g.b.b.match[!is.na(g.in.g.b.b.match)]
    # these lists match by order
    V(g)$name[g.in.g.b.b.index]
    V(g.b.b)$name[g.in.g.b.b.match]
    
    V(g)$fixed[g.in.g.b.b.index] <- TRUE
    V(g)[which(V(g)$fixed)]$name
    
    V(g)[g.in.g.b.b.index]$x <- V(g.b.b)[g.in.g.b.b.match]$norm.x
    V(g)[g.in.g.b.b.index]$y <- V(g.b.b)[g.in.g.b.b.match]$norm.y
    
    seed <- matrix(c(V(g)$x, V(g)$y), ncol = 2, byrow=F)
    cat(network.name, " (", network.choice, "): starting Fixed BB DrL layout at ", as.character(Sys.time()), "\n")
    t <- system.time(
      l <- layout.drl(g, options=igraph.drl.experiment, use.seed=T, seed=seed, fixed=V(g)$fixed )
    )
    cat("   DrL elapsed time ", round(t[3], 2), ", at ", as.character(Sys.time()), "\n")
    V(g)$x <- l[,1]
    V(g)$y <- l[,2]
    
    V(g)$frame.color <- NA # "white" # node.col # "white"
    V(g)$label <- ""
    
    V(g)$size <- .8
    #V(g)$color <- white.bg.node.col
    
    E(g)$curved <- .5
    E(g)$width <- 0.3
    #E(g)$color <- white.bg.link.col
    
    
    E(g)[which(is.mutual(g))]$curved <- .01
    #E(g)[which(is.mutual(g))]$color <- mutual.link.col
    
    mut.weights <- E(g)[which(is.mutual(g))]$weight
    mut.edge.line.widths <- (5 * (mut.weights/max(mut.weights))) + 2
    E(g)[which(is.mutual(g))]$width <- mut.edge.line.widths
    
    #plot(E(g)[which(is.mutual(g))]$width, col=E(g)[which(is.mutual(g))]$color)
    
    #plot.igraph(g, edge.arrow.size=0.0, edge.arrow.width=0.0)
    
    igraph.drl.experiment <- igraph.drl.default
    tmp.layout.name <- "DrL_default_Fixed_BB"
    plot.title.2 <- "DrL Fixed BB"
    plot.title.3 <- paste(num.nodes, " nodes, ", ecount(g), " edges", sep="")
    bg.msg <- "w"
    if (network.plot.bg.color == "black") { 
      bg.msg <- "b"
    }
    
    net.plot.f.name <- paste(dir.path.plots, network.name, "_", tmp.layout.name, "_", plot.width, "x", plot.height, bg.msg, sep="")
    net.plot.f.name
    
    cat(network.name, " (", network.choice, "): Plotting fixed-backbone DrL to ", net.plot.f.name, ", as PDF: ", plot.to.pdf, ", at ", as.character(Sys.time()), "\n", sep="")   
    if (plot.to.pdf) {
      pdf(paste(net.plot.f.name, ".pdf", sep=""), width=8.5, height=11, title=net.plot.f.name, pointsize=1/300, paper="letter", compress=F)
    } else {
      png(paste(net.plot.f.name, ".png", sep=""), width=plot.width, height=plot.height)
    }
      if (network.plot.bg.color == "black") { 
        par(bg="black") 
      }
      par(mar=c(0,0,5,0)) ##  c(bottom, left, top, right)
      t2 <- system.time(
        plot.igraph(g, edge.arrow.size=0.00, edge.arrow.width=0.00)
      )
      
      mtext(plot.title.1, side=3, line=2.5, cex=2, adj=.01, col=network.plot.fg.color)
      mtext(plot.title.2, side=3, line=1, cex=1.3, adj=.01, col=network.plot.fg.color)
      mtext(plot.title.3, side=3, line=-.5, cex=1.1, adj=.01, col=network.plot.fg.color)
      mtext(paste("layout time: ", round(t[3], 2), " seconds"), side=3, line=-1.5, cex=1.1, adj=.01, col=network.plot.fg.color)
      mtext(paste("Plot time: ", round(t2[3], 2), " seconds"), side=3, line=-2.5, cex=1.1, adj=.01, col=network.plot.fg.color)
    
    dev.off()
    cat("     device off at ", as.character(Sys.time()), "\n\n")
  }
  
   
  
  #################################################################
  #
  # Additional Info Plots
  #
  #
  ###############################################################  
  
  # plot.overview <- FALSE
  if (plot.overview) {
    all.folks <- unique(c(tweet.data$user.screen_name, tweet.data$rt.user.screen_name))
    tweeters.table <- table(tweet.data$user.screen_name)
    retweeted.table <- table(tweet.data$rt.user.screen_name)
    
    all.folks.df <- data.frame (users=all.folks, tweets=rep(0, length(all.folks), retweets=rep(0, length(all.folks))))
    
    # index of items in all.folks that match in tweeters.table
    all.in.tweeters.index <- which (all.folks %in% names(tweeters.table))
    all.in.retweeted.index <- which (all.folks %in% names(retweeted.table))
    # now get match of all.folks in tweeters.table and remove nas
    all.in.tweeters.match <- match (all.folks, names(tweeters.table))
    all.in.tweeters.match <- all.in.tweeters.match[!is.na(all.in.tweeters.match)]
    all.in.retweeted.match <- match (all.folks, names(retweeted.table))
    all.in.retweeted.match <- all.in.retweeted.match[!is.na(all.in.retweeted.match)]
    
    all.folks.df$tweets[all.in.tweeters.index] <- tweeters.table[all.in.tweeters.match] 
    all.folks.df$retweets[all.in.retweeted.index] <- retweeted.table[all.in.retweeted.match]
    
    x <- all.folks.df$tweets
    y <- all.folks.df$retweets
    
    net.plot.f.name <- paste(dir.path.plots, network.name, "_overview_", bg.msg, sep="")
    net.plot.f.name
    
    cat(network.name, " (", network.choice, "): Plotting final report to ", net.plot.f.name, ", as PDF: ", plot.to.pdf, ", at ", as.character(Sys.time()), "\n", sep="")   
    
    if (plot.to.pdf) {
      pdf(paste(net.plot.f.name, ".pdf", sep=""), width=8.5, height=11, title=net.plot.f.name, pointsize=1/300, paper="letter", compress=F)
    } else {
      png(paste(net.plot.f.name, ".png", sep=""), width=plot.width, height=plot.height)
    }
      #if (network.plot.bg.color == "black") { 
      #par(bg="black") 
      #}
    
      par(mfrow=c(3,2))
      par(xpd=NA)
    
      plot(sort(network.cluster.object$csize, decreasing=T), bty="n", col=white.bg.node.col
         , main="Cluster report", xlab=network.name, ylab="cluster size")
  
      plot(sort(sapply(comps, diameter), decreasing=T), bty="n", col=white.bg.node.col
         , main="Components", xlab=network.name, ylab="weight")
    
      plot(x,y, bty="l", col=white.bg.node.col
           , xlab="tweets", ylab="retweets", main="user activity")
    
      plot.igraph(g.b.b, edge.arrow.size=0.00, edge.arrow.width=0.0, edge.curved = F, main=network.name)
      mtext(text="backbone", side=1, line=0)
      
      plot(sort(E(g.b.b)$weight, decreasing=T), bty="n", col=white.bg.node.col
         , main="BB edge weights", xlab=network.name, ylab="weight")
    
      
      plot(centralization.evcent (g, directed = FALSE, scale = TRUE,options = igraph.arpack.default, normalized = TRUE)$vector
           , bty="n", col=white.bg.node.col
           , main="Eigenvector Cent Dist", xlab=network.name, ylab="weight")
    
    dev.off()
    cat("     device off at ", as.character(Sys.time()), "\n\n")
  } # end plot.overview
}


