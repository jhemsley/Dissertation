library(igraph)
options(scipen=99)

base.dir <- "c:/r/aDiss/descriptivePlots/"

blue <- rgb(80, 165, 255, maxColorValue=255)

star.chain.df1 <- data.frame(
  from=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  , to=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
  )

star.chain.df2 <- data.frame(
  from=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
  , to=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
)

star.chain.df3 <- data.frame(
  from=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
  , to=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
)

star.chain.df4 <- data.frame(
  from=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  1,  1,   1,  1, 1,   1,  1,  1, 1)
  , to=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
)

star.chain.df5 <- data.frame(
  from=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  1,  1,  2,  2,  2,  10, 16, 13, 0, 0)
  , to=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
)

star.chain.df6 <- data.frame(
  from=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  , to=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
)




g1 <- graph.data.frame(star.chain.df1, directed=T)
g2 <- graph.data.frame(star.chain.df2, directed=T)
g3 <- graph.data.frame(star.chain.df3, directed=T)
g4 <- graph.data.frame(star.chain.df4, directed=T)
g5 <- graph.data.frame(star.chain.df5, directed=T)
g6 <- graph.data.frame(star.chain.df6, directed=T)

graph.list <- list(g1, g2, g3, g4, g5, g6)

tmp.v <- rep(NA, 6)
df.closenesses <- data.frame(c.out=tmp.v, c.in=tmp.v, c.all=tmp.v, c.total=tmp.v)

#star.chain.plots <- paste(base.dir, "ChainingOutCloseness.png", sep="")
#png(star.chain.plots, width=1024, height=800, pointsize=20)

star.chain.plots <- paste(base.dir, "ChainingOutCloseness.pdf", sep="")
pdf(star.chain.plots, useDingbats=FALSE) #, width=1024, height=800, pointsize=16)
# pdf(file=star.chain.plots, paper="letter", pointsize=8)
par(mfrow=c(2,3))

for (i in 1:6) {

  # i <- 1 + i
  g <- graph.list[[i]]
  diam <- diameter(g)
  round.to <- 3
  #deg <- round(centralization.degree (g, mode = c("all", "out", "in", "total"),loops = TRUE, normalized = TRUE)$centralization , round.to)
  clos.out <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[1],normalized = TRUE)$centralization, round.to)
  clos.in <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[2],normalized = TRUE)$centralization, round.to)
  clos.all <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[3],normalized = TRUE)$centralization, round.to)
  clos.total <- round(centralization.closeness (g, mode = c("out", "in", "all", "total")[4],normalized = TRUE)$centralization, round.to)
  #bet <- round(centralization.betweenness (g, directed = TRUE, nobigint = TRUE,normalized = TRUE)$centralization, round.to)
  #evec <- round(centralization.evcent (g, directed = FALSE, scale = TRUE,options = igraph.arpack.default, normalized = TRUE)$centralization, round.to)
  
  df.closenesses[i,] <- cbind(clos.out, clos.in, clos.all, clos.total)

  tmp.report <- paste("Diameter ", diam, "\n" #, degree ", deg, "\n"
                      , "Closeness (out): ", clos.out, sep="")
                      #, paste(clos.out, clos.in, clos.all, clos.total, sep=",")
                      #, sep="")

  par(mar=c(5,1,4,1))
  
  if (i < 6) {
    l <- layout.reingold.tilford(g, root="0")
  } else {
    l <- layout.kamada.kawai(g)
  }
  V(g)$x <- l[,1]
  V(g)$y <- l[,2]
  
  #plot(g, edge.arrow.size=.5, edge.arrow.width=1, edge.line.weight=2, vertex.label="")
  
  plot(g, vertex.label=NA, vertex.size=10, vertex.color=blue , vertex.frame.color="white"
  , edge.width=.5, edge.color="black", edge.arrow.size=0, edge.arrow.width=0, edge.curved = F)
  
       #mtext(text="Star and chain measurments", side=3, line=0)
  mtext(text=paste("test graph", i, sep=" "), side=3, line=1, cex=1.2)
  mtext(text=tmp.report, side=1, line=2, cex=1)
}

dev.off()
gc()





