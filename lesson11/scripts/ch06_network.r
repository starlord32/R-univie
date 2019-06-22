# Load libraries
library(igraph)
library(RColorBrewer)
options(width=70)

OUTDIR = "../img/ch06/"
dir.create(OUTDIR,FALSE, TRUE)

# Simple example with the British Monarchy
g = graph.empty(directed=TRUE)
g = g + vertex("Elizabeth II")
g = g + vertex("Philip")
g = g + vertex("Charles")
g = g + vertex("Diana")
g = g + vertex("William")
g = g + vertex("Harry")
g = g + vertex("Catherine")
g = g + vertex("George")

g = g + edges("Elizabeth II", "Charles")
g = g + edges("Philip", "Charles")
g = g + edges("Charles", "William")
g = g + edges("Diana", "William")
g = g + edges("Charles", "Harry")
g = g + edges("Diana", "Harry")
g = g + edges("William", "George")
g = g + edges("Catherine", "George")

set.seed(1)
pdf(paste0(OUTDIR, "royalFamily.pdf"), 6, 6)
par(mar=c(0,3,0,3))
V(g)$color = rgb(0,0,1,0.1)
plot.igraph(g, vertex.label.cex=2)
dev.off()

lout=matrix(c(1,3,3,3,2,2,3,2,1,1,3,1,2,1,1,0),8,2,TRUE)

pdf(paste0(OUTDIR, "royalFamilyTree.pdf"), 6, 6)
par(mar=c(0,3,0,3))
V(g)$color = rgb(0,0,1,0.1)
plot.igraph(g, layout=lout, vertex.label.cex=2)
dev.off()

# Read in the Supreme court data and create a simple graph
options(stringsAsFactors=FALSE)
allCounts = as.matrix(read.csv("../data/ch06/ac.csv"))
themes = read.csv("../data/ch06/themes.csv")

G = graph.edgelist(allCounts, directed=FALSE)

# Limit to desgregation themes:
tSet = unique(themes$usid[themes$issue %in% c(20040, 20050)])
H = induced.subgraph(G, tSet)
n = igraph::vcount(H)

set.seed(1)
pdf(paste0(OUTDIR, "casesLabels.pdf"), 8, 8)
par(mar=c(0,0,0,0))
plot.igraph(H, vertex.size=1, vertex.label.cex=0.8, edge.width=0.1)
dev.off()

# Plot eigenvalue centrality
vals = evcent(H)[[1]]
bins = quantile(vals, seq(0,1,0.05))
vals = cut(vals, bins, labels=FALSE, include.lowest=TRUE)

V(H)$color = rev(heat.colors(20))[vals]

set.seed(1)
pdf(paste0(OUTDIR, "eigenSupremeCourt.pdf"), 8, 8)
par(mar=c(0,0,0,0))
plot.igraph(H, vertex.label=NA, vertex.size=5)
dev.off()

# Plot betweeness centrality
vals = betweenness(H)
bins = unique(quantile(vals, seq(0,1,0.05)))
vals = cut(vals, bins, labels=FALSE, include.lowest=TRUE)

V(H)$color = rev(heat.colors(length(bins)))[vals]

set.seed(1)
pdf(paste0(OUTDIR, "betweennessSupremeCourt.pdf"), 8, 8)
par(mar=c(0,0,0,0))
plot.igraph(H, vertex.label=NA, vertex.size=5)
dev.off()

# Plot betweeness crossed with eigenvecotr centrality
betweenCent = betweenness(H)
eigenCent = evcent(H)$vector
colorVals = rep("white", length(betweenCent))
colorVals[which(eigenCent < 0.36 & betweenCent > 200)] = "red"
V(H)$color = colorVals

set.seed(1)
pdf(paste0(OUTDIR, "betweenVertexSupremeCourt.pdf"), 8, 8)
par(mar=c(0,0,0,0))
plot.igraph(H, vertex.label=NA, vertex.size=5)
dev.off()

# Identify major communities
w = edge.betweenness.community(H)
V(H)$color = rep("white", length(w$membership))
keepTheseCommunities = names(sizes(w))[sizes(w) > 3]
matchIndex = match(w$membership, keepTheseCommunities)
colorVals = rainbow(5)[matchIndex[!is.na(matchIndex)]]
V(H)$color[!is.na(matchIndex)] = colorVals

set.seed(1)
pdf(paste0(OUTDIR, "clustersSupremeCourt.pdf"), 8, 8)
par(mar=c(0,0,0,0))
plot.igraph(H, vertex.label=NA, vertex.size=5)
dev.off()
