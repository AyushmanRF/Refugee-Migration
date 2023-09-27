# Set the working directory
dirpath <- "\uploads"
setwd(dirpath)

# Load the required library
library(igraph)

# Read the CSV data
refugee <- read.csv("Refugee_Migration.csv", header = TRUE, sep = ",")

# Create a directed graph
g_refugee <- graph.data.frame(refugee, directed = TRUE, vertices = NULL)

# Use the Largest Connected Component
g.decompose <- decompose(g_refugee)
g.refugee <- g.decompose[[1]]

# Calculate edge and vertex counts
ecount.full <- c("Edge Count Full", ecount(g.refugee))
vcount.full <- c("Vertex Count Full", vcount(g.refugee))

# Check if the graph is simple
is.simple(g.refugee)

# Community detection on an undirected graph
g_refugee_commun <- graph.data.frame(refugee, directed = FALSE, vertices = NULL)
g.decompose <- decompose(g_refugee_commun)
g.refugee.commun.undir <- g.decompose[[1]]

# Simplify the graph
g.refugee.commun.undir <- simplify(g.refugee.commun.undir)

# Community Detection using Fast Greedy
g.refugee.fast <- fastgreedy.community(g.refugee.commun.undir, weights = E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex <- 0.3

# Plot the graph
plot(g.refugee.fast, g.refugee.commun.undir, vertex.color = "purple", vertex.frame.color = "#ffffff",
     vertex.size = 3, edge.width = E(g.refugee.commun.undir)$weight/5, edge.arrow.size = 0.3,
     vertex.label.color = "black", edge.color = adjustcolor("purple", alpha.f = 0.4))
title("Fast greedy Algorithm")

# Get community membership
c.m.fast <- membership(g.refugee.fast)

# Community Detection using Walktrap
g.refugee.walktrap <- walktrap.community(g.refugee.commun.undir, step = 6, weights = E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex <- 0.3

plot(g.refugee.walktrap, g.refugee.commun.undir, vertex.color = "purple", vertex.frame.color = "#ffffff",
     vertex.size = 3, edge.width = E(g.refugee.commun.undir)$weight/5, edge.arrow.size = 0.3,
     vertex.label.color = "black", edge.color = adjustcolor("purple", alpha.f = 0.4))
title("WalkTrap Algorithm")

c.m.walktrap <- membership(g.refugee.walktrap)

# Community Detection using Spinglass
g.refugee.spinglass <- spinglass.community(g.refugee.commun.undir, spins = 60, weights = E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex <- 0.3

plot(g.refugee.spinglass, g.refugee.commun.undir, vertex.color = "purple", vertex.frame.color = "#ffffff",
     vertex.size = 3, edge.width = E(g.refugee.commun.undir)$weight/5, edge.arrow.size = 0.3,
     vertex.label.color = "black", edge.color = adjustcolor("purple", alpha.f = 0.4))
title("Spinglass Algorithm")

c.m.spinglass <- membership(g.refugee.spinglass)


# Community Detection using Label Propagation
g.refugee.label <- label.propagation.community(g.refugee.commun.undir, weights = E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex <- 0.3

plot(g.refugee.label, g.refugee.commun.undir, vertex.color = "purple", vertex.frame.color = "#ffffff",
     vertex.size = 3, edge.width = E(g.refugee.commun.undir)$weight/5, edge.arrow.size = 0.3,
     vertex.label.color = "black", edge.color = adjustcolor("purple", alpha.f = 0.4))
title("Label Propagation Algorithm")

c.m.label <- membership(g.refugee.label)


# Community Detection using Girvan-Newman
g.refugee.gn <- edge.betweenness.community(g.refugee.commun.undir, weights = E(g.refugee.commun.undir)$weight)
V(g.refugee.commun.undir)$label.cex <- 0.3

plot(g.refugee.gn, g.refugee.commun.undir, vertex.color = "purple", vertex.frame.color = "#ffffff",
     vertex.size = 3, edge.width = E(g.refugee.commun.undir)$weight/5, edge.arrow.size = 0.3,
     vertex.label.color = "black", edge.color = adjustcolor("purple", alpha.f = 0.4))
title("Community Detection using Girvan-Newman")

c.m.gn <- membership(g.refugee.gn)
