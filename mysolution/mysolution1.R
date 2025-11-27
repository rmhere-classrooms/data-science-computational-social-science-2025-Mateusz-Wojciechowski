library(igraph)

g <- erdos.renyi.game(p.or.m=0.05, n=100)

# U--- 
# this graph is not weighted 
summary(g)

# listing vertices and edges
print("Vertices:")
V(g)
print("Edges:")
E(g)

# adding weights to the edges
E(g)$weight <- runif(length(E(g)), 0.01, 1)

# U-W- 
# now the graph is weighted
summary(g)


print("Degrees of vertices:")
degree(g)

# creating a histogram
hist(degree(g), 
     main="Distribution of vertices degrees", 
     xlab="Degree", 
     ylab="Amount of vertices",
     col="lightblue")


# after a few runs in most cases there are 1 or 2 cluster
cl <- clusters(g)
print(paste("Amount of clusters:", cl$no))

# computing page rank
pr <- page.rank(g)$vector

plot(g, 
     vertex.size = pr*500,
     vertex.label = NA
)
