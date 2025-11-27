library(igraph)

set.seed(123)

g <- barabasi.game(1000)
layout <- layout.fruchterman.reingold(g)
plot(g, layout=layout, vertex.size=2,
     vertex.label=NA, edge.arrow.size=.2)

# computing betweenness and finding most central node according to it
betweenness_values <- betweenness(g)
most_central_node <- which.max(betweenness_values)

# BASED ON THE FIRST RUN:

# the most central node according to betweenness is node 2
# its betweenness value - 602
print(paste("Most central node by betweenness:", most_central_node))
print(paste("Its betweenness value:", betweenness_values[most_central_node]))

# graph's diameter equals 10
diam <- diameter(g)
print(paste("Graph diameter:", diam))

# DIFFERENCES BETWEEN ERDŐS-RÉNYI AND BARABÁSI-ALBERT GRAPHS

# ERDŐS-RÉNYI MODEL:
# - Creation process: N nodes are created, then each pair of nodes is connected with probability p 
# - Degree distribution: most nodes have similar degree around the mean
# - Structure: "Democratic" - every node has equal opportunity for connections
# - No clear hubs - all nodes are roughly equally important
# - Diameter: O(log n)
# - Key characteristic: homogeneous network with uniform connection distribution

# BARABÁSI-ALBERT (BA) MODEL:
# - Creation process: Start with small connected graph, then add nodes one by one.
#   New nodes connect to existing nodes with probability proportional to their degree
# - Degree distribution: few nodes have VERY high degree, most nodes have low degree
# - Structure: "Hierarchical" - clear distinction between hubs and peripheral nodes
# - Strong hubs act as central connectors for the entire network
# - Diameter: O(log log n)
# - Key characteristic: heterogeneous network with hub-dominated structure


# Nice take on the subject from stack overflow about BARABÁSI-ALBERT:) - https://stackoverflow.com/questions/63951391/difference-between-barabási-albert-model-and-erdos-renyi-model

# " Think of it like the current American economy. If you're born rich, you're practically handed money, 
# but if you are born poor you have extra fees. 
# This is known as the matthew effect, from the book of Matthew: 
# "For to everyone who has, more will be given, and to those who have nothing, even that will be taken away". "