# Part0
library("lda")
library("igraph")
library("networkD3")
data("sampson")





# Part1
# A static plot using igraph::plot.igraph()
samplik <- sampson$SAMPLK1
g_samplik <- graph_from_adjacency_matrix(samplik, 
                                         mode = "directed",
                                         diag = FALSE, 
                                         weighted = NULL,
                                         add.colnames = NULL,
                                         add.rownames = NA
                                         )
vertex_attr_names(g_samplik)
g_samplik |>
  plot.igraph(vertex.label = V(g_samplik)$name)
# An interactive plot using networkD3::forceNetwork()
# Links: One row = one edge (relationship). There must be at least two columns for the start and end points, with indexing starting from 0.
# Nodes: One row = one node (person/monk). There must be at least one column for a readable name or ID.
samplik
nodes <- data.frame(name = rownames(samplik))
nodes
links <- data.frame(source = integer(), target = integer(), value = numeric())
# Traverse each cell in the Matrix
for(i in 1:nrow(samplik)){
  for (j in 1:ncol(samplik)){
      links <- rbind(links, data.frame(source=i-1, target=j-1, value = samplik[i,j]))
  }
}
#Plot
plot <-forceNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  Group = "name"
)
print(plot)





# Part2
samplik2 <- sampson$SAMPLK2
# Out-degree
outgree <- rowSums(samplik2)
# In-degree
ingree <- colSums(samplik2)
#mean tie strength
mean(sampson$SAMPLK2[sampson$SAMPLK2 > 0])
#Store measures in a list
samplik2_measures_list <- list(outgree = rowSums(samplik2),
                                 ingree =colSums(samplik2),
                                 mean = mean(samplik2[samplik2>0]))

# Make a barplot of the in-degree statistic.
barplot(height = samplik2_measures_list$ingree)
# Which monk is most liked? Which monk is least liked?
sort(samplik2_measures_list$ingree)






#Part 3 
# Write a function that will generate a network according to the rule that each monk assigns their likes
# (or dislikes) completely at random. This function should be general enough to allow the user to generate
# a network of any size. (Hint: for loops can be helpful here)

gen_network_adjacency_matrix <- function(nSize){
  # generate probabilities for each person.
  runif_nums <- runif(nSize, min = 0, max = 1)
  probs <- runif_nums /sum(runif_nums)
  #print(probs)
  cat("Sum: ", sum(probs))
  
  mat <- matrix(data = rep(0, nSize * nSize), 
                nrow = nSize, 
                dimnames = list(paste("MONK", 1:nSize, sep="_"), paste("MONK", 1:nSize, sep="_")))
  for (i in 1:ncol(mat)){
    top_three <- sample((1:nSize)[-i], 3, replace = TRUE, prob = probs[-i])
    mat[i, top_three[1]] <- 3
    mat[i, top_three[2]] <- 2
    mat[i, top_three[3]] <- 1
  }              
  
  mat
}

samplik_sim <- gen_network_adjacency_matrix(18)
print(samplik_sim)





#Par4
#Create nodes from adjacency matrix
nodes <- data.frame(name = rownames(samplik_sim))
#Create links from adjacency matrix
links <- data.frame(source = integer(), target = integer(), value = numeric())

for(i in 1:nrow(samplik_sim)){
  for (j in 1:ncol(samplik_sim)){
    if (samplik[i,j] > 0){
      links <- rbind(links, data.frame(source=i-1, target=j-1, value = samplik[i,j]))
    }
  }
}


plot <- forceNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  Group = "name"
)

print(plot)