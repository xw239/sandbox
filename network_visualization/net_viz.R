### This R script plots a few transportation networks.
## All networks downloaded from http://www.bgu.ac.il/~bargera/tntp/
## Some analysis adopted from http://kateto.net/network-visualization

#set a temp working directory.To be removed after switching to 
setwd('C:/Users/xwang/Documents/GitHub/sandbox/network_visualization/data/Transportation_Networks/')

library(igraph)
library(data.table)
library(RColorBrewer)
library(dplyr)#display.brewer.all()
library(networkD3)
library(animation)

# Network 1: Sioux Fall (Illustrative)
#Read in the Sioux Falls Network
Nodes <- data.table(read.delim2('SiouxFalls_node.txt', header = T))
Edges <- data.table(read.delim2('SiouxFalls_net.txt', header = T, skip=7))
Flows <- data.table(read.delim2('SiouxFalls_flow.txt', header = T, dec = '.'))
#clean up exccessive columns
Nodes <- Nodes[, .(Node, X,Y)]
Edges <- Edges[, 2:(ncol(Edges)-1), with =F]

Edges <- merge(Edges, Flows[, .(From, To, Volume)], by.x = c('Init.node', 'Term.node'),
               by.y = c('From', 'To'))  

rm(Flows)
#create the graphics
net <- graph.data.frame(Edges, vertices = Nodes, directed = T)
#We can access the edge and vertices of the network
E(net)
V(net)
E(net)$Speed.limit
V(net)$X

#plotting the network
#basic plot. The default setting does not assume fix coordinates, and will produce 
# different network configurations randomly if called multiple times
plot(net, edge.arrow.size = 0.5)

#we can also supply a set of fixed coordinates for the vertices, commonly 
#available for graphs with geo-spatial interpretations. Graph with coordinates
#will be plotted with a deterministic layout.
plot(net,layout = cbind(X= V(net)$X, Y= V(net)$Y),edge.arrow.size = 0.7)

# use  predfined layouts
plot(net,layout = layout.circle(net),edge.arrow.size = 0.7)
plot(net,layout=layout.sphere(net),edge.arrow.size = 0.7)

# plotting with more graphics parameters
#use volume as edge width
edge_weights <- scale(E(net)$Volume, center =F)*6

#use vertex degree as vertex color
vertex_degree <- degree(net)
colors <- brewer.pal(length(unique(vertex_degree)), "Set3")

#use the average of inflow and outflow as vertex size
out_flow <- Edges[,.(volume_out = sum(Volume)), by = .(node=Init.node)]
in_flow <- Edges[,.(volume_in = sum(Volume)), by = .(node = Term.node)]
avg_flow <- unlist(merge(out_flow,in_flow, all = T,
                            by = 'node')[, .(weight = (volume_out+volume_in)/2)])
vertex_size <- (avg_flow +2*min(avg_flow))/3000 #add minimums to reduce range 

plot(net, edge.arrow.size = 0.7, edge.color = 'darkgray', 
     vertex.size = vertex_size,
     vertex.color =  colors[dense_rank(vertex_degree)],
     edge.width = edge_weights, edge.curved = 0,
     rescale =F,
     layout = layout.norm(cbind(X= V(net)$X, Y= V(net)$Y))*1.2)

#display.brewer.pal(length(unique(vertex_degree)), "Set3")

# Network 2: Chicago Regional (Stress Testing)
#net.m <- net - E(net)[E(net)$type=="hyperlink"]
#plot(net, mark.groups=c(1,4,5,8), mark.col="#C5E5E7", mark.border=NA)
#news.path <- get.shortest.paths(net, V(net)[media=="MSNBC"],
 #                               V(net)[media=="New York Post"],
  #                              mode="all", output="both")