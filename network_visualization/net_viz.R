### This R script plots a few transportation networks.
## All networks downloaded from http://www.bgu.ac.il/~bargera/tntp/
## Some analysis adopted from http://kateto.net/network-visualization

#set a temp working directory.To be removed after switching to 
#setwd('C:/Users/xwang/Documents/GitHub/sandbox/network_visualization/data/Transportation_Networks/')

library(igraph)
library(data.table)
library(RColorBrewer) #display.brewer.all()
library(dplyr)
library(networkD3)

# Network 1: Sioux Fall (Illustrative)
#download and load the Sioux Falls Network
download.file('http://www.bgu.ac.il/~bargera/tntp/SiouxFalls/SiouxFalls_node.zip',
              'SiouxFalls_node.zip')
download.file('http://www.bgu.ac.il/~bargera/tntp/SiouxFalls/SiouxFalls_net.zip',
              'SiouxFalls_net.zip')
download.file('http://www.bgu.ac.il/~bargera/tntp/SiouxFalls/SiouxFalls_flow.zip',
              'SiouxFalls_flow.zip')

# To download files, make sure to have write access on your working directory
Nodes <- data.table(read.delim2(unz('SiouxFalls_node.zip','SiouxFalls_node.txt'), 
                                header = T))
Edges <- data.table(read.delim2(unz('SiouxFalls_net.zip','SiouxFalls_net.txt'), 
                                header = T, skip=7))
Flows <- data.table(read.delim2(unz('SiouxFalls_flow.zip','SiouxFalls_flow.txt'),
                                header = T, dec = '.'))

#delete downloaded files
unlink(c('SiouxFalls_node.zip', 'SiouxFalls_net.zip', 'SiouxFalls_flow.zip'))

#clean up exccessive columns
Nodes <- Nodes[, .(Node, X,Y)]
Edges <- Edges[, 2:(ncol(Edges)-1), with =F]

Edges <- merge(Edges, Flows[, .(From, To, Volume)], 
               by.x = c('Init.node', 'Term.node'),
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

#use the average of total inflow and outflow for each vertex as vertex size
out_flow <- Edges[,.(volume_out = sum(Volume)), by = .(node=Init.node)]
in_flow <- Edges[,.(volume_in = sum(Volume)), by = .(node = Term.node)]
avg_flow <- unlist(merge(out_flow,in_flow, all = T,
                            by = 'node')[, .(weight = (volume_out+volume_in)/2)])

vertex_size <- (avg_flow +2*min(avg_flow))/3500 #add minimums to reduce range 

#adjust edge color to illustrate a shortest path
#returns the indices of selected edeges
hl_path <- unlist(shortest_paths(net, from = V(net)[24], to = V(net)[7], 
                 mode = 'all', weight = E(net)$Free.Flow.Time, 
                 output = 'epath' )$epath)

#adjust color based on whether the edge belongs to the path
edge_color <- ifelse(1:length(E(net)) %in% hl_path, 'gold', 'darkgray')

plot(net, edge.arrow.size = 0.7, edge.color = edge_color, 
     vertex.size = vertex_size,
     vertex.color =  colors[dense_rank(vertex_degree)],
     edge.width = edge_weights, edge.curved = 0, #edge.label = round(E(net)$Volume),
     rescale =F, layout = layout.norm(cbind(X= V(net)$X, Y= V(net)$Y))*1.1, 
     mark.groups=c(1,4,5,8), mark.col="#C5E5E7", mark.border=NA) # mark an area

#make a D3 forcednetwork. Currently difficult to set nodes to fixed postition

#prepare node sets and edge sets
Edges_D3 <- Edges[, .(from = Init.node-1, to = Term.node-1, Value = Free.Flow.Time)]
Nodes_D3 <- Nodes[, .(ID = Node-1, degree = vertex_degree/2)]

forceNetwork(Edges_D3, Nodes_D3, Source = 'from', Target = 'to', Value = 'Value',
             NodeID = 'ID', Group = 'degree', legend =T, width =  500, height = 500,
             zoom = T,  opacity = 0.9)

# Network 2: Chicago Regional (Stress Testing)
#net.m <- net - E(net)[E(net)$type=="hyperlink"]