library(igraph)
library(ggplot2)
library(reshape2)
print("box data successfully\n")
dan=read.csv("dan.0.1.csv.edge.csv",header=T)
print("import data successfully\n")
g_dan = graph_from_data_frame(dan, directed=FALSE)
E(g_dan)$weight = dan$weight
print("weight data successfully\n")
dan_GN<-cluster_edge_betweenness(g_dan,weights = E(g_dan)$weight)
dan_GN_label=matrix(nrow = length(get.vertex.attribute(g_dan)[[1]]), ncol = 2)
dan_GN_label<-as.data.frame(dan_GN_label)
dan_GN_label[,1]=get.vertex.attribute(g_dan)[[1]]
dan_GN_label[,2]=dan_GN$membership
dan_GN_label_list <- list()
for(i in 1:length(dan_GN)){
  dan_GN_label_list[[i]]<-dan_GN_label[dan_GN_label$V2==i,1]
}
save( dan_GN_label_list,file = "GN_dan.Rdata")
