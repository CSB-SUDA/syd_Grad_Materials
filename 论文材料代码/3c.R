data=read.csv("RegLogEx-danci.csv",header = T)

library(igraph)
library(ggpubr)
g = graph_from_data_frame(data1, directed=FALSE)
degree_single=strength(g,weights = data1$weight)
data1=read.table("duoMat_0.edges.txt",header = T)
g1 = graph_from_data_frame(data2, directed=FALSE)
degree_multiple=strength(g1,weights = data2$weight)
boxplot_data_degree=data.frame(para="Degree",group=c(rep("Single",length(degree_single)),rep("Multiple",length(degree_multiple))),
                        value=c(degree_single,degree_multiple))
boxplot_data_degree$value=log2(boxplot_data_degree$value+1)
boxplot_data_degree$value=(boxplot_data_degree$value-min(boxplot_data_degree$value))/(max(boxplot_data_degree$value)-min(boxplot_data_degree$value))

 #¿½±´
 library(igraph)
 g = graph_from_data_frame(data, directed=FALSE)
 degree_single=betweenness(g,weights = data$weight)
 g1 = graph_from_data_frame(data1, directed=FALSE)
 degree_multiple=betweenness(g1,weights = data1$weight)
 boxplot_data_betweenness=data.frame(para="betweenness",group=c(rep("single_radiation",length(degree_single)),rep("multiple_radiation",length(degree_multiple))),
                         value=c(degree_single,degree_multiple))
boxplot_data_betweenness$value=log2(boxplot_data_betweenness$value+1)
boxplot_data_betweenness$value=(boxplot_data_betweenness$value-min(boxplot_data_betweenness$value))/(max(boxplot_data_betweenness$value)-min(boxplot_data_betweenness$value))
 
 
 
 library(igraph)
 g = graph_from_data_frame(data, directed=FALSE)
 degree_single=closeness(g,weights = data$weight,normalized = T)
 data1=read.csv("duo0.45.csv",header = T)
 g1 = graph_from_data_frame(data1, directed=FALSE)
 clo_multiple=closeness(g1,weights = data1$weight,normalized = T)
 boxplot_data_closeness=data.frame(para="closeness",group=c(rep("single_radiation",length(degree_single)),rep("multiple_radiation",length(degree_multiple))),
                                     value=c(degree_single,degree_multiple))
 boxplot_data_closeness$value=log2(boxplot_data_closeness$value+1)
 
 g = graph_from_data_frame(data, directed=FALSE)
 transitivity_single=transitivity(g,weights = data$weight,type = "local")
 g1 = graph_from_data_frame(data1, directed=FALSE)
 transitivity_multiple=transitivity(g1,weights = data1$weight,type = "local")
 boxplot_data_transitivity=data.frame(para="Transitivity",group=c(rep("single_radiation",length(transitivity_single)),rep("multiple_radiation",length(transitivity_multiple))),
                                   value=c(transitivity_single,transitivity_multiple))
 
 boxplot_data_transitivity=boxplot_data_transitivity[boxplot_data_transitivity$value!="NaN",]
 boxplot_data_transitivity$value=log2(boxplot_data_transitivity$value+1)
 boxplot_data_transitivity$value=(boxplot_data_transitivity$value-min(boxplot_data_transitivity$value))/(max(boxplot_data_transitivity$value)-min(boxplot_data_transitivity$value))
 
 g = graph_from_data_frame(data, directed=FALSE)
evcent_single=evcent(g,weights = data$weight)
evcent_single=evcent_single$vector
 g1 = graph_from_data_frame(data1, directed=FALSE)
evcent_multiple=evcent(g1,weights = data1$weight)
evcent_multiple=evcent_multiple$vector
 boxplot_data_evcent=data.frame(para="evcent",group=c(rep("single_radiation",length(evcent_single)),rep("multiple_radiation",length(evcent_multiple))),
                                      value=c(evcent_single,evcent_multiple))
 
 boxplot_data_evcent$value=log2(boxplot_data_evcent$value+1)
 
boxplot_data_degree$group=factor(boxplot_data_degree$group,level=c("single_radiation","multiple_radiation"))
 boxplot_data$laber=paste0(boxplot_data$para,boxplot_data$group)
 boxplot_data=rbind(boxplot_data_degree,boxplot_data_betweenness,boxplot_data_transitivity)
 boxplot_data$group=factor(boxplot_data_degree$group,levels = c("single_radiation",'multiple_radiation'))
  
ggviolin(data =  data,x="group",y="value",fill = "group",
          palette =  c("#7CAE00",'#00BFC4',"#F8766D"),
          add.params = list(fill = "white"),width =1.3,alpha=1)+geom_boxplot(width = 0.15, fill = "#E8E8E8", notchwidth=0.5)+
     stat_compare_means(comparisons = my_comparisons,label.y = c(14,14.2,14.4))
  
   
    stat_compare_means(aes(group =group,label = sprintf("p=%5.4f", as.numeric(..p.format..))),
                      label = "p.format",
                      method = "t.test",label.y = 1.15,
                      hide.ns = T,size = 4)+ggsignif::geom_signif(annotations = rep("",1),
                                                                   xmin = seq(0.85,3+.85-1),
                                                                   y_position = 1.1,
                                                                   xmax = seq(1.15,3+1.15-1),
                                                                   tip_length = rep(0,2*1),
                                                                   size = 0.5)
ylab("protein length") + xlab("group")


my_comparisons=list(c("C","S"),c("C","M"),c("S","M"))
