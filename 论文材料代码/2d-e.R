pathway1=sUMbolKEGG[1:15,]
library(ggplot2)
pathway1 <- arrange(pathway1,pathway1[,6])
pathway1$Description <- factor(pathway1$Description,levels = rev(pathway1$Description))

ggplot(pathway1,aes(Count,Description))+geom_point(aes(size=Count,color=p.adjust))+
scale_color_gradient(high = "#00FFFF",low  = "#104E8B")+
labs(color=expression(p.adjust),size="Count",x="Count",y="Description")+theme_bw()+
  theme(axis.title=element_text(size=14,face="plain",color="black"),axis.text =element_text(size = 12,face="plain",color="black"),panel.grid  = element_blank())


data1=read.csv("RegLogEx.csv",header = T)                                               
data2=read.csv("bet_mul.csv",header = T)
data_sin=data1[,2]                                                                                                                
data_sin=log2(data_sin)
