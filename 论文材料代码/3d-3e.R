#kegg通路分析
BiocManager::install("clusterProfiler")
library("clusterProfiler")
library("org.Hs.eg.db")
gene<-read.csv("dan没有duo有.txt",header = F)
gene1<-bitr(t(gene),fromType = "SYMBOL",toType = "ENTREZID",OrgDb="org.Hs.eg.db")
sydKEGG <- enrichKEGG(gene=gene$ENTREZID,organism = "hsa", pvalueCutoff = 0.05)
SUMKEGG <-sydKEGG@result
symbolKEGG<- setReadable(sydKEGG, OrgDb = org.Hs.eg.db, keyType="ENTREZID")  #将gene ID转为symbol
symbolKEGG <-symbolKEGG@result 
write.table(symbolKEGG,"enrich",quote = F,row.names = F ,sep = "\t" )

pathway1=sUMbolKEGG[1:15,]
library(ggplot2)
pathway1 <- arrange(pathway1,pathway1[,6])
pathway1$Description <- factor(pathway1$Description,levels = rev(pathway1$Description))

ggplot(pathway1,aes(Count,Description))+geom_point(aes(size=Count,color=p.adjust))+
  scale_color_gradient(high = "#00FFFF",low  = "#104E8B")+
  labs(color=expression(p.adjust),size="Count",x="Count",y="Description")+theme_bw()+
  theme(axis.title=element_text(size=14,face="plain",color="black"),axis.text =element_text(size = 12,face="plain",color="black"),panel.grid  = element_blank())