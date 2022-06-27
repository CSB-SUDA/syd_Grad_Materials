#基因通路图
data=data1
data=read.csv("DEG_0.05.csv",header = T)
data$log2FoldChange=scale(data$log2FoldChange)
colnames(gene1)=c("gene")
c=merge(gene1,data,by="gene")
genelist=data$log2FoldChange
names(genelist)<-as.character(data$gene)
names(genelist)=data$gene
head(genelist)
genelist<-sort(genelist,decreasing=TRUE)
cnetplot(symbolKEGG,colorEdge = TRUE,circular=F,categorySize=Count,foldChange = genelist,showCategory =1)
