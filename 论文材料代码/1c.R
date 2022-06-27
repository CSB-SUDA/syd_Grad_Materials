library(stringr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
ourRMSD=function(data){
  RMSDmatrix<-matrix(0,nrow = ncol(data),ncol =ncol(data))
  rownames(RMSDmatrix)<-colnames(data)
  colnames(RMSDmatrix)<-colnames(data)
  for(i in 1:ncol(data)){
    for(j in 1:ncol(data)){
   RMSDmatrix[i,j]<-sqrt(sum((data[,i]-data[,j])^2)/nrow(data)) 
    } 
  }
  return(RMSDmatrix)
}
data <- read.table(file = "TCGA-LUAD.htseq_counts.tsv.gz",header = T,row.names = 1,check.names = F)
data <- round(2^data-1)
pheotype <- read.delim(file = "TCGA-LUAD.GDC_phenotype.tsv.gz",header = T)
samples <- intersect(colnames(data),pheotype$submitter_id.samples)
pheotype <- pheotype[match(samples,pheotype$submitter_id.samples),]
ourdata <- read.csv(file = "count.csv")
ourdata<-ourdata[,c("gene","sin_P10_1","sin_P10_2","sin_P10_3",
                    "mul_P10_1","mul_P10_2","mul_P10_3")]
dataannotation <- read.table(file = "gencode.v22.annotation.gene.probeMap",header = T)
data <-data[,samples]
pheotype$tumor_stage.diagnoses=gsub("[ab]$","",pheotype$tumor_stage.diagnoses)
data <- data[,pheotype$tumor_stage.diagnoses=="stage iv"]
data <- data[,substr(colnames(data),14,15)!="11"]
databackup <- data
data<-databackup
for(i in 1:300){
  data <- databackup[,sample(1:ncol(databackup),10,replace = F)]
  
  data <- rownames_to_column(data,var="Ensembl_ID")
  data <- inner_join(data,dataannotation,by=c("Ensembl_ID" = "id"))
  data <- inner_join(data,ourdata,by="gene")
  data <- data[,str_detect(colnames(data),"TCGA|_P")]
  data <- edgeR::cpm(data,log = T)
  dataCor <- ourRMSD(data)
  ggplot2table <- reshape2::melt(dataCor[(ncol(data)-5):ncol(data),1:(ncol(data)-6)])
  ggplot2table$group <- substr(ggplot2table$Var1,1,3)
  ggplot2table$group<-factor(ggplot2table$group,levels = c("sin","mul"))
p2=ggplot(ggplot2table,aes(x=group,y=value,fill=group))+ ##设置图形的纵坐标横坐标和分组
  stat_boxplot(mapping=aes(x=group,y=value,fill = group),
               geom ="errorbar",                             ##添加箱子的bar为最大、小值
               width=0.2,position=position_dodge(0.5))+     ##bar宽度和组间距
    geom_boxplot(aes(fill=group),                             ##分组比较的变量
                 position=position_dodge(0.8),                 ##因为分组比较，需设组间距
                 width=0.55,                                    ##箱子的宽度
                 outlier.color = "white")+                     ##离群值的颜色
    stat_summary(mapping=aes(group=group),                    ##分组计算的变量
                 fun="mean",                                   ##箱线图添加均值
                 geom="point",shape=23,size=3,fill="white",    ##均值图形的设置
                 position=position_dodge(0.8))+                ##因为分组比较，需设置两组间距
    scale_fill_manual(values=c('#104E8B',"#CD2626"))+theme_bw()+theme(panel.grid = element_blank()
                                                                                                          ,axis.text=element_text(face = "bold",size = 11,colour = "black"))+
    stat_compare_means(aes(group=group),method = "wilcox.test",label="p.signif",comparisons = list(c("sin","mul")),label.x.npc = 0.5,exact = FALSE)
ourp <- wilcox.test(ggplot2table[ggplot2table$group=="sin",3],ggplot2table[ggplot2table$group=="mul",3])$p.value
cat(paste0(i,"====",ourp,"\n"))
if(ourp<0.05){
  ggsave(p2,filename = "cor.pdf",width = 5,height = 4)
  break
 }
}

  
 
  
  
  

 