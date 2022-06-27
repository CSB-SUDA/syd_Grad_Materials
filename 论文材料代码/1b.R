data=read.csv("RegLogEx.csv",header = T,row.names = 1)
data=data[rowSums(data==0)<round(ncol(data)*0.8),] 
library(reshape2)
library(ggplot2)
library(factoextra)
exprData<-data
data_t <- t(data)
variableL <- ncol(data_t)
pca <- prcomp(data_t, scale=T)
print(str(pca))
fviz_eig(pca, addlabels = TRUE)
fviz_pca_ind(pca, repel=T) 
A=stringr::str_extract(rownames(data_t),"[A-Za-z0-9]*")
A[A=="Control"]="Ctrl"
A[A=="R1"]="R1-05"
A[A=="R25"]="R25-05"
A=factor(A,levels = c("Ctrl","R1-05","R25-05"))
fviz_pca_ind(pca, col.ind=A, mean.point=F, addEllipses = T, legend.title="Groups",
             geom = "point",palette= c("#008B00",'#104E8B',"#CD2626"),title = "                                    PCA主成分分析")

library(stringr)
library(ggplot2)
library(ggsci)
library(ggbiplot)
install.packages("ggbiplot")
BiocManager::install("ggbiplot")
library(devtools)
install_github("vqv/ggbiplot")


