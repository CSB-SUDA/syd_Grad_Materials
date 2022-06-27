library(WGCNA)
library(reshape2)
library(stringr)
options(stringsAsFactors = FALSE);
enableWGCNAThreads()
DATA=read.csv("matrix.csv",header = T)
dim(DATA)
names(DATA)
datExpr0 = as.data.frame(t(DATA))
names(datExpr0)=DATA$Gene
rownames(datExpr0) = names(DATA)


type = "unsigned"
corType = "pearson"
corFnc = ifelse(corType=="pearson", cor, bicor)
maxPOutliers = ifelse(corType=="pearson",1,0.05)
robustY = ifelse(corType=="pearson",T,F)

#data frame
datExpr1=datExpr0[2:13,]
datExpr2=as.data.frame(lapply(datExpr1,as.numeric))

# Re-cluster samples
dist=dist(datExpr1,method = "euclidean")
sampleTree1 = hclust(dist, method = "complete")
sizeGrWindow(12,9)
par(cex = 0.6);
par(mar = c(0,4,2,0))
plot(sampleTree1, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,cex.axis = 1.5, cex.main = 2)
plclust(sampleTree1)
plotDendroAndColors(sampleTree1),main = ("Sample dendrogram")
abline(k=3, col = "red");

save(datExpr2,  file = "radData-duo-01-dataInput.Rdata")
clust = cutreeStatic(sampleTree1, cutHeight = 15, minSize = 10)
table(clust)
keepSamples = (clust==1)
datExpr = datExpr0[keepSamples, ]
nGenes = ncol(datExpr2)
nSamples = nrow(datExpr2)
# Display the current working directory
getwd();
# If necessary, change the path below to the directory where the data files are stored.
# "." means current directory. On Windows use a forward slash / instead of the usual \.
workingDir = ".";
setwd(workingDir);
# Load the WGCNA package
library(WGCNA)
# The following setting is important, do not omit.
options(stringsAsFactors = FALSE);
# Allow multi-threading within WGCNA. This helps speed up certain calculations.
# At present this call is necessary for the code to work.
# Any error here may be ignored but you may want to update WGCNA if you see one.
# Caution: skip this line if you run RStudio or other third-party R environments.
# See note above.
enableWGCNAThreads()
# Load the data saved in the first part
lnames = load(file = "radData-dan-01-dataInput.Rdata");
#The variable lnames contains the names of loaded variables.
lnames
# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=20,y=2))
# Call the network topology analysis function
sft = pickSoftThreshold(datExpr2, powerVector = powers, verbose = 5)
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));
cex1 = 0.9;

# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.80,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

#¹¹½¨ÍøÂç
# Co-expression similarity and adjacency
softPower =16;
adjacency = adjacency(datExpr2, power = 16);
# Turn adjacency into topological overlap
TOM = TOMsimilarity(adjacency);
weight=table(cut(weight),seq(from = 12, to=20,y=2))