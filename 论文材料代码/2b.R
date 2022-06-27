danMat <- read.table("danMat.txt")
duoMat <- read.table("duoMat.txt")
danMat.eigen <- eigen(danMat)
save(danMat.eigen,file = "danMat.eigen.Rdata")
duoMat.eigen <- eigen(duoMat)
save(duoMat.eigen,file = "duoMat.eigen.Rdata")

load("danMat.eigen.Rdata")
load("duoMat.eigen.Rdata")
RMSIPs <- NULL
for(i in seq(1,nrow(danMat.eigen$vectors),by = round(nrow(danMat.eigen$vectors)/500))){
  print(i)
  RMSIP_part<- 0
  for(seq_temp_i in seq(1,i)){
    for(seq_temp_j in seq(1,i)){
      eigen_value <- (danMat.eigen$vectors[,seq_temp_i] %*% duoMat.eigen$vectors[,seq_temp_j])
      eigen_value <- as.numeric(eigen_value[1,1])
      eigen_value <-  eigen_value^2
      RMSIP_part <- RMSIP_part + eigen_value
    }
  }
  RMSIP <- sqrt(RMSIP_part/i)
  RMSIPs <- c(RMSIPs,RMSIP)
}

RMSips <-cbind(seq(1,nrow(danMat.eigen$vectors),by = round(nrow(danMat.eigen$vectors)/500))[seq(1,length(RMSIPs))],RMSIPs)
write.table(RMSips,file="RMSips.txt",quote = F,col.names = T,row.names = F)