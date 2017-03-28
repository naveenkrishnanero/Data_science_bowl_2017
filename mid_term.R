library(oro.dicom)
library("dplyr")
library("parallel")


memory.size(max=T)
#short tips for parallel
#detect cores 
#makecluster 
# clusterEvalQ to import libraries requiredinto the cluster
#use parLapply parSapply
# clusterEvalQ(p,c(library("dplyr"),library(oro.dicom))

setwd("C:\\Users\\mnave\\Downloads\\midterm")
sample_images <- readDICOM("sample_images",recursive = TRUE,verbose = TRUE)
test <- sample_images

no_cores <- detectCores()-2

#reading the dicom file
#all variables starting with t are test ones not to effect my original data
#except for transpose function t()


si.header <- sample_images$hdr
img.names <- names(si.header)


cluster1 <- makeCluster(no_cores,type = "PSOCK")
clusterExport(cluster1,c("si.header","img.names"))
#clusterEvalQ(cluster1,c(library("dplyr"),library(oro.dicom)))
si.hdrdata <- parLapply(cluster1,1:length(img.names),function(x,y=si.header,z=img.names){return(y[[z[x]]])})
#stopCluster(cluster1)             

#cluster2 <- makeCluster(no_cores,type = "PSOCK")
#clusterExport(cluster2,c("si.header","img.names","si.hdrdata"))
clusterExport(cluster1,c("si.hdrdata"))

#clusterEvalQ(cluster2,c(library("dplyr"),library(oro.dicom)))
hdr.data <-  parLapply(cluster1,1:length(img.names),function(x,y=si.hdrdata,z=img.names){return ((rbind(y[[x]][,c("name","value")],c("image_id",z[x]))))})
#stopCluster(cluster2)


col.list <- c("Modality","SeriesDescription","PatientID","PatientsBirthDate","SeriesNumber","ImagePositionPatient",
"ImageOrientationPatient","SliceLocation","SamplesperPixel","PhotometricInterpretation","Rows","Columns","WindowCenter","WindowWidth",
"RescaleIntercept","RescaleSlope","image_id")
#filter(test,test[,1] %in% col.list)
#filter for only columns required

#cluster3 <- makeCluster(no_cores,type = "PSOCK")
clusterExport(cluster1,c("col.list","hdr.data"))
clusterEvalQ(cluster1,c(library("dplyr")))
hdr.filter.data <-  parLapply(cluster1,1:length(hdr.data),function(x,y=hdr.data,z=col.list){return(filter(y[[x]],y[[x]][,1] %in% z))})
#stopCluster(cluster3)

#cluster4 <- makeCluster(no_cores,type = "PSOCK")
clusterExport(cluster1,c("hdr.filter.data"))
hdr.values<-parLapply(cluster1,1:length(hdr.filter.data),function(x,y=hdr.filter.data){y[[x]][,2]})
stopCluster(cluster1)

data <- NULL

for(i in 1:length(hdr.values))
{
  
  data <- rbind(data,hdr.values[[i]])
  
}
data <- as.data.frame(data)
colnames(data) <- col.list

write.csv(data,"sample_images_hdr.csv")




