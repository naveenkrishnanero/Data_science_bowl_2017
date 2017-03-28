library(oro.dicom)
library("dplyr")


memory.size(max=T)

setwd("C:\\Users\\mnave\\Downloads\\midterm")
sample_images <- readDICOM("sample_images",recursive = TRUE,verbose = TRUE)
si.header <- sample_images$hdr
img.names <- names(si.header)


si.hdrdata <- lapply(1:length(img.names),function(x,y=si.header,z=img.names){return(y[[z[x]]])})
hdr.data <-  lapply(1:length(img.names),function(x,y=si.hdrdata,z=img.names){return ((rbind(y[[x]][,c("name","value")],c("image_id",z[x]))))})
col.list <- c("Modality","SeriesDescription","PatientID","PatientsBirthDate","SeriesNumber","ImagePositionPatient",
"ImageOrientationPatient","SliceLocation","SamplesperPixel","PhotometricInterpretation","Rows","Columns","WindowCenter","WindowWidth",
"RescaleIntercept","RescaleSlope","image_id")

hdr.filter.data <-  lapply(1:length(hdr.data),function(x,y=hdr.data,z=col.list){return(filter(y[[x]],y[[x]][,1] %in% z))})

hdr.values<-lapply(1:length(hdr.filter.data),function(x,y=hdr.filter.data){y[[x]][,2]})

data <- NULL

for(i in 1:length(hdr.values))
{
  
  data <- rbind(data,hdr.values[[i]])
  
}
data <- as.data.frame(data)
colnames(data) <- col.list

write.csv(data,"sample_images_hdr.csv")




