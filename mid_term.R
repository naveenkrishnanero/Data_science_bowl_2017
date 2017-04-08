library(oro.dicom)
library("dplyr")


memory.size(max=T)
setwd("C:\\Users\\mnave\\Downloads\\midterm")
sample_images <- readDICOM("sample_images",recursive = TRUE,verbose = TRUE)
#train_images <- readDICOM("part2",recursive = TRUE,verbose = TRUE) # for server
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
{data <- rbind(data,hdr.values[[i]])}
data <- as.data.frame(data)
colnames(data) <- col.list
#write.csv(data,"sample_images_hdr.csv")


#sample_images[[2]][[1]]
#write.csv(z,"test.csv")

p <- 1:(512*512)

# appending all data into a column
#retireving each image
#getting respective row and appending it to a previous one
# Goal is to make entire matrix into a single column to store in an excel
#test <- sample_images
#sample_images <- test


# final <- data.frame()
# for(i in 1:3)
# {
#   data <- integer()
#   p <- sample_images[[2]][[i]]
#   for(j in 1:512)
#   {
#     data <- append(data,p[j,])
#   }
#   write.table(final,"test.csv",row.names = F,col.names = T,append = T)
#   
# }

final <- data.frame()
for(i in 1)
{
  data <- integer()
  p <- sample_images[[2]][[i]]
  for(j in 1:512)
  {
    data <- rbind(data,p[j,])
  }
  write.table(final,"test.csv",row.names = F,col.names = T,append = T)
  
}





rm(list = ls())

#folder containing my individual csv files
setwd("C:\\Users\\mnave\\Desktop\\train")


#step1 combining all my individual header files
excel_files <- list.files()

temp <- data.frame()
data_ <- data.frame()

for (i in 1:length(excel_files))
{
  if(i==1)
  {  data_ <- read.csv(file = excel_files[i],header = TRUE,fill = TRUE,as.is = TRUE,row.names = NULL)}
  else
  {
    temp<- read.csv(file = excel_files[i],fill = TRUE,as.is = TRUE,row.names = NULL)
    data_ <- rbind(temp,data_)
    #Tried append function it causes problems due to un equal number of rows
    
  }
}

write.csv(data_,file = "header_data.csv")

data_ <- read.csv(file = "header_data1.csv")

#data contains all the header data
#cleaning up my mess to get my image ID
#Add patient to the split string

#test <- data[data$PatientID==data$PatientID[1],]
#tapply(test$image_id,test$PatientID,strsplit,split=c(paste(reg_split,test$PatientID,"/",sep = "")),simplify=FALSE)

#function that supplies my cleaned image id



clean_image_id <- function(x,y=data_$PatientID,z=data_$image_id)
{
  
  split.var <- paste("part[0-9]+/",as.character(y[[x]]),"/",sep = "")
  unlist(strsplit(as.character(z[x]),split = split.var))[2]
  
}



new_image_id <- unlist(lapply(1:length(data_$image_id),clean_image_id))

test <- data_

test$X <- NULL
test$image_id <- new_image_id

write.csv(test,file="test.csv")






#the header file has some NA values which need to be checked


#test2 <- test2[is.na(test2$image_id),]
#test2 <- test[,c(5,19)]












