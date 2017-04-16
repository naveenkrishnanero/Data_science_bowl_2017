# This code contains 4 parts 
# extracting header file from sample and stage_1 train dicom file 
# extraction code with some changes to train image data and both header and image files
# Little modification to prof's code 
# Preparing my extracted data for models with and without slice location
# models used nueral networks and random forest

# I am not including the part where we extract train and test form csv files 
# I have serveral csv for different function they perform


library(oro.dicom)
library("dplyr")
library(EBImage)
library("h2o")


# used parallels library in the beginning in janus server
# first that processor is far more inferior to i7 processors we have
# All this will be detailedy dealt in lessons learnt



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



# This step had to be written because when wirting this code I didn't 
#know just simply needs row names to append



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

#write.csv(data_,file = "header_data.csv")

colnames(data) <- col.list


# Due to my mistakes done in extracting image data I had spent more time cleaning my mess
# Rather than on my code 
# Well thats difference between pro(my prof) and ametuer




#setwd("C:\\Users\\mnave\\Desktop\\train")
#data_ <- read.csv(file = "sample_images_hdr.csv",header = TRUE)

#data_ <- read.csv(file = "header_data.csv")

#data contains all the header data
#cleaning up my mess to get my image ID
#Add patient to the split string

#test <- data[data$PatientID==data$PatientID[1],]
#tapply(test$image_id,test$PatientID,strsplit,split=c(paste(reg_split,test$PatientID,"/",sep = "")),simplify=FALSE)

#function that supplies my cleaned image id



clean_image_id <- function(x,y=data_$PatientID,z=data_$image_id)
{
  
  split.var <- paste("part[0-9]/",as.character(y[[x]]),"/",sep = "")
  # split.var <- paste("sample_images/",as.character(y[[x]]),"/",sep = "")
  
  unlist(strsplit(as.character(z[x]),split = split.var))[2]
  
}



new_image_id <- unlist(lapply(1:length(data_$image_id),clean_image_id))

test <- data_

test$X <- NULL
test$image_id <- new_image_id

write.csv(test,file="final_header2.csv")

# That ends part 1 ie extarction of header data into a final csv file
# This code is not all which still left some NA values and compications 
# I manually changed the error files in the excel file 


# This snippet contains all the extraction code for all test stage_2 data
# removing the part where we extract header info as we can already extracted train data



setwd("D:\\kaggle\\data_science_bowl_2017\\midterm_predictive\\stage2") #path
#path to my directory


#patient_list=list.files("./sample_images/") #path
patient_list=list.files() #path


for(k in 201:length(patient_list))
{  
  
  
  images_list=list.files(paste("./",patient_list[k],sep = "")) #path
  z=array(dim=c(length(images_list),32,32))
  header=array(dim=c(length(images_list),3))
  
  for (i in 1:length(images_list))
  {
    #  dicom_image=paste("./sample_images/",patient_list[k],"/",images_list[i],sep="") #path
    dicom_image=paste("./",patient_list[k],"/",images_list[i],sep="") #path
    mydata=readDICOMFile(dicom_image)
    y=resize(t(mydata$img), w=32,h=32) 
    z[i,,]=imageData(y)
    temp <- match(c("PatientID","SliceLocation"),mydata$hdr$name )
    h=as.vector(c(mydata$hdr$value[temp],images_list[i]))
    header[i,]=h
    #  names(header[i,])=mydata$hdr$name
    # names(header[i,])=as.vector(c("PatientID","SliceLocation","image_id"))
    
    # rm(mydata)
    rm(y)
    rm(h)
  }
  if(k %% 20==1)
  {print(k)}# TO know status of my file for large list
  
  if(k==201)
  {
    p1=as.data.frame(z)
    p2=as.data.frame(header)
    colnames(p2) <- c("PatientID","SliceLocation","image_id")
    pic1=cbind(p2,p1)
    
    # p2=as.data.frame(z)
    # colnames(header_df)<-c("PatientID","SliceLocation","image_id") 
    # p2 <- cbind(header_df,p2)
    #p1=p2
  }
  else
  {
    p1=as.data.frame(z)
    p2=as.data.frame(header)
    # colnames(p1) <- NULL
    colnames(p2) <- c("PatientID","SliceLocation","image_id")
    temp <- cbind(p2,p1)
    #colnames(temp) <- NULL
    pic1=rbind(pic1,temp)
  }
  
}
write.csv(pic1,"C:\\Users\\mnave\\Desktop\\test201_all.csv") 
#path


format(object.size(z),"Mb")



#This part deals with the part of adding labels to the train data 
# clubbing them together 

#read train set
#train <- read.csv("500_patients_0.csv",header = TRUE)
train <- read.csv("train_agg_0.csv",header = TRUE)

a <- read.csv("500_patients_0.csv",header = TRUE)
b <- read.csv("1000_patients_0.csv",header = TRUE)

c <- rbind(a,b)
train <- a


# read labels
labels <- read.csv("stage1_labels.csv",header = TRUE)
colnames(labels)[1] <- "PatientID"

#Preparing train data
train$X  <- NULL
train$image_id <- NULL

#train$cancer <- NULL
train$Cancer.1 <- NULL
train$Slicelocation<- NULL

#If first column is not patient ID
#merge data with labels 
train.f <- train
train.f <- merge(train,labels,by="PatientID")
# this step is need if u want your model to distinguish between each slice
train$X.1 <- NULL
train.f$X <- NULL
train$SliceLocation <- NULL
#train.f$SliceLocation <- round(train.f$SliceLocation/100)
#train.f$SliceLocation <- NULL
train.f <- aggregate(train.f[,c(-1,-2)],list(train.f$PatientID,train.f$Cancer),mean)

#train.f <- aggregate(train.f[,c(-1,-2)],list(train.f$PatientID,train.f$cancer),mean)


#train.f <- aggregate(train.f[,c(-1,-2)],list(train.f$PatientID,train.f$SliceLocation,train.f$cancer),mean)

#colnames(train.f)[1:3] <- c("PatientID","Slicelocation","Cancer")


#test <- read.csv("test.csv",header = TRUE)

#preaparin test data
if(colnames(test)[1]!="PatientID")
{test[,1] <- NULL}
#If first column is not patient ID
#merge data with labels 
# this step is need if u want your model to distinguish between each slice

test.NA <- subset(test,is.na(test$SliceLocation))
test.NA$SliceLocation <- NULL
test.NA$X <- NULL
test.NA$image_id<- NULL
test.NA <- aggregate(test.NA[,-1],list(test.NA$PatientID),mean)
colnames(test.NA)[1] <- c("PatientID")
test.D<- subset(test,!is.na(test$SliceLocation))

#write.csv(test.NA,"test.NA.csv")
#write.csv(test.D,"test.D.csv")


test.D$SliceLocation <- round(test.D$SliceLocation/100)
test.D$X <- NULL
test.D$image_id <-  NULL
test.D <- aggregate(test.D[,c(-1,-2)],list(test.D$PatientID,test.D$SliceLocation),mean)
colnames(test.D)[1:2] <- c("PatientID","Slicelocation")
test.D$Slicelocation <- as.factor(test.D$Slicelocation)



train <- read.csv("500_patients.csv",header = TRUE)  
train$X<- NULL
train$X.1<- NULL
train$image_id <- NULL

train_2 <- train


for(i in 1:length(train_0_big$PatientID))
{
  
  for(j in 1:length(train_0_big))
  {
    
    if((train_0_big[i,j]==-1024)||(train_0_big[i,j]==-1000)||(train_0_big[i,j]==-2048)||(train_0_big[i,j]==-2000))
    {   train_0_big[i,j]=0    }
  }
  
}

# Last part model preperation 
# We had two algorithms but two different types of data
# That makes it four

train <- read.csv("train_agg_0.csv")
test <- read.csv("test_agg_0.csv")



library("h2o")


h2o.init(nthreads = -1)

a <- as.h2o(train)
#split <- h2o.splitFrame(a,ratios = 0.80,seed = -30)


train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)




#train_h2o <- as.h2o(train)
#test_h2o <- as.h2o(test)

#model_1 <- h2o.deeplearning(x=c(1,3:1026),y=2,training_frame = train_h2o,hidden = c(20,20),epochs = 100)

model_2 <- h2o.randomForest(x=c(1,3:1026),y=2,training_frame = train_h2o,ntrees = 600,max_depth = 17,sample_rate = 0.95)


#best performance with new data
#model_1 <- h2o.deeplearning(x=c(3:1025),y=2,training_frame = train_h2o,hidden = c(10),epochs = 10,standardize = TRUE)
#model_1 <- h2o.deeplearning(x=c(3:1025),y=2,training_frame = train_h2o,ntrees = 100)



#h2o.performance(model = model_2)

#h2o.performance(model = model_1,newdata =test_h2o )

result <- h2o.predict(model_2,test_h2o)
result <- as.data.frame(result)

result <- round(as.data.frame(result),digits = 1)
#out <- cbind(as.data.frame(test_h2o)[,3],result)
out <- cbind(test$PatientID,result)

#pig <- sqldf("select PatientID,max(Cancer),max(predict) from out group by out")

#pig <- aggregate(out[,c(-2)],list(out$`as.data.frame(test_h2o)[, c(1)]`),m)

write.csv(out,"out.csv")

h2o.shutdown(prompt = FALSE)

# This tiny snipped is to correct negative and too much out values due to neural nets


for(i in 1:length(result$predict))
{
  if(result$predict[i]<0)
  {result$predict[i]=0}
}
for(i in 1:length(result$predict))
{
  if(result$predict[i]>1)
  {result$predict[i]=1}
}


# Decleration random forest worked best 

