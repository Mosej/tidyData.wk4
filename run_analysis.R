library(dplyr)
library(reshape2)
library(readr)
library(tidyr)
#read files into r
# load the features vector for col names of measurements

features<-read.table("features.txt", header = FALSE)
featcol<-features[,2]

# read from ./test. subject(subjest_test), activity(y_test), measurments(x_test) & bind the data

subject.test<- read.table("test/subject_test.txt", col.names = "subject")
activity.test<- read.table("test/y_test.txt", col.names = "activity")
measurements.test<-read.table("test/X_test.txt", col.names= featcol) #with feature as colomn names
test<- cbind(subject.test, activity.test, measurements.test)

# from ./train. subject(subjest_train), activity(y_train), measurments(x_train) & bind

subject.train<- read.table("train/subject_train.txt", col.names = "subject")
activity.train<- read.table("train/y_train.txt", col.names = "activity")
measurements.train<-read.table("train/X_train.txt", col.names = featcol) #with feature as colomn names
train<- cbind(subject.train, activity.train, measurements.train)

# merge train and test, extract measure of means and std for each measurement, using diplyr chain

mydata<- merge(train, test, all = TRUE)
mydata<-mydata[,grepl("subject|activity|mean...[^Freq]|std...", colnames(mydata))]

#adding row indexes

mydata$row<- 1:nrow(mydata)

#gathering, separate measurment and spread mean and std into columns using Tidyr. and remove row column

mydata<-gather(mydata, measurement,measure, tBodyAcc.mean...X:fBodyGyro.std...Z, -subject, -row) %>%
        separate(measurement, c("signal","measurement","axial"))%>%
        spread(measurement, measure)%>%
        within(rm(row))

#add descriptive names to the activities

mydata$activity<- as.factor(mydata$activity)
levels(mydata$activity)<-c("Walking","Walking_upstairs","Walking_downstairs","sitting","standing","laying")

#using reshape to create averages for variables, activities and subjects

mydata<- melt(mydata, id = c("subject","activity","signal","axial"))

#tranform the Id variables into factors
mydata$subject<- as.factor(mydata$subject)
mydata$activity<- as.factor(mydata$activity)
mydata$signal<- as.factor(mydata$signal)
mydata$axial<- as.factor(mydata$axial)
#after transforming them, reshape using dcast
final<-dcast(mydata, subject + activity + signal+ axial ~ variable, mean)

#write to text file
file.create("tidyData.txt") 
write.table(final, file = "tidyData.txt", row.names = FALSE, col.names = TRUE)

## read the data using read.table with header=TRUE
