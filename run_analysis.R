##
## Course: Getting and Clearing Data
## Subject: Course project
##
##

# define directory path of dataset folder
#
# Note: 
#    Before testing, below URL should be modified accordingly to 
#    individual's platform environment
workspaceURL <- "D:/My Education/Coursera - Data Science/Rworkspace/UCI HAR Dataset/"

# upload 'train' dataset
data <- read.table(paste(workspaceURL,"train/X_train.txt",sep=""))
# upload 'test' dataset
data.test <- read.table(paste(workspaceURL,"test/X_test.txt",sep=""))

##
## Requirement 4 - label dataset variable with desctipive names
##
# read in measurement variables names list
mv <- read.table(paste(workspaceURL,"features.txt",sep=""),sep=" ")
# set column labels with measurement variables names
colnames(data) <- mv$V2 
colnames(data.test) <- mv$V2
rm(mv) # free unused data

# read in 'train' subject labels
train.sl <- read.table(paste(workspaceURL,"train/subject_train.txt",sep=""))
# read in 'train' activity labels
train.al <- read.table(paste(workspaceURL,"train/y_train.txt",sep=""))

# read in 'test' subject labels
test.sl <- read.table(paste(workspaceURL,"test/subject_test.txt",sep=""))
# read in 'train' activity labels
test.al <- read.table(paste(workspaceURL,"test/y_test.txt",sep=""))

# add 'train' activities label 
data <- cbind(activity=train.al$V1,data)
# add 'train' subject label to front column
data <- cbind(subject=train.sl$V1,data)
# add "train" label to dataset column
data <- cbind(dataset=rep("train",7352),data)

# add 'test' activities label 
data.test <- cbind(activity=test.al$V1,data.test)
# add 'test' subject label to front column
data.test <- cbind(subject=test.sl$V1,data.test)
# add "test" label to dataset column
data.test <- cbind(dataset=rep("test",2947),data.test)


##
## Requirement 1 - merge two datasets
## 
data <- rbind(data,data.test) # dim(10299,564)
rm(data.test) # free unused data


##
## Requirement 3 - provide "descriptive activity names"
##
# copy out original 'activity' column & convert into factors
d <- data.frame(act=as.character(data$activity),stringsAsFactors=TRUE)
# convert level names into descriptive names
levels(d$act)<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
# copy back and replace
data$activity <- d$act
rm(d) # free unused data


##
## Requirement 2 - subset of mean() and std() measurement datas
##
# a vector of 'colnames' members with partial strings: "-mean" or "-std"
sub <- names(data)[grep("-mean|-std",names(data))] # 79 activities
# create subset with column names within the vector
subdata <- subset(data,select=c(names(data)[1:3],sub))


##
## Requirement 5 - Creates a second, independent tidy data set 
##          with the average of each variable for each activity 
##          and each subject. 
##
library(reshape2)
subdata2 <- subset(data,select=-dataset) #remove unused column
subdata2 <- data[order(data$subject),]  #rank by subject
# create empty master data.frame 
tidy.mean <- subdata2[1,]
tidy.mean <- tidy.mean[-1,]
#

# for each of the 30 subjects
#     prepare measurement average for each activity
#     concatenate to master data.frame
for (i in 1:30) {
    s <- subset(subdata2, subject==i)
    # methods based on week 3, module 4-reshaping data
    t <- melt(s,id=c("activity"), measure.vars=names(s)[3:563])
    tmean <- dcast(t,activity ~ variable, mean)
    # add subject ID column
    tmean <- cbind(subject=rep(i,6),tmean)
    # append to bottom of master data.frame
    tidy.mean <- rbind(tidy.mean,tmean)
}

# if sorting according to activity
# tidy.mean <- tidy.mean[order(tidy.mean$activity),]

# output tidy data.frame as .csv file with sep=","
write.csv(tidy.mean,"tidy.mean.csv")
