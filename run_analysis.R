#PART1
setwd("~/Desktop/coursera/UCI HAR Dataset/test")
xtest <- read.table("X_test.txt")
ytest <- read.table("Y_test.txt")
subtest <- read.table("subject_test.txt")

setwd("~/Desktop/coursera/UCI HAR Dataset/train")
xtrain <- read.table("X_train.txt")
ytrain <- read.table("Y_train.txt")
subtrain <- read.table("subject_train.txt")

setwd("~/Desktop/coursera/UCI HAR Dataset")
features <- read.table("features.txt")
activitylabels <- read.table("activity_labels.txt", header = FALSE)

colnames(ytest) <- "activityID"
colnames(ytrain) <- "activityID"
colnames(subtest) <- "subjectID"
colnames(subtrain) <- "subjectID"
#need to put the names of the variables in col2 of features into xtrain
colnames(xtrain) <- features[,2]
colnames(xtest) <- features[,2]
#now all of the columns are properly labeled

#since the columns are of all different varibles need to cbind
mergetest <- cbind(subtest, ytest)
merge_test <- cbind(mergetest, xtest)
#have to combine the dataframes seperately because when combine together the activityID is missing
mergetrain <- cbind(subtrain, ytrain)
merge_train <- cbind(mergetrain, xtrain)

#since there are common columns between the mergetest and mergetrain can merge the data
alldata <- merge.data.frame(mergetest, mergetrain, by.x = "activityID", "subjectID", all = T)
#my using the merge data function this just adds columns/varibles and doubles the variables instead of just adding the data to the matching variables
data_all <- rbind(merge_test, merge_train)
#this takes the rows of data and adds it to the matching columns which maintains the number of varibles but adds all the rows together

#PART 2
#Extract the mean and standard deviation of each column
colNames <- colnames(data_all)
m_s <- (grepl("activityID", colNames) | grepl("subjectID", colNames) | grepl("mean..", colNames) | grepl("std..", colNames))
all_m_s <- data_all[ , m_s == TRUE]
clean_m_s <- data_all %>% select(subjectID, activityID, contains("mean"), contains("std"))

#Part 3
clean_all <- merge.data.frame(clean_m_s, activity_labels, by = "activityID", all.x=TRUE)
#the labels in the activitylabels file do not have varibles that match merged dataset
activity_labels <- rename(activitylabels, activityID=V1, activity=V2)
#replace the activityID with the actual activity
clean_all$activityID <- activity_labels[clean_all$activityID, 2]

#PART 4
#Acc <- accelerometer
#t <- time
#f <- frequency
#Gyro <- gyroscope
#Mag <- Magnitude
names(clean_all) <- gsub("Acc", "Accelerometer", names(clean_all))
names(clean_all) <- gsub("Gyro", "Gyroscope", names(clean_all))
names(clean_all) <- gsub("^t", "Time", names(clean_all))
names(clean_all) <- gsub("^f", "Frequency", names(clean_all))
names(clean_all) <- gsub("Mag", "Magnitude", names(clean_all))

#Part 5
#average of each varible for each activity and each subject
data5 <- clean_all %>% group_by(subjectID, activityID)
#need to order subjectID before activityID so it orders by subject first then by activityID
data5 <- summarise_all(data5, funs(mean))
#this summarizes the data5 by taking the mean of each activity for each subject which was organized from the previous code

#Submission
write.table(data5, "data5.txt", row.names = FALSE)
