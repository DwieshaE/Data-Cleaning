setwd('./Data Science Course/Lecture Notes/Data Cleaning')

if (!file.exists("datacleanproject")){dir.create("datacleanproject")}

FileUrl2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(FileUrl2, destfile = "UCI-HAR-dataset.zip")
unzip('./UCI-HAR-dataset.zip')

# Merges the training and the test sets to create one data set.
xtrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
xtest <- read.table('./UCI HAR Dataset/test/X_test.txt')
x <- rbind(xtrain, xtest)

subtrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
subtest <- read.table('./UCI HAR Dataset/test/subject_test.txt')
subbind <- rbind(subtrain, subtest)

ytrain <- read.table('./UCI HAR Dataset/train/y_train.txt')
ytest <- read.table('./UCI HAR Dataset/test/y_test.txt')
y <- rbind(ytrain, ytest)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table('./UCI HAR Dataset/features.txt')
meansd <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
xmeansd <- x[, meansd]

# Uses descriptive activity names to name the activities in the data set
names(xmeansd) <- features[meansd, 2]
names(xmeansd) <- tolower(names(xmeansd)) 
names(xmeansd) <- gsub("\\(|\\)", "", names(xmeansd))

activities <- read.table('./UCI HAR Dataset/activity_labels.txt')
activities[, 2] <- tolower(as.character(activities[, 2]))
activities[, 2] <- gsub("_", "", activities[, 2])

y[, 1] = activities[y[, 1], 2]
colnames(y) <- 'activity'
colnames(subbind) <- 'subject'

# Appropriately labels the data set with descriptive activity names.
data <- cbind(subbind, xmeansd, y)
str(data)
write.table(data, './UCI HAR Dataset/merged.txt', row.names = F)

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
averagesec <- aggregate(x=data, by=list(activities=data$activity, sub=data$subject), FUN=mean)
averagesec <- average.df[, !(colnames(average.df) %in% c("subj", "activity"))]
str(averagesec)
write.table(averagesec, './UCI HAR Dataset/average.txt', row.names = F)

