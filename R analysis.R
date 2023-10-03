#Course project - Samsung Galaxy data
install.packages("data.table")
library(data.table)
#Set working directory
path <- getwd()
path

#Read datasets and name accordingly
features_names <- read.table("features.txt")
activity_label <- read.table("activity_labels.txt")

subject_test <- read.table("test/subject_test.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

subject_train <- read.table("train/subject_train.txt")
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

#Q1: Merge training and test datasets
subject_combine <- rbind(subject_test, subject_train)
activity_combine <- rbind( y_train, y_test)
features <- rbind(x_train, x_test)
colnames(features) <- t(features_names[2])
colnames(activity_combine) <- "Activity"
colnames(subject_combine) <- "Subject"
merged <- cbind(features, activity_combine, subject_combine)


#Q2: Extracts data with only mean and SD
meansd <- grep(".*Mean.*|.*Std.*", names(merged), ignore.case=TRUE)
requiredcols <- c(meansd, 562, 563)
dim(merged)
extracted <- merged[ ,requiredcols]

#Q3: Rename activities
extracted$Activity <- as.character(extracted$Activity)
for (i in 1:6){
      extracted$Activity[extracted$Activity == i] <- as.character(activity_label[i,2])
}

#Q4: Labels
names(extracted)<-gsub("Acc", "Accelerometer", names(extracted))
names(extracted)<-gsub("Gyro", "Gyroscope", names(extracted))
names(extracted)<-gsub("BodyBody", "Body", names(extracted))
names(extracted)<-gsub("Mag", "Magnitude", names(extracted))
names(extracted)<-gsub("^t", "Time", names(extracted))
names(extracted)<-gsub("^f", "Frequency", names(extracted))
names(extracted)<-gsub("tBody", "TimeBody", names(extracted))
names(extracted)<-gsub("-mean()", "Mean", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("-std()", "STD", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("-freq()", "Frequency", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("angle", "Angle", names(extracted))
names(extracted)<-gsub("gravity", "Gravity", names(extracted))
names(extracted)


#Q5: Create independent tidy data set for each activity and subject
extracted$Subject <- as.factor(extracted$Subject)
extracted <- data.table(extracted)

tidy <- aggregate(. ~Subject + Activity, extracted, mean)
tidy <- tidy[order(tidy$Subject,tidy$Activity),]
write.table(tidy, file = "Tidy.txt", row.names = FALSE)


