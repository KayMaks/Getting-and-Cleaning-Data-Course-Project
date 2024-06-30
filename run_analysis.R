#Set working directory to location of all necessary unzipped files
#Load necessary packages (dplyr)
library(dplyr)

#Read test and train data into R
temp <- list.files(pattern = "\\.txt")
for (i in 1:length(temp)) {
         assign(temp[i], read.table(temp[i]))
}
#Merge data together using cbind (for the test files and then the train files), 
#and then rbind to combine the new data sets together
#Give columns unique names to prevent errors (such as V1 not being unique)
colnames(subject_test.txt) <- "Subject"
colnames(subject_train.txt) <- "Subject"
colnames(y_test.txt) <- "Activity"
colnames(y_train.txt) <- "Activity"
colnames(X_test.txt) <- features.txt$V2
colnames(X_train.txt) <- features.txt$V2
#Bind columns together to create one large data set using cbind and rbind
dataset1 <- cbind(subject_test.txt, y_test.txt, X_test.txt)
dataset2 <- cbind(subject_train.txt, y_train.txt, X_train.txt)
dataset <- rbind(dataset1, dataset2)

#Extract the data referring to the mean and standard deviation measurement
#throughout the data set, using keywords
Keep <- grepl("Subject|Activity|mean|std", colnames(dataset))
SubjectActivity <- dataset[, Keep]

#Use activity labels to replace the values within column 2
SubjectActivity$Activity <- factor(SubjectActivity$Activity, 
                          levels = activity_labels.txt[, 1],
                          labels = activity_labels.txt[, 2])

#Give the columns descriptive names
#Retrieve names
columnnames <- colnames(SubjectActivity)
#Remove special characters
columnnames <- gsub("[\\(\\)-]", "", columnnames)
#Change lower_case letters to upper case
columnnames <- gsub("mean", "Mean", columnnames)
#Substitute shorthand titles for descriptive
columnnames <- gsub("^f", "frequencyDomain", columnnames)
columnnames <- gsub("^t", "timeDomain", columnnames)
columnnames <- gsub("Acc", "Accelerometer", columnnames)
columnnames <- gsub("Gyro", "Gyroscope", columnnames)
columnnames <- gsub("Mag", "Magnitude", columnnames)
columnnames <- gsub("Freq", "Frequency", columnnames)
columnnames <- gsub("std", "StandardDeviation", columnnames)
#Fix typo
columnnames <- gsub("BodyBody", "Body", columnnames)
#Change column names on data set
colnames(SubjectActivity) <- columnnames

#Create a second, independent tidy data set with the average of each variable
#for each activity and each subject
#Group by Subject and Activity, then summarise 
SubjectActivity.Means <- SubjectActivity %>%
                        group_by(Subject, Activity) %>%
                        summarise_all(mean)