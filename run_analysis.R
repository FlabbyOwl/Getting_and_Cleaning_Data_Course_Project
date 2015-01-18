# You should create one R script called run_analysis.
# R that does the following. 

# 1. Merges the training and the test sets to create one data set.

x.train <- read.table("./train/X_train.txt")
x.test <- read.table("./test/X_test.txt")
x.all <- rbind(x.train, x.test)
rm(x.train)
rm(x.test)

y.train <- read.table("./train/y_train.txt")
y.test <- read.table("./test/y_test.txt")
y.all <- rbind(y.train, y.test)
rm(y.train)
rm(y.test)

s.train <- read.table("./train/subject_train.txt")
s.test <- read.table("./test/subject_test.txt")
s.all <- rbind(s.train, s.test)
rm(s.train)
rm(s.test)

# 2. Extracts only the measurements on the mean and standard 
#    deviation for each measurement. 

features <- read.table("./features.txt")
ind.mean_std <- sort(c(grep("-mean[()]", features[, 2]), 
                       grep("-std[()]", features[, 2])))
x.mean_std <- x.all[, ind.mean_std]
names(x.mean_std) <- features[ind.mean_std,2]

# 3. Uses descriptive activity names to name the activities in the data set

activity <- read.table("./activity_labels.txt")
y.names <- data.frame(activity[y.all[, 1], 2])
names(y.names) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names. 
names(s.all) <- "subject"
all <- cbind(s.all, y.names, x.mean_std)
write.table(all, "./all_cleaned_data.txt")

# 5. From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and each subject.

only.subj <- sort(unique(all[, 1]))
only.act <- levels(unique(all[, 2]))
tidy.data <- data.frame()
for (i in 1:length(only.subj)) {
        for (j in 1:length(only.act)) {
             
                temp.df <- subset(all, all$subject == only.subj[i] & 
                                          all$activity == only.act[j])
                row.names(temp.df) <- NULL
                avg.data <- colMeans(temp.df[, -c(1, 2)])
                avg.data.n <- cbind(unique(temp.df[c(1,2)]), 
                                    t(data.frame(avg.data)))
                tidy.data <- rbind(tidy.data, avg.data.n)        
        }
}

write.table(tidy.data, "./tidy_average_data.txt", row.name=FALSE)
