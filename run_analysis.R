#Merges the training and the test sets to create one data set.

xtrain <- read.table("X_train.txt")
xtest <- read.table("X_test.txt")
X <- rbind(xtrain, xtest)

subject_train <- read.table("subject_train.txt")
subject_test <- read.table("subject_test.txt")
S <- rbind(subject_train, subject_test)

ytrain <- read.table("y_train.txt")
ytest <- read.table("y_test.txt")
Y <- rbind(ytrain, ytest)

features <- read.table("features.txt")

#Extracts only the measurements on the mean and standard deviation for each measurement.

indexstd<-grep(glob2rx("*-std*"), featuresvector, value=TRUE)
indexmean<-grep(glob2rx("*-mean*"), featuresvector, value=TRUE)
featureindex<-features[features$V2 %in% indexmean | features$V2 %in% indexstd,]
X<-X[c(featureindex$V1)]
names(X)<-features[features$V2 %in% indexmean| features$V2 %in% indexstd,2]

#Uses descriptive activity names to name the activities in the data set
activities <- read.table("activity_labels.txt")
#Replace _ and tolower function
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"


names(S) <- "subject"
result <- cbind(S, Y, X)

#Appropriately labels the data set with descriptive variable names.
names(result)[1:2]<-c("subject", "activity")
names(result)[3:563]<-as.character(features$V2)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
labels<-colnames(result)[-c(1,2)]
data <- lapply(X =labels, FUN=function(x) tapply(result[,x], list(result$activity, result$subject), mean))


write.table(data, "merged_data.txt")
