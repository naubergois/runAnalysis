library(plyr)
require(tcltk)

#Merges the training and the test sets to create one data set.
message("Choose X_train")
xtrainfile<-file.choose()
xtrain <- read.table(xtrainfile)
message("Choose X_test")
xtestfile<-file.choose()
xtest <- read.table(xtestfile)
X <- rbind(xtrain, xtest)

message("Choose Subject train")
subjecttrainfile<-file.choose()
subject_train <- read.table(subjecttrainfile)
message("Choose Subject test")
subjecttestfile<-file.choose()
subject_test <- read.table(subjecttestfile)
S <- rbind(subject_train, subject_test)

message("Choose Y train")
ytrainfile<-file.choose()
ytrain <- read.table(ytrainfile)
message("Choose Y test")
ytestfile<-file.choose()
ytest <- read.table(ytestfile)
Y <- rbind(ytrain, ytest)

message("Choose features file")
featuresfile<-file.choose()
features <- read.table(features)
featuresvector<-as.character(features[,2])
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
names(result)[3:81]<-as.character(features$V2[c(featureindex$V1)])


#start seperating out featName column to seperate columns
names(result) <- gsub('^t', 'time', names(result))
names(result) <- gsub('^f', 'frequency', names(result))
names(result) <- gsub('Acc', 'Accelerometer', names(result))
names(result) <- gsub('Gyro','Gyroscope', names(result))
names(result) <- gsub('mean[(][)]','Mean',names(result))
names(result) <- gsub('std[(][)]','Std',names(result))
names(result) <- gsub('-','',names(result))




#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


tidy<-aggregate(. ~subject + activity, result, mean)
tidy<-tidy[order(tidy$subject,tidy$activity),]
write.table(tidy, file = "tidy.txt",row.name=FALSE)



