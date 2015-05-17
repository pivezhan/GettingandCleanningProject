####################### here is loading stage for data base ###########
setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\
      Getting and Cleaning Data\\programming")
#here is different directories for all datasets
dir1<-"C:\\Users\\lenovo\\Desktop\\Rlearning\\
Getting and Cleaning Data\\programming\\UCI HAR Dataset\\"
dir2<-"C:\\Users\\lenovo\\Desktop\\Rlearning\\
Getting and Cleaning Data\\programming\\UCI HAR Dataset\\train\\"
dir3<-"C:\\Users\\lenovo\\Desktop\\Rlearning\\
Getting and Cleaning Data\\programming\\UCI HAR Dataset\\test\\"

################################ part1 ################################
#in this part of assignment trainning and test sets should
#  be merged together
#all files in directory that should be considered 
#1- features.txt
#2- subject_train.txt
#3- X_train.txt
#4- y_train.txt
#5- subject_test.txt
#6- X_test.txt
#7- y_test.txt
#8- activity_labels.txt

activity_labels<-read.table(paste(dir1,"activity_labels.txt",sep=""),
                            col.names=c("ID1","activity"))
features<-read.table(paste(dir1,"features.txt",sep=""),
                     col.names=c("ID2","feature"))

subject_train<-read.table(paste(dir2,"subject_train.txt",sep=""))
X_train<-read.table(paste(dir2,"X_train.txt",sep=""))
y_train<-read.table(paste(dir2,"y_train.txt",sep=""))
subject_test<-read.table(paste(dir3,"subject_test.txt",sep=""))
X_test<-read.table(paste(dir3,"X_test.txt",sep=""))
y_test<-read.table(paste(dir3,"y_test.txt",sep=""))

TrainValues<-cbind(subject_train,X_train,y_train)
TestValues<-cbind(subject_test,X_test,y_test)

subject<-rbind(TrainValues,TestValues)

############################## part2 #################################
##grep function return all mean and std features in feature set
MeanMeasurment<-features$feature[grep("mean",tolower(as.character(features$feature)))]
StdMeasurment<-features$feature[grep("std",tolower(as.character(features$feature)))]
allfeatures<-grep("mean|std",tolower(as.character(features$feature)))
############################## part3 #################################
# all column in train and test sets renamed by col.names parameter
# in read.table function
X_train<-read.table(paste(dir2,"X_train.txt",sep=""),
                    col.names=features$feature)
X_test<-read.table(paste(dir3,"X_test.txt",sep=""),
                   col.names=features$feature)
subject_train<-read.table(paste(dir2,"subject_train.txt",sep=""),
                          col.names="subject")
subject_test<-read.table(paste(dir3,"subject_test.txt",sep=""),
                         col.names="subject")
y_train<-read.table(paste(dir2,"y_train.txt",sep=""),
                    col.names="labels")
y_test<-read.table(paste(dir3,"y_test.txt",sep=""),
                   col.names="labels")
# renamed files modified as bellow

RenamedTrain<-cbind(subject_train,y_train,X_train[,allfeatures])
RenamedTest<-cbind(subject_test,y_test,X_test[,allfeatures])

Renamedsubject<-rbind(RenamedTrain,RenamedTest)

############################### part4 #################################
# all feature names modified as bellow
RepairedNames1<-gsub("\\(\\)","",as.character(names(Renamedsubject)))
RepairedNames2<-gsub("\\.","",RepairedNames1)
RepairedNames3<-gsub("[0-9]","",RepairedNames2)
names(Renamedsubject)<-RepairedNames3

################################ part5 ################################
# all features average dependent on subject cases 
UniqueSubject<-unique(Renamedsubject$subject)
UniqueActivity<-length(unique(Renamedsubject$label))
SubjectFeatureMeans<-matrix(rep(0,
                length(UniqueSubject)*UniqueActivity*(ncol(Renamedsubject))),
                (length(UniqueSubject)*UniqueActivity),(ncol(Renamedsubject)))
for (i in 1:length(UniqueSubject)){
        for (j in 1:UniqueActivity){
        SubjectFeatureMeans[(((i-1)*UniqueActivity)+j),]<-
                c(i,as.character(activity_labels$activity[j]),
                sapply(Renamedsubject[(Renamedsubject$subject== 
                UniqueSubject[i])&(Renamedsubject$label==j),(3:ncol(Renamedsubject))],mean))
}
}
####################### writting modified tidy data ###################
SubjectFeatureMeans<-data.frame(SubjectFeatureMeans)
names(SubjectFeatureMeans)<-RepairedNames3
write.table(SubjectFeatureMeans,file="SubjectFeatureMeans.txt", row.name=FALSE)

test<-read.table(paste(dir1,"new2.txt",sep=""))
head(test)
