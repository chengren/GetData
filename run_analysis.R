## Getting and Cleaning Data Project
## Peer Assessments

# the working directory
#------------------------------------------------
# setwd("C:\Users\chengren\Desktop/UCI")
files <- list.files(getwd(), recursive=TRUE)
files <- rev(files) # a reversed version of vector 'files' for handling later issues

# read function
#------------------------------------------------
read <- function(x){
  if(x %in% c("test","train")){
      x <- as.character(x)
  } else {stop("x is not valid")}
  # Create an index i for directory files
  i <- grep(pattern=paste0("(.*)",x,"+(.*)txt"), x=files)
  # Read the data
  data <- lapply(X=i, function(X){read.table(file=files[X])})
  DF <- data.frame(Reduce(cbind, data))
  return(data.frame("class"=rep(x, times=nrow(DF)), DF))
}

# 1. Merges the training and the test sets to create one data set.
#------------------------------------------------
raw_train <- read(x="train")
raw_test <- read(x="test")
raw <- rbind(raw_train,raw_test)
# dim(raw) # Retrieve or set the dimension of the object.

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#------------------------------------------------
filesnames = grep(pattern="train+(.*)", x=files, value=TRUE)
namestxt <- gsub(pattern=paste0("(.*)+(./?)+/"),replacement="",x=filesnames)
names <- gsub(pattern="_train.txt",replacement="",x=namestxt)
i <- grep(pattern="body|total",x=names)
measures <- lapply(X=i, function(X){ paste0(names[X], seq_len(128)) })
measures <- unlist(measures)
features <- read.table(file="features.txt",row.names=1,colClasses="character")
features <- features$V2
# names as in files once reversed
colnames(raw) <- c("class","activity","subject",features,measures)

meanORstd <- grep(pattern="*mean\\(\\)*|*std\\(\\)*",x=colnames(raw),value=TRUE)
extraction <- c("class","activity","subject", meanORstd)
extract <- raw[,extraction]
# dim(extract)

# 3. Uses descriptive activity names to name the activities in the data set.
#------------------------------------------------
# This step was performed in the extraction process.
# colnames(extract)

# 4. Appropriately labels the data set with descriptive activity names.
#------------------------------------------------
activities <- read.table(file="activity_labels.txt", row.names=1)
extract[,"activity"] <- factor(x=extract[,"activity"], labels=activities$V2)

# 5. Creates a second, independent tidy data set with the average of each
# variable for each activity and each subject.
#------------------------------------------------
tidy <- aggregate(x = extract[,meanORstd], by = extract[,c("activity","subject")], FUN = "mean")
# head(tidy, n=10)
# dim(tidy)[1] == length(activities$V2) * length(unique(extract[,"subject"]))

# The tidy data frame is written to a file.
write.csv(x=tidy, file = "tidy.txt", row.names=FALSE)

# The tidy data frame can be read as follows
# read.csv(file = "tidy.txt", header = TRUE, check.names = FALSE)