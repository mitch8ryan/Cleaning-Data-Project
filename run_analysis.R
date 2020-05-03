## Mitch Ryan
## Getting and Cleaning Data 

if (!require('dplyr')) packages.install('dplyr'); library('dplyr')

dataset.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

# Our working directory; it will be created it necessary
working.directory.path <- file.path(path.expand('~'), 'getdata-016-project')
# The name of the directory which holds the data set; a subfolder of working.directory.path
data.directory.name <- 'UCI HAR Dataset'

# Function to Fix the variable names in the dataset
Variable_cleaner <- function (var) {
  
  var <- gsub('[(]', '', var)
  var <- gsub('[)]', '', var)
  var <- gsub('[-]', '.', var)
  
  prefix <- 'time'
  if (substr(var, 1, 1) == 'f') {
    prefix <- 'frequency'    
  }
  
  var <- substring(var, 2)
  var <- gsub("BodyBody", "Body", var)
  return(paste('mean', prefix, tolower(var), sep = '.'))
}

# Function to load in the data
LoadData <- function (set) {

  path = file.path(getwd(), data.directory.name, set)
  
  subjects <- read.table(file.path(path, paste('subject_', set, '.txt', sep = '')))
  activities <- read.table(file.path(path, paste('y_', set, '.txt', sep = '')))
  metrics <- read.table(file.path(path, paste('X_', set, '.txt', sep = '')))
  
  metric.labels <- sapply(metric.labels, Variable_cleaner)
  names(metrics) <- metric.labels
  

  columnIndexes = sapply(metric.labels, function(colname){ grepl(colname,  pattern = "\\.std") | (grepl(colname,  pattern = "\\.mean") & !grepl(colname,  pattern = "\\.meanfreq")) })
  metrics <- metrics[, columnIndexes]
  
  data <- cbind(subjects, activities)
  names(data) <- c('subject', 'activity')
  data <- cbind(data, metrics)
  
  
  data
}
# Function that checks the working directory and sets it
Check_wd <- function () {
  
  if(!file.exists(working.directory.path)) {
    dir.create(working.directory.path)
  }
  
  if(!file.exists(working.directory.path)) {
    stop('Unable to establish working directory')    
  }
  
  setwd(working.directory.path)
}

# Function that downloads the data and unzips it
DownloadData <- function () {

  datapath <- file.path(working.directory.path, data.directory.name)
  
  if (file.exists(datapath)) {
    return(NULL)
  }
  
  message('Downloading the data set; this may take a few moments.....')
  tmp <- tempfile()    
  download.file(dataset.url, tmp, method = "curl", cacheOK = TRUE)
  unzip(tmp)
  unlink(tmp)
  
  if (!file.exists(datapath)) {
    stop('Unable to acquire data')
  }
}

Check_wd()
DownloadData()

metric.labels <- read.table(file.path(getwd(), data.directory.name, 'features.txt'))[, 2]
activity.labels <- as.character(read.table(file.path(getwd(), data.directory.name, 'activity_labels.txt'))[, 2])


data <- rbind(LoadData('train'), LoadData('test'))


data <- data[order(data[,2]), ]
data[,2] <- factor(data[,2], labels = activity.labels)

summarized.data <- group_by(data, subject, activity) %>% summarise_each(funs(mean))


write.table(summarized.data, row.name=FALSE, file='./tidyDataSet.txt')