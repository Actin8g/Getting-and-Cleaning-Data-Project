require(tidyverse)

testData <- read.table("X_test.txt", header = F, sep = "")
trainData <- read.table("X_train.txt", header = F, sep = "")
mergedData <- rbind(testData, trainData)

testsubjects <- read.table("subject_test.txt")
trainsubjects <- read.table("subject_train.txt")
mergedsubjects <- rbind(testsubjects, trainsubjects)

testActivities <- read.table("y_test.txt")
trainActivities <- read.table("y_train.txt")
mergedActivities <- rbind(testActivities, trainActivities)

allMerged <- cbind(seq.int(nrow(mergedsubjects)), mergedsubjects, mergedActivities, mergedData)
names(allMerged)[1] <- "record_id"
names(allMerged)[2] <- "subject_id"
names(allMerged)[3] <- "activity"

allMerged <- as_tibble(allMerged)

dataVariableNames <- read.table("features.txt")[,2]
names(allMerged)[4:564] <- as.character(dataVariableNames)

#Find and keep all of the columns that contain "mean(" or "std(" and also keep the first two - subject_id and activities
meanStdCols <- grep("mean\\x28|std\\x28", names(allMerged))
meanStdMerged <- allMerged[,c(1:3,meanStdCols)]

tidyMerged <- meanStdMerged %>%
  
  gather(key = "tempkey", value = "mean_or_std", -(record_id:activity)) %>%

  mutate(domain = if_else(substring(tempkey, 1, 1) == "t", "time", "frequency")) %>%
  mutate(tempkey = substring(tempkey, 2)) %>%
  separate("tempkey", into = c("measurement", "meanstd", "component"), sep = "-") %>%
  mutate(component = if_else(is.na(component), "total", component))  %>%
  spread(meanstd, mean_or_std)

names(tidyMerged)[7:8] <- c("mean", "standard_deviation")

tidyMerged$activity<-cut(tidyMerged$activity, 6, labels = c("walking", "walking upstairs", "walking downstairs", "sitting", "standing", "laying"))

tidyAverages <- tidyMerged %>%
  group_by(subject_id, activity, measurement, component, domain) %>%
  mutate(overall_average = mean(mean)) %>%
  mutate(mean_std_deviation = mean(standard_deviation))
