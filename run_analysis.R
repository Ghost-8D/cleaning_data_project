# Load libraries
library(data.table)
library(dplyr)
library(tidyr)

# Read train and test data
tr_data <- fread("./data/train/X_train.txt", sep=" ")
te_data <- fread("./data/test/X_test.txt", sep=" ")

# Read column names 
feature_names <- fread("./data/features.txt", sep= " ")
head(feature_names)

# Set feature names of train and test sets
names(tr_data) <- feature_names$V2
names(te_data) <- feature_names$V2

# Take a look at the train dataset
dim(tr_data)
str(tr_data)
head(tr_data, n = 2)

# Take a look at the test dataset
dim(te_data)
str(te_data)
head(te_data, n = 2)

# Check if test and train data have same number of columns
if (dim(tr_data)[2] != dim(te_data)[2]){
    stop("Error! Test and train data sets must have the same number of columns")
}

# Combine train and test into one table and check dimensions
all_data <- rbind(tr_data, te_data)
dim(all_data)

# Extract only the measurements on the mean and standard deviation for each measurement
use_col <- grep("(mean|std)\\(\\)", names(all_data))
use_data <- select(all_data, all_of(use_col))

# Fix column names 
new_names <- sub("^f", "frequency_", names(use_data))
new_names <- sub("^t", "time_", new_names)
new_names <- sub("-mean\\(\\)", "_mean", new_names)
new_names <- sub("-std\\(\\)", "_std", new_names)
new_names <- sub("-X", "_X", new_names)
new_names <- sub("-Y", "_Y", new_names)
new_names <- sub("-Z", "_Z", new_names)

# Prepare tidy dataset
tidy_data <- copy(use_data)
names(tidy_data) <- new_names
tidy_data <- as_tibble(tidy_data)

# Combine mean and std values to reduce number of columns
tidy_data <- gather(tidy_data, key= activity_signal_measure_direction, value = value, 1:ncol(tidy_data))

# Separate activity, signal and direction values
tidy_data <- separate(tidy_data, activity_signal_measure_direction, c("activity", "signal", "measure", "direction"))

# Spread data from measure to mean and std columns
tidy_data <- (tidy_data %>% 
    group_by(measure) %>% 
    mutate(grouped_id = row_number()) %>% 
    spread(measure, value) %>% 
    select(-grouped_id)
)
# Display first tidy data set 
View(tidy_data)

# Check if we missed some data 
if (dim(use_data)[1] * dim(use_data)[2] != dim(tidy_data)[1] * 2){
    stop("Error! Some data are missing from the tidy data set.")
}

# Create a tidy dataset with the average of each variable for each activity and each subject.
tidy_data2 <- (tidy_data %>% 
    group_by(activity, signal, direction) %>% 
    summarise(average_mean = mean(mean), average_std = mean(std))
)
# Display second tidy data set 
View(tidy_data2)

# Write tidy data to file
write.table(tidy_data2, row.name=FALSE, file = "./data/tidy_data2.txt")

