## project: Getting and Cleaning Data Project
## date:    2022-05
## author:  SL-47

## script
## 1 -  Merges test and training data into a single data set.seed
## 2 -  Extracts only measurements on the mean and standard deviation for each
##      measurement
## 3 -  Uses descriptive activity names to name the activities in the datasets
## 4 -  Labels the dataset with descriptive variable names
## 5 -  From the data set in 4, creates a new tidy data set with the average
##      of each variable for each activity and subject


## This script is based on the file structure stated in the code-book.rmd
## in the tidy directory and the working directory is the project root

## There are 2 datasets to be processed into one, the structure of each set
## of data being identical.

## Libraries
# install any packages if required.
packages <- c("tidyverse", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)




## load common data
activity_labels_df <- read.table("./activity_labels.txt"
                                 , sep = ""
                                 , header = FALSE
                                 , na.strings = ""
                                 , col.names = c("id", "activity")
                                 , stringsAsFactors = TRUE)

feature_labels_df <- read.table("./features.txt"
                                , sep = ""
                                , header = FALSE
                                , col.names = c("index", "label_name")
                                , colClasses = c("numeric", "character")
                                , na.strings = ""
                                , stringsAsFactors = FALSE)


get_data <- function(x_file, y_file, subject_file){
    ## Process data set of same structure
    ## returns a data set with the required features

    ## import, cast to numeric, rename per feature.txt
    test_x_df <- read.table(x_file
                            , sep = ""
                            , header = FALSE
                            , na.strings = ""
                            , col.names = feature_labels_df[,2]
                            , check.names = FALSE
                            , stringsAsFactors = FALSE)

    test_y_df <- read.table(y_file
                            , sep = ""
                            , header = FALSE
                            , na.strings = ""
                            , col.names = "Y"
                            , colClasses = "numeric"
                            , stringsAsFactors = FALSE)

    test_subjects_df <- read.table(subject_file
                                   , sep = ""
                                   , header = FALSE
                                   , na.strings = ""
                                   , col.names = "subject"
                                   , stringsAsFactors = FALSE)

    test_xy_df <- cbind(test_subjects_df
                        , test_y_df
                        , test_x_df[,grep("mean|std"
                                          , names(test_x_df))]
                        )

    # merge descriptive y activities  test_subjects_df

    test_xy_merged_df <- merge(test_xy_df, activity_labels_df
                               , by.x = "Y", by.y = "id"
                               , all = FALSE)
    test_xy_merged_df <- select(test_xy_merged_df, -Y)

}

#process test data
test_df <- get_data("./test/X_test.txt"
                    , "./test/y_test.txt"
                    , "./test/subject_test.txt")
# process training data
train_df <- get_data("./train/X_train.txt"
                     , "./train/y_train.txt"
                     , "./train/subject_train.txt")

# combine test and train data
test_train_df <- rbind(test_df, train_df)

# remove construction blocks
rm(list = c("test_df", "train_df", "activity_labels_df", "feature_labels_df"))

## create new df with avg of each values for each activity for each subject

final_df <- test_train_df %>%
            group_by(subject, activity) %>%
            summarize_if(is.numeric, mean, na.rm = TRUE) %>%
            write.table("./analysis.txt"
                      , row.names = FALSE
                      )

