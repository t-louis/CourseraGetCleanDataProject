################################################################################
##  Prerequisites:
##  --------------
## Required packages which needs to be installed
## in user environment prior to run the analysis
## script:
##  - tidyverse
##  - data.table
##  - dplyr
##  - fs
################################################################################
library(tidyverse)
library(data.table)
library(dplyr)
library(fs)

################################################################################
## Some operations such as summarize and group_by would transform
## data frame into tibble. Tibbles have different signiifcant digits precision
## (default is 3) whereas data in dataset values have 10. In order not to loose
## in precision we need to issue options command to alter floating points
## precision
################################################################################
options(digits = 10)
options(pillar.sigfig = 10)

################################################################################
## Master function that perform the overall data processing. It allows to
## define auxiliary functions that are only required for the data processing
## and would not be visible within the current session. In addition, we can
## variables that are only visible within the function so that we do not
## polute the current session. The only data to be created and exported outside
## would be the tidy sanitized data frame.
################################################################################
run_analysis <- function() {
  
  ################################################################################
  ##  Assumptions:
  ##  -----------
  ##  - The unpacked dataset files are in sub-directory called "dataset"
  ##  in the project directory. If you dataset is elsewhere, please make
  ##  sure your are updating variable datasetDirectory to the right
  ##  value)
  ##  - Setting some variables which is bound to the dataset (more
  ##  information can be found in "dataset/README.txt" file) and used
  ##  in below various functions:
  ##    - featureFile                : Path to CSV containing dataset feature names
  ##    - activityFile               : Path to CSV containing dataset activity names
  ##    - trainSubjectFile           : Path to CSV training subject dataset file
  ##    - trainMeasureFile           : Path to CSV training measurement dataset file
  ##    - trainLabelFile             : Path to CSV training label file
  ##    - testSubjectFile            : Path to CSV test subject dataset file
  ##    - testMeasureFile            : Path to CSV test measurement dataset file
  ##    - testLabelFile              : Path to CSV test label file
  ##    - tidyDatasetFile            : Path of CSV file holding the final tidy dataset
  ##    - numDatasetFeatures         : Total number of features (561)
  ##    - subjectFeatureName         : Feature name representing subjects
  ##    - activityFeatureName        : Feature name representing labels
  ##    - datasetFeatureList         : List of all raw features to be retrieved
  ##    - datasetSelectedFeatureList : List of selected raw features based on mean and std measurements
  ##    - datasetMappedFeatureList   : List of re-factored features from selected raw features
  ##    - numDatasetActivities       : Total number of measured activities (6)
  ##    - datasetActivityList        : list of all raw activities
  ##    - activityValidRange         : List of valid values for labels in raw dataset
  ##    - numDatasetSubjects         : Total number of measured subjects (30)
  ##    - subjectValidRange          : List of valid values for subjects in raw dataset
  ##    - trainDataFrame             : Data frame representing raw training dataset
  ##    - testDataFrame              : Data frame representing raw test dataset
  ##    - combinedDataFrame          : Data frame representing raw combined (training, test) dataset
  ##    - tidyDataFrame              : Filtered (reduced and remapped features) and tidy combined dataset 
  ##
  ################################################################################
  
  ## ---> Begin section for user to modify <--- ##
  datasetDirectory           <- path(getwd(), "dataset")
  featureFile                <- path(datasetDirectory, "features.txt")
  activityFile               <- path(datasetDirectory, "activity_labels.txt")
  trainSubjectFile           <- path(datasetDirectory, "train", "subject_train.txt")
  trainMeasureFile           <- path(datasetDirectory, "train", "X_train.txt")
  trainLabelFile             <- path(datasetDirectory, "train", "y_train.txt")
  testSubjectFile            <- path(datasetDirectory, "test", "subject_test.txt")
  testMeasureFile            <- path(datasetDirectory, "test", "X_test.txt")
  testLabelFile              <- path(datasetDirectory, "test", "y_test.txt")
  tidyDatasetFile            <- path(getwd(), "tidy_dataset.csv")
  ## ---> End section for user to modify <--- ##
  
  numDatasetFeatures         <- 561
  subjectFeatureName         <- "subject"
  activityFeatureName        <- "activity"
  datasetFeatureList         <- NULL
  datasetSelectedFeatureList <- NULL
  datasetMappedFeatureList   <- NULL
  numDatasetActivities       <- 6
  datasetActivityList        <- NULL
  activityValidRange         <- 1:numDatasetActivities
  numDatasetSubjects         <- 30
  subjectValidRange          <- 1:numDatasetSubjects
  trainDataFrame             <- NULL
  testDataFrame              <- NULL
  combinedDataFrame          <- NULL
  tidyDataFrame              <- NULL
  
  ################################################################################
  ##  Function: mapfeatureName
  ##  -----------------------
  ##  Auxiliary function that enables to map existing
  ##  raw feature names into non-illegal characters.
  ##  We do not try to map the original measurement names
  ##  as they were described in "dataset/features_info.txt".
  ##  However, we would manipulate suffixes or raw dataset
  ##  feature names. We remove "()" and "-". We replace "mean"
  ##  by "Mean" and "std" by "Std".
  ##
  ##  Example: "tBodyAcc-mean()-X" would become "tBodyAccMeanX"
  ##
  ##  Argument(s):
  ##  ----------
  ##  - feature: character array representing original feature name
  ##
  ##  Return Value:
  ##  ------------
  ##  character array / NULL / original argument value (if unabled to be processed)
  ##
  ################################################################################
  mapfeatureName <- function(feature) {
    
    ## Initialize the mapped feature name with
    ## the original feature name so that if we
    ## unabled to process it then we at least
    ## return the original feature name instead
    ## of NULL
    mappedfeature <- feature
    if(is.character(feature) & (nchar(feature) > 0)) {
      
      ## Substitute any occurence of "-" to "_"
      mappedfeature <- gsub("-", "", mappedfeature, ignore.case = TRUE)
      
      ## Remove any occurrence of "()" 
      mappedfeature <- gsub("mean\\(\\)", "Mean", mappedfeature, ignore.case = TRUE)
      mappedfeature <- gsub("std\\(\\)", "Std", mappedfeature, ignore.case = TRUE)
    }
    mappedfeature
  }
  
  ################################################################################
  ##  Function: getFeatureList
  ##  ------------------------
  ##  Auxiliary function retrieve the list of all feature names
  ##  by parsing file "dataset/features.txt".
  ##  We expect and character table of 561 x 2 (second column hold feature names) 
  ##
  ##  Argument(s):
  ##  ----------
  ##  - file: path to file containing list of raw dataset feature names
  ##
  ##  Return Value:
  ##  -----------
  ##  list / NULL 
  ##
  ################################################################################
  getFeatureList <- function(file) {
    
    ## Initialize the data frame that will return
    ## the information about raw feature names
    data <- NULL
    
    ## Parse file
    if(is_file(file) & !is_file_empty(file)) {
      df <- read.table(file, header = FALSE)
      
      ## We need to check if we have a valid data frame
      ## before passing it over
      if(is.data.frame(df)) {
        
        ## We first check that we have the right
        ## dimensions of the data frame 561 x 2
        if(any((dim(df) == c(numDatasetFeatures, 2)) == FALSE)) {
          stop(sprintf("Parsed feature file specified %s resulted in wrong data frame dimensions.", file))
        } else {
          
          ## We check each feature names as not either empty string or
          ## NA
          features <- df$V2
          if((any(is.na(features) == TRUE)) | any((nchar(features) == 0) == TRUE)) {
            stop(sprintf("Parsed feature file specified %s contains wrong values (NAs and/or empty strings).", file))
          } else {
            
            ## We can safely make the return value assignment
            data <- features
          }
        }
      } else {
        stop(sprintf("Unable to parse feature file %s", file))
      }
    } else {
      stop(sprintf("Invalid feature file %s", file))
    }
    
    ## Return the list
    data
  }
  
  ################################################################################
  ## Initialize the list of features by calling function getFeatureList
  ################################################################################
  datasetFeatureList <- getFeatureList(featureFile)
  if(is.null(datasetFeatureList)) {
    stop(sprintf("Unable to retrieve the list of raw dataset features"))
  }
  
  ################################################################################
  ##  Function: selectFeatureList
  ##  ---------------------------
  ##  Auxiliary function to retrieve the list of feature names associated with
  ##  mean and standard deviation for each measurement. It relies on information
  ##  retrieve from function getFeatureList
  ##
  ##  Argument(s):
  ##  -----------
  ##  - featureList: list of raw dataset feature names
  ##
  ##  Return Value:
  ##  ------------
  ##  list / NULL 
  ##
  ################################################################################
  selectFeatureList <- function(featureList) {
    
    ## Initialize the data frame that will return
    ## the information about selected feature names
    data <- NULL
    
    if(is.character(featureList)) {
      fl <- Filter(function(x) {grepl("mean\\(\\)|std\\(\\)", x, ignore.case = TRUE)}, featureList)
      if(is.character(fl)) {
        data <- fl
      } 
    } 
    
    ## Return the list
    data
  }
  
  ################################################################################
  ## Initialize the list of features by calling function getFeatureList
  ################################################################################
  datasetSelectedFeatureList <- selectFeatureList(datasetFeatureList)
  if(is.null(datasetSelectedFeatureList)) {
    stop(sprintf("Unable to extract list of selected dataset features (mean and std for each measurement"))
  }
  datasetMappedFeatureList <- unlist(lapply(datasetSelectedFeatureList, mapfeatureName))
  
  ################################################################################
  ##  Function: getActivityList
  ##  -------------------------
  ##  Auxiliary function retrieve the list of all activity names
  ##  by parsing file "dataset/activity_labels.txt".
  ##  We expect and character table of 6 x 2 (second column hold activity names) 
  ##
  ##  Argument(s):
  ##  -----------
  ##  - file: path to file containing list of raw dataset feature names
  ##
  ##  Return Value:
  ##  ------------
  ##  data.frame / NULL 
  ##
  ################################################################################
  getActivityList <- function(file) {
    
    ## Initialize the data frame that will return
    ## the information about raw activity names
    data <- NULL
    
    ## Parse file
    if(is_file(file) & !is_file_empty(file)) {
      df <- read.table(file, header = FALSE)
      
      ## We need to check if we have a valid data frame
      ## before passing it over
      if(is.data.frame(df)) {
        
        ## We first check that we have the right
        ## dimensions of the data frame 561 x 2
        if(any((dim(df) == c(numDatasetActivities, 2)) == FALSE)) {
          stop(sprintf("Parsed activity label file specified %s resulted in wrong data frame dimensions.", file))
        } else {
          
          ## We check each activity names as not either empty string or
          ## NA
          activities <- df$V2
          if((any(is.na(activities) == TRUE)) | any((nchar(activities) == 0) == TRUE)) {
            stop(sprintf("Parsed activity label file specified %s contains wrong values (NAs and/or empty strings).", file))
          } else {
            
            ## We can safely create the list of features
            data <- activities
          }
        }
      } else {
        stop(sprintf("Unable to parse activity label file %s", file))
      }
    } else {
      stop(sprintf("Invalid activity label file %s", file))
    }
    
    ## Return the list
    data
  }
  
  ################################################################################
  ## Initialize the list of activities by calling function getActivityList
  ################################################################################
  datasetActivityList <- getActivityList(activityFile)
  if(is.null(datasetActivityList)) {
    stop(sprintf("Unable to retrieve the list of raw activity label list"))
  }
  
  ################################################################################
  ##  Function: getMeasurementDataFrame
  ##  ---------------------------------
  ##  Auxiliary function retrieve raw measurements for either training or test dataset
  ##  by parsing files "dataset/train/*_train.txt" or "dataset/train/*_test.txt".
  ##  We expect and data frame of <number or observations> x (1 + 561 + 1)  
  ##
  ##  Argument(s):
  ##  -----------
  ##  - subjectFile: path to file containing list of subjects per observation
  ##  - measurementFile : path to file containing 561 measurements per observation
  ##  - labelFile : path to file containing label activity per observation
  ##
  ##  Return Value:
  ##  ------------
  ##  data.frame / NULL 
  ##
  ################################################################################
  getMeasurementDataFrame <- function(subjectFile, measurementFile, labelFile) {
    
    ## Initialize the data frame that will return
    ## the information about raw observations
    data <- NULL
    
    ## We first parse the subject file.
    ## We expect a data frame of one colum
    ## column value is a number 1 and 30
    if(is_file(subjectFile) & !is_file_empty(subjectFile)) {
      df <- read.table(subjectFile, header = FALSE)
      if(is.data.frame(df)) {
        dfDim <- dim(df)
        if((dfDim[2] == 1) & all(df$V1 %in% subjectValidRange)) {
          
          ## Assign a meaningful data frame to identify the number of
          ## observations in the dataset
          subjects <- df
          colnames(subjects) <- c(subjectFeatureName)
          numObs <- dfDim[1]
          print(sprintf("*** INFO ***: %d observations found in dataset subject file %s", numObs, subjectFile))
          
        } else {
          stop(sprintf("Dataset subject file %s is invalid or has occurence(s) of invalid subject id", subjectFile))
        }
      } else {
        stop(sprintf("Unable to parse dataset subject file  %s", subjectFile))
      }
    } else {
      stop(sprintf("Invalid dataset subject file  %s", subjectFile))
    }
    
    ## We parse the measurement file.
    ## We expect a data frame of numObs x 561
    ## column value are floating numbers
    if(is_file(measurementFile) & !is_file_empty(measurementFile)) {
      
      ## We use fread instead of read.table as it is much
      ## faster for large dataset
      df <- fread(measurementFile, header = FALSE)
      if(is.data.frame(df)) {
        dfDim <- dim(df)
        if(any((dfDim == c(numObs, numDatasetFeatures)) == FALSE)) {
          stop(sprintf("Parsed dataset measurement file %s has invalid dimensions %d x %d. Expected dimensions %d x %d", file, dfDim[1], dfDim[2], numObs, numDatasetFeatures))
        } else {
          if(sum(is.na(df)) == 0) {
            
            ## We have a fully validated table
            ## We extract needed columns
            ## We remapped the columns
            measures <- df
            colnames(measures) <- datasetFeatureList
            measures <- select(measures, datasetSelectedFeatureList)
            
            ## We alter the column names for the remapped selected
            ## feature list
            colnames(measures) <- datasetMappedFeatureList
            
            ## Order measurements
            measures <- select(measures, order(datasetMappedFeatureList))
            
          } else {
            
            ## We do not know what to do if we have invalid measurement
            ## values or NAs.
            ## We decide to exist the processing until the measurement is
            ## fixed
            stop(sprintf("Parses dataset measurement file %s has invalid measurement values or NAs", measurementFile))
          }
        }
      } else {
        stop(sprintf("Unable to parse dataset measurement file  %s", measurementFile))
      }
    } else {
      stop(sprintf("Invalid dataset measurement file  %s", measurementFile))
    }
    
    ## We parse the label file.
    ## We expect a data frame of numObs x 1 and the
    ## column value is a number 1 and 6
    if(is_file(labelFile) & !is_file_empty(labelFile)) {
      df <- read.table(labelFile, header = FALSE)
      if(is.data.frame(df)) {
        dfDim <- dim(df)
        if(all((dfDim == c(numObs, 1)) == TRUE) & all(df$V1 %in% activityValidRange)) {
          
          ## Assign a meaningful data frame to identify the number of
          ## observations in the dataset
          labels <- df
          colnames(labels) <- c(activityFeatureName)
          
          ## Remap activity id with activity name
          labels$activity <- unlist(lapply(labels$activity, function(x) {datasetActivityList[x]}))
          
        } else {
          stop(sprintf("Label file %s is invalid or has occurence(s) of invalid activity id", labelFile))
        }
      } else {
        stop(sprintf("Unable to parse dataset label file  %s", labelFile))
      }
    } else {
      stop(sprintf("Invalid dataset label file  %s", labelFile))
    }
    
    ## When we reached this stage, it means the processing has not stopped
    ## and we can safely create the overall data frame by merging subjects,
    ## measures and labels
    data <- cbind(subjects, measures)
    data <- cbind(data, labels)
    
    ## Return the data frame
    data
  }
  
  ################################################################################
  ## Retrieve training data set
  ################################################################################
  trainDataFrame <- getMeasurementDataFrame(
    subjectFile = trainSubjectFile,
    measurementFile = trainMeasureFile,
    labelFile = trainLabelFile
  )
  
  if(is.null(trainDataFrame)) {
    stop(sprintf("Unable to retrieve training dataset"))
  } else {
    print("*** INFO *** : Successfully retrieve training dataset")
  }
  
  ################################################################################
  ## Retrieve test data set
  ################################################################################
  testDataFrame <- getMeasurementDataFrame(
    subjectFile = testSubjectFile,
    measurementFile = testMeasureFile,
    labelFile = testLabelFile
  )
  
  if(is.null(testDataFrame)) {
    stop(sprintf("Unable to retrieve test dataset"))
  } else {
    print("*** INFO *** : Successfully retrieve test dataset")
  }
  
  ################################################################################
  ## Combine dataset
  ################################################################################
  combinedDataFrame <- rbind(trainDataFrame, testDataFrame)
  if(is.null(combinedDataFrame)) {
    stop(sprintf("Unable to combine training and test dataset"))
  } else {
    print(sprintf("*** INFO *** : Successfully retrieve test dataset with %d observations", dim(combinedDataFrame)[1]))
  }
  
  ################################################################################
  ##  Function: summarizeMeasurementDataFrame
  ##  --------------------------------------
  ##  Auxiliary function to summarize the factored combined dataset of average of
  ##  all selected measurement per subject and per activity
  ##
  ##  Argument(s):
  ##  -----------
  ##  - dataset: data.frame dataset
  ##
  ##  Return:
  ##  ------
  ##  data.frame / NULL 
  ##
  ################################################################################
  summarizeMeasurementDataFrame <- function(dataset) {
    
    ## Initialize the sanitized data frame 
    data <- NULL
    
    if(is.data.frame(dataset)) {
      
      ## Group the data frame by subject and activity
      df <- dataset
      df <- group_by(df, subject, activity)
      data <- summarize_all(df, mean)
    }
    
    ## Return the resulting data frame
    data
  }
  
  ################################################################################
  ## Sanitized combined dataset
  ################################################################################
  tidyDataFrame <- summarizeMeasurementDataFrame(combinedDataFrame)
  if(is.null(tidyDataFrame)) {
    stop(sprintf("Unable to generate tidy dataset"))
  } else {
    print(sprintf("*** INFO *** : Successfully generated tidy dataset with %d observations", dim(tidyDataFrame)[1]))
  }
  
  ## Conversion isrequired because a tibble was generated
  tidyDataFrame <- as.data.frame(tidyDataFrame)

  ################################################################################
  ## Write the tidy datset into a CSV file !!!
  ################################################################################
  fwrite(tidyDataFrame, file=tidyDatasetFile)
  if(is_file(tidyDatasetFile) & !is_file_empty(tidyDatasetFile)) {
    print(sprintf("*** INFO *** : Successfully created tidy dataset file %s", tidyDatasetFile))
  } else {
    stop(sprintf("Unable to create tidy datase file %s", tidyDatasetFile))
  }
  
  ################################################################################
  ## Print successful analysis greeting !!!
  ################################################################################
  print("*** INFO *** : Tidy data processing completed successfully ")
  
  ## Return the tidy data frame
  tidyDataFrame
  
}

################################################################################
## Call the main function run_analysis to generate
## the tidy data set and visualize it with View() function
################################################################################
tidy <- run_analysis()
View(tidy)

################################################################################
## Run garbage collector to free up some memory
################################################################################
gc()
