
# Objective

The exercise is to generate a tidy dataset derived from the following source dataset http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones . The generation of the tidy dataset would be done in following steps:

* Step 1: Merges the training and the test sets to create one data set.
* Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
* Step 3: Uses descriptive activity names to name the activities in the data set
* Step 4: Appropriately labels the data set with descriptive variable names. 
* Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

R script *run_analysis.R* was created in order to accomplish above 5 steps. However for programmatically reason, the script does not follow above steps sequentially but would achieve the same goals

# Approach

Both the training and test data are not merged from the beginning. They are treated separately. They would only be merged toward the end of the R script when we know it is safe to do so. At the beginning, we assume that both datasets might be "messy" and the script would pre-process them until we are certain that they have the right number of features and valid values. The R Script has following operations sequence:

* Extract through function **getFeatureList** the list of all features from file *<path to dataset directory>/features.txt*. It allows to ensure that we have match between the expected number of features and what stated in the dataset documentation (read file *<path to dataset directory>/README.txt*)
* Extract through function **selectFeatureList** the list of selected features representing mean and standard deviation for the each measurement. We simply detect in feature name subtrings **mean()** or **std()**. There are other feature names with substring such as *meanFrew* and other variations of *mean* which we are ignoring. The list of selected feature list allows to perform a column selection in the measurement dataset.
* Extract through function **getActivityList** the list of all subject activities from file *<path to dataset directory>/activity_labels.txt*. It allows to map activity dataset integer value to activity name.
* Extract through function **getMeasurementDataFrame** the training measaurement dataset from files *<path to dataset directory>/train/subject_train.txt*, *<path to dataset directory>/train/X_train.txt* and *<path to dataset directory>/train/y_train.txt*. During the extraction, only the desired selection of features are retained and column feature names have been renamed (as per instruction in Step 4 of the above objective section). 
* Extract through function **getMeasurementDataFrame** the test measaurement dataset from files *<path to dataset directory>/test/subject_test.txt*, *<path to dataset directory>/test/X_test.txt* and *<path to dataset directory>/train/y_test.txt*.
* Merge both processed training and test through a simple call to function **rbind**
* Generate the tidy derived dataset through function **summarizeMeasurementDataFrame** from above merged dataset
  
All above execution steps are encapsulated with a single function called **run_analysis**. It is the master function that allows the user to re-run the entire sequence. The code is made in such as a way that it is highly re-usable. There is a small section of the code that can be altered by the user. It is delimited in the code by entries **## ---> Begin section for user to modify <--- ##** and **## ---> End section for user to modify <--- ##**.

# Code execution

The whole code is contained in single file called *run_analysis.R*. The code assumes that following R packages are already installed:

* tidyverse
* dplyr
* data.table
* fs
  
The code also assumes by default that the whole dataset directory structure was unpacked under directory called **dataset** located in the R project working diretory. The entire execution can be launched by just sourcing the R script. At the end of the execution data frame called *tidy* will be available for the user to manipulate further (if required). If the code is executed in RStudio a **View(tidy)** has been issued so that the tidy dataset is displayed. Finally, a file called *tidy_dataset.csv* has been created containing the tidy dataset (as per Step 5 of above objective section).

