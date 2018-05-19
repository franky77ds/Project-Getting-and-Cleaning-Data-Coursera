# INTRODUCTION

This is a Project for the "Getting and Cleaning Data" course of Coursera.
## Review Criteria: 
* The submitted data set is tidy.
* The Github repo contains the required scripts.
* GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
* The README that explains the analysis files is clear and understandable.
* The work submitted for this project is the work of the student who submitted it.

# ABOUT THE DATASET

The experiments were carried out with a group of 30 volunteers within an age bracket of 19-48 years. They performed a protocol of activities composed of six basic activities: three static postures (standing, sitting, lying) and three dynamic activities (walking, walking downstairs and walking upstairs). The experiment also included postural transitions that occurred between the static postures. These are: stand-to-sit, sit-to-stand, sit-to-lie, lie-to-sit, stand-to-lie, and lie-to-stand. All the participants were wearing a smartphone (Samsung Galaxy S II) on the waist during the experiment execution. We captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz using the embedded accelerometer and gyroscope of the device. The experiments were video-recorded to label the data manually. The obtained dataset was randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of 561 features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

This dataset is an updated version of the UCI Human Activity Recognition Using smartphones Dataset that can be found at: [Web Link] 
This version provides the original raw inertial signals from the smartphone sensors, instead of the ones pre-processed into windows which were provided in version 1. This change was done in order to be able to make online tests with the raw data. Moreover, the activity labels were updated in order to include postural transitions that were not part of the previous version of the dataset. 


Attribute Information:

The dataset is then divided in two parts and they can be used separately. 

1. Inertial sensor data 
- Raw triaxial signals from the accelerometer and gyroscope of all the trials with with participants. 
- The labels of all the performed activities. 

2. Records of activity windows. Each one composed of: 
- A 561-feature vector with time and frequency domain variables. 
- Its associated activity label. 
- An identifier of the subject who carried out the experiment. 

The dataset includes the following files: 
========================================= 

- 'README.txt' 

-  The raw triaxial acceleration signal for the experiment number XX and associated to the user number YY. Every row is one acceleration sample (three axis) captured at a frequency of 50Hz. 

- The raw triaxial angular speed signal for the experiment number XX and associated to the user number YY. Every row is one angular velocity sample (three axis) captured at a frequency of 50Hz. 

-  include all the activity labels available for the dataset (1 per row). 
Column 1: experiment number ID, 
Column 2: user number ID, 
Column 3: activity number ID 
Column 4: Label start point (in number of signal log samples (recorded at 50Hz)) 
Column 5: Label end point (in number of signal log samples) 

- 'features_info.txt': Shows information about the variables used on the feature vector. 

- 'features.txt': List of all features. 

- 'activity_labels.txt': Links the activity ID with their activity name. 

- : Training set. 

- : Training labels. 

- : Test set. 

- : Test labels. 

- : Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- : Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

Notes: 
====== 

- Features are normalized and bounded within [-1,1]. 
- Each feature vector is a row on the 'X' and 'y' files. 
- The units used for the accelerations (total and body) are 'g's (gravity of earth -> 9.80665 m/seg2). 
- The gyroscope units are rad/seg. 
- A video of the experiment including an example of the 6 recorded activities with one of the participants can be seen in the following link: [Web Link] 

For more information about this dataset please contact har '@' smartlab.ws or check our website www.smartlab.ws

# DATA TRANSFORMATIONS
There was not transformation about data. Just column names

* We have join all test and train data sets in one dataset
* We have change the columns names of activities (activityid and activity)
* We have change the columns names of features (featureId and feature)
* We have change the columns names of X_test.txt and X_train.txt for the names in the second columns of features
* We have change the columns names of y_test.txt and y_train .txt for activityid
* In slicedMergedSet we have only the measurements on the mean and standard deviation for each measurement
* In slicedMergedSet$Activity we replace 1 2 3 4 5 6 by LAYING SITTING STANDING WALKING WALKING_DOWNSTAIRS WALKING_UPSTAIRS
* We remove "(",")" and "-" in the column names
* and change ^f for freq and ^t for time, mean for Mean and std for Std
