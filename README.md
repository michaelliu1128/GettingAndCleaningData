Course project of "Getting and Cleaning Data"

Goal: 
1) To demonstrate the ability to collect, work with, and clean 
   a data set. 
2) To prepare tidy data that can be used for later analysis. You 
   will be 

Submit: 
1) A tidy data set (tidy.mean.csv) 
2) A script for performing the analysis (run_analysis.R)
3) A code book that describes the variables, the data, and any 
   transformations or work that is performed to clean up the data 
   (CodeBook.md)
4) A README.md (this file) explains how all of the scripts work 
   and how they are connected 
------------------------------------------------------------------

Background:
The data linked from website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the link for data of the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.  

How it works:
The file run_analysis.R shows the details script and comments on how to  
use R to manipulate and restructure data sets.

'data' is the data.frame variable which holds the merged measurement data under both training and testing data.

'subdata2' is the data.frame subset which holds only mean() and std() data.

melt() and dcast() functions from reshape2 library package are used to tablize and calculalte the average values of all measurement variables. With the 'for loop' conditional control of R to prepare the date for each candidate (subject) for each activity.

The created data set is exported as .csv file named tidy.mean.csv

A cook book (CookBook.md) is prepare to explain the format and descript the details of each variable in the data file.
