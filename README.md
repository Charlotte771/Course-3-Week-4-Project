HOW THE SCRIPT WORKS
  
  The script is divided into sections based on each part of the assignment. 
  
  The first part involves downloading the file and renaming each of the data tables for easier use. The varible in "ytest/train" is the "activityID" and "subtest/train" is the "subjectID". The names of the columns in "xtrain/test" need to be corrected and match the names provided in the second column of "features.txt". Lastly, the sections of test and train are combined into one data frame.
  
  To replace the activityID with the actual activity the activity labels file needed the columns to be renamed and then the activities will be substituted with the code.
  
  Since the variable names that needed to be changed had common substitutions and were in multiple variable names, to prevent from having to rename each of the titles one-by-one I used gsub so they would all get changed at once.
  
  By using the group_by funciton I was able to take the dataset and group it by subjectID and activityID making sure to put subjectID first so it is organized by it first. The the summarize_all takes the dataset and applies the mean function which finds the mean values of each activity for each subject.
