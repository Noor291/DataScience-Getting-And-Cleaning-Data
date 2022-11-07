# Getting-and-Cleaning-Data

## About this R script
File "run_analysis.R" performs 5 following steps:   
1. Merging the training and the test sets to create one data set.   
  - Reading files    
  - Reading trainings tables   
  - Reading testing tables   
  - Reading feature vector   
  - Reading activity labels
  - Merging Training and Testing sets
  - Assigning column names   
  - at last Merging all datasets in one set   

2. Extracting only the measurements on the mean and standard deviation for each measurement   
  - Extracting the indices columns using grep which either have mean or std in their names
  - making the vector along with the indices of these columns and columns "Activity" and "Subject"
  - extracting a new dataset containing only these columns
   
3. Using descriptive activity names to name the activities in the data set   
  - naming the activities according to activity_names
   
4. Appropriately labeling the data set with descriptive variable names 
  - labeling the column names with descriptive names that are more legible to people
  
5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject   
  - Making second tidy data set grouped by activity and subject 
  - Writing second tidy data set in txt file   
