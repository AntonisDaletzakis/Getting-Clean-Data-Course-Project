Getting Cleaning Data Project
------------------------------------
#####Antonios Daletzakis


###Data
The data has been carried out with a group of 30 voluteers. Each perform some activities with the Samsung Galaxy S II using the accelerometer and gyroscope.

We have been given 2 data set , a train and a test data set. 

###Step 1. Merge the train and test set in one
Unzip the data file and check the inside of them we figure out the data are:

* features.txt
* activity_labels.txt
* subject_train.txt
* x_train.txt
* y_train.txt
* subject_test.txt
* x_test.txt
* y_test.txt

###Step 2. Subset on the mean and standard deviation of the data
The only thing we had to do is to create a vector of logical values for this subset.

###Step 3. Use descriptive activity name
Use the activity_labels vector to let us more freedom to manypulate the data

###Step 4. Change the label of the data
The data labels was extremelly unreadable and so we had to convert them propertly.

###Step 5. Create a Tidy data set
The analysis we perform has to be conluded in a result, so the result is the new data set we extract.