#Repository for the Coursera Getting and Cleaning Data
##Course Project
###Objective
This repository have the objective to maintain all the files resulting of the course project challenge:
> The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
> One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.
###Contents
Only have three files, described below:
* **README.md** --- This file;
* **codebook.md** --- Describes the variables and the data resulting script execution;
* **run_analysis.R** --- The R script, your description and details are encountered bellow.
###Pre requisites
Before the execution of *run_analysis.R* you need:
1. Download [this file](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip, "Necessary files to download") (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip);
2. Unpack the downloaded file in the **R work directory**;
3. **Don't change any directory structure** of unpacked file, changes make this script don't work!
###Installation
This script does not need installation, simply download the *run_analysis.R* file and put in the **R work directory**.
After this, load inside the R:
    source("run_analysis.R")
    
If the pre requisites are complete, no action is necessary, because this script install and loads the necessary libraries, too, load all data files, process them and generate a output tabulated data file *tidy_data.txt*.
###How this stuff works??
In resume, this script:
* install and load all necessary libraries;
* join and treat all data origined from the downloaded files;
* and exit a data text file (tidy_data.txt) what contain a data set with the average of each variable for each activity and each subject.
**You can find some considerable execution details in source code.**
> That's all folks!


