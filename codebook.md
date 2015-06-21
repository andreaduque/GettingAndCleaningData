Codebook for data_tidy.txt
==================================

## Variables
````
subject                    1..2
    Subject number
                           1..30 .Unique identifier assigned to each subject

label                      6..18
    Acitivity label
                           "WALKING"
                           "WALKING_UPSTAIRS"
                           "WALKING_DOWNSTAIRS"
                           "SITTING"
                           "STANDING"
                           "LAYING"

timebodyaccelerometermean-x                        12
    Described below

timebodyaccelerometermean-y                        12
    Described below

timebodyaccelerometermean-z                        12
    Described below

timebodyaccelerometerstd-x                         12
    Described below

timebodyaccelerometerstd-y                         12
    Described below

timebodyaccelerometerstd-z                         12
    Described below

timegravityaccelerometermean-x                     12
    Described below

timegravityaccelerometermean-y                     12
    Described below

timegravityaccelerometermean-z                     12
    Described below

timegravityaccelerometerstd-x                      12
    Described below

timegravityaccelerometerstd-y                      12
    Described below

timegravityaccelerometerstd-z                      12
    Described below

timebodyaccelerometerjerkmean-x                    12
    Described below

timebodyaccelerometerjerkmean-y                    12
    Described below

timebodyaccelerometerjerkmean-z                    12
    Described below

timebodyaccelerometerjerkstd-x                     12
    Described below

timebodyaccelerometerjerkstd-y                     12
    Described below

timebodyaccelerometerjerkstd-z                     12
    Described below

timebodygyroscopemean-x                            12
    Described below

timebodygyroscopemean-y                            12
    Described below

timebodygyroscopemean-z                            12
    Described below

timebodygyroscopestd-x                             12
    Described below

timebodygyroscopestd-y                             12
    Described below

timebodygyroscopestd-z                             12
    Described below

timebodygyroscopejerkmean-x                        12
    Described below

timebodygyroscopejerkmean-y                        12
    Described below

timebodygyroscopejerkmean-z                        12
    Described below

timebodygyroscopejerkstd-x                         12
    Described below

timebodygyroscopejerkstd-y                         12
    Described below

timebodygyroscopejerkstd-z                         12
    Described below

timebodyaccelerometermagnitudemean                 12
    Described below

timebodyaccelerometermagnitudestd                  12
    Described below

timegravityaccelerometermagnitudemean              12
    Described below

timegravityaccelerometermagnitudestd               12
    Described below

timebodyaccelerometerjerkmagnitudemean             12
    Described below

timebodyaccelerometerjerkmagnitudestd              12
    Described below

timebodygyroscopemagnitudemean                     12
    Described below

timebodygyroscopemagnitudestd                      12
    Described below

timebodygyroscopejerkmagnitudemean                 12
    Described below

timebodygyroscopejerkmagnitudestd                  12
    Described below

frequencybodyaccelerometermean-x                   12
    Described below

frequencybodyaccelerometermean-y                   12
    Described below

frequencybodyaccelerometermean-z                   12
    Described below

frequencybodyaccelerometerstd-x                    12
    Described below

frequencybodyaccelerometerstd-y                    12
    Described below

frequencybodyaccelerometerstd-z                    12
    Described below

frequencybodyaccelerometerjerkmean-x               12
    Described below

frequencybodyaccelerometerjerkmean-y               12
    Described below

frequencybodyaccelerometerjerkmean-z               12
    Described below

frequencybodyaccelerometerjerkstd-x                12
    Described below

frequencybodyaccelerometerjerkstd-y                12
    Described below

frequencybodyaccelerometerjerkstd-z                12
    Described below

frequencybodygyroscopemean-x                       12
    Described below

frequencybodygyroscopemean-y                       12
    Described below

frequencybodygyroscopemean-z                       12
    Described below

frequencybodygyroscopestd-x                        12
    Described below

frequencybodygyroscopestd-y                        12
    Described below

frequencybodygyroscopestd-z                        12
    Described below

frequencybodyaccelerometermagnitudemean            12
    Described below

frequencybodyaccelerometermagnitudestd             12
    Described below

frequencybodyaccelerometerjerkmagnitudemean        12
    Described below

frequencybodyaccelerometerjerkmagnitudestd         12
    Described below

frequencybodygyroscopemagnitudemean                12
    Described below

frequencybodygyroscopemagnitudestd                 12
    Described below

frequencybodygyroscopejerkmagnitudemean            12
    Described below

frequencybodygyroscopejerkmagnitudestd             12
    Described below
````

## Data

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.


timebodyaccelerometermean-x             

timebodyaccelerometermean-y             

timebodyaccelerometermean-z             

timebodyaccelerometerstd-x              

timebodyaccelerometerstd-y              

timebodyaccelerometerstd-z              

timegravityaccelerometermean-x          

timegravityaccelerometermean-y          

timegravityaccelerometermean-z          

timegravityaccelerometerstd-x           

timegravityaccelerometerstd-y           

timegravityaccelerometerstd-z           

timebodyaccelerometerjerkmean-x         

timebodyaccelerometerjerkmean-y         

timebodyaccelerometerjerkmean-z         

timebodyaccelerometerjerkstd-x          

timebodyaccelerometerjerkstd-y          

timebodyaccelerometerjerkstd-z          

timebodygyroscopemean-x                 

timebodygyroscopemean-y                 

timebodygyroscopemean-z                 

timebodygyroscopestd-x                  

timebodygyroscopestd-y                  

timebodygyroscopestd-z                  

timebodygyroscopejerkmean-x             

timebodygyroscopejerkmean-y             

timebodygyroscopejerkmean-z             

timebodygyroscopejerkstd-x              

timebodygyroscopejerkstd-y              

timebodygyroscopejerkstd-z              

timebodyaccelerometermagnitudemean      

timebodyaccelerometermagnitudestd       

timegravityaccelerometermagnitudemean   

timegravityaccelerometermagnitudestd    

timebodyaccelerometerjerkmagnitudemean  

timebodyaccelerometerjerkmagnitudestd   

timebodygyroscopemagnitudemean          

timebodygyroscopemagnitudestd           

timebodygyroscopejerkmagnitudemean      

timebodygyroscopejerkmagnitudestd       

frequencybodyaccelerometermean-x        

frequencybodyaccelerometermean-y        

frequencybodyaccelerometermean-z        

frequencybodyaccelerometerstd-x         

frequencybodyaccelerometerstd-y         

frequencybodyaccelerometerstd-z         

frequencybodyaccelerometerjerkmean-x    

frequencybodyaccelerometerjerkmean-y    

frequencybodyaccelerometerjerkmean-z    

frequencybodyaccelerometerjerkstd-x     

frequencybodyaccelerometerjerkstd-y     

frequencybodyaccelerometerjerkstd-z     

frequencybodygyroscopemean-x            

frequencybodygyroscopemean-y            

frequencybodygyroscopemean-z            

frequencybodygyroscopestd-x             

frequencybodygyroscopestd-y             

frequencybodygyroscopestd-z             

frequencybodyaccelerometermagnitudemean    

frequencybodyaccelerometermagnitudestd     

frequencybodyaccelerometerjerkmagnitudemean

frequencybodyaccelerometerjerkmagnitudestd 

frequencybodygyroscopemagnitudemean        

frequencybodygyroscopemagnitudestd         

frequencybodygyroscopejerkmagnitudemean    

frequencybodygyroscopejerkmagnitudestd 


The set of variables that were estimated from these signals are:

**mean**: Mean value

**std**: Standard deviation

## Transformation

All the values are means, aggregated over 30 subjects and 6 activities, hence the resulting dataset is 180 rows by 68 columns.
