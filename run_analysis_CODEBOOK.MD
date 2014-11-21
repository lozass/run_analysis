Description of the columns of the Tidy Data Set
Code book
By S. Loza
November 21, 2014

Overall information about the numeric signals. 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals. These temporal(time) domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing the frequency domain signals. 

These signals were used to estimate variables of the feature vector for each pattern: '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.



Column   Column.Name                                      Type/values or Description

1   subject	                                              1 to 30 - identification of the subject of the observation. identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
2	  activity	Categorical.                                Type of activity the subject was doing. One of "LAYING", "SITTING", "STANDING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"
3	  Avg.Temporal.Body.Acceleration.mean.X	                Numeric variable, average of Body Acceleration means-X axis, temporal domain
4	  Avg.Temporal.Body.Acceleration.mean.Y	                Numeric variable, average of Body Acceleration means-Y axis, temporal domain
5	  Avg.Temporal.Body.Acceleration.mean.Z	                Numeric variable, average of Body Acceleration means-Z axis, temporal domain
6	  Avg.Temporal.Body.Acceleration.stdev.X	              Numeric variable, average of Body Acceleration Standard Deviation - X axis, temporal domain
7	  Avg.Temporal.Body.Acceleration.stdev.Y	              Numeric variable, average of Body Acceleration Standard Deviation - Y axis, temporal domain
8	  Avg.Temporal.Body.Acceleration.stdev.Z	              Numeric variable, average of Body Acceleration Standard Deviation - Z axis, temporal domain
9	  Avg.Temporal.Gravity.Acceleration.mean.X	            Numeric variable, average of Gravity Acceleration mean - X axis, temporal domain 
10	Avg.Temporal.Gravity.Acceleration.mean.Y	            Numeric variable, average of Gravity Acceleration mean - Y axis, temporal domain
11	Avg.Temporal.Gravity.Acceleration.mean.Z	            Numeric variable, average of Gravity Acceleration mean - Z axis, temporal domain
12	Avg.Temporal.Gravity.Acceleration.stddev.X	          Numeric variable, average of Gravity Acceleration Standard Deviation - X axis, temporal domain
13	Avg.Temporal.Gravity.Acceleration.stddev.Y	          Numeric variable, average of Gravity Acceleration Standard Deviation - Y axis, temporal domain
14	Avg.Temporal.Gravity.Acceleration.stddev.Z	          Numeric variable, average of Gravity Acceleration Standard Deviation - Z axis, temporal domain
15	Avg.Temporal.Body.Acceleration.Jerk.mean.X	          Numeric variable, average of Body Acceleration Jerk mean - X axis, temporal domain
16	Avg.Temporal.Body.Acceleration.Jerk.mean.Y	          Numeric variable, average of Body Acceleration Jerk mean - Y axis, temporal domain
17	Avg.Temporal.Body.Acceleration.Jerk.mean.Z	          Numeric variable, average of Body Acceleration Jerk mean - Z axis, temporal domain
18	Avg.Temporal.Body.Acceleration.Jerk.stddev.X	        Numeric variable, average of Body Acceleration Jerk Standard Deviation - X axis, temporal domain
19	Avg.Temporal.Body.Acceleration.Jerk.stddev.Y	        Numeric variable, average of Body Acceleration Jerk Standard Deviation - Y axis, temporal domain
20	Avg.Temporal.Body.Acceleration.Jerk.stddev.Z	        Numeric variable, average of Body Acceleration Jerk Standard Deviation - Z axis, temporal domain
21	Avg.Temporal.Body.Gyro.mean.X	                        Numeric variable, average of Body angular velocity from Gyroscope mean - X axis, temporal domain
22	Avg.Temporal.Body.Gyro.mean.Y	                        Numeric variable, average of Body angular velocity from Gyroscope mean - Y axis, temporal domain
23	Avg.Temporal.Body.Gyro.mean.Z	                        Numeric variable, average of Body angular velocity from Gyroscope mean - Z axis, temporal domain
24	Avg.Temporal.Body.Gyro.stddev.X	                      Numeric variable, average of Body angular velocity from Gyroscope standard Deviation - X axis, temporal domain
25	Avg.Temporal.Body.Gyro.stddev.Y	                      Numeric variable, average of Body angular velocity from Gyroscope standard Deviation - Y axis, temporal domain
26	Avg.Temporal.Body.Gyro.stddev.Z	                      Numeric variable, average of Body angular velocity from Gyroscope standard Deviation - Z axis, temporal domain
27	Avg.Temporal.Body.Gyro.Jerk.mean.X	                  Numeric variable, average of Body angular velocity from Gyroscope Jerk mean - X axis, temporal domain
28	Avg.Temporal.Body.Gyro.Jerk.mean.Y	                  Numeric variable, average of Body angular velocity from Gyroscope Jerk mean - Y axis, temporal domain
29	Avg.Temporal.Body.Gyro.Jerk.mean.Z	                  Numeric variable, average of Body angular velocity from Gyroscope Jerk mean - Z axis, temporal domain
30	Avg.Temporal.Body.Gyro.Jerk.stddev.X	                Numeric variable, average of Body angular velocity from Gyroscope Jerk standard deviation - X axis, temporal domain
31	Avg.Temporal.Body.Gyro.Jerk.stddev.Y	                Numeric variable, average of Body angular velocity from Gyroscope Jerk standard deviation - Y axis, temporal domain
32	Avg.Temporal.Body.Gyro.Jerk.stddev.Z	                Numeric variable, average of Body angular velocity from Gyroscope Jerk standard deviation - Z axis, temporal domain
33	Avg.Temporal.Body.Acceleration.Magnitude.mean.X	      Numeric variable, average of Body Acceleration Magnitude mean - X axis, temporal domain
34	Avg.Temporal.Body.Acceleration.Magnitude.stddev.X	    Numeric variable, average of Body Acceleration Magnitude Standard Deviation - X axis, temporal domain
35	Avg.Temporal.Gravity.Acceleration.Magnitude.mean	    Numeric variable, average of Gravity Acceleration Magnitude mean , temporal domain
36	Avg.Temporal.Gravity.Acceleration.Magnitude.stddev	  Numeric variable, average of Gravity Acceleration Magnitude standard deviation, temporal domain
37	Avg.Temporal.Body.Acceleration.Magnitude.mean	        Numeric variable, average of Body Acceleration magnitude mean, temporal domain
38	Avg.Temporal.Body.Acceleration.Magnidude.stddev	      Numeric variable, average of Body Acceleration magnitude standard deviation, temporal domain
39	Avg.Temporal.Body.Gyro.Magnitude.mean	                Numeric variable, average of Body angular velocity from Gyroscope Magnitude Standard Deviation, temporal domain
40	Avg.Temporal.Body.Gyro.Magnitude.stddev	              Numeric variable, average of Body angular velocity from Gyroscope Magnitude standard deviation, temporal domain
41	Avg.Temporal.Body.Gyro.Jerk.Magnitude.mean	          Numeric variable, average of Body angular velocity from Gyroscope Jerk Magnitude mean , temporal domain
42	Avg.Temporal.Body.Gyro.Jerk.Magnitude.stddev	        Numeric variable, average of Body angular velocity from Gyroscope Jerk Magnitude standard deviation, temporal domain
43	Avg.Frequency.Body.Acceleration.mean.X	              Numeric variable, average of Body Acceleration mean - X axis, frequency domain
44	Avg.Frequency.Body.Acceleration.mean.Y	              Numeric variable, average of Body Acceleration mean - Y axis, frequency domain
45	Avg.Frequency.Body.Acceleration.mean.Z	              Numeric variable, average of Body Acceleration mean - Z axis, frequency domain
46	Avg.Frequency.Body.Acceleration.stddev.X	            Numeric variable, average of Body Acceleration Standard Deviation - X axis, frequency domain
47	Avg.Frequency.Body.Acceleration.stddev.Y	            Numeric variable, average of Body Acceleration Standard Deviation - Y axis, frequency domain
48	Avg.Frequency.Body.Acceleration.stddev.Z	            Numeric variable, average of Body Acceleration Standard Deviation - Z axis, frequency domain
49	Avg.Frequency.Body.Acceleration.Jerk.mean.X	          Numeric variable, average of Body Acceleration Jerk mean - X axis, frequency domain
50	Avg.Frequency.Body.Acceleration.Jerk.mean.Y	          Numeric variable, average of Body Acceleration Jerk mean - Y axis, frequency domain
51	Avg.Frequency.Body.Acceleration.Jerk.mean.Z	          Numeric variable, average of Body Acceleration Jerk mean - Z axis, frequency domain
52	Avg.Frequency.Body.Acceleration.Jerk.stddev.X	        Numeric variable, average of Body Acceleration jerk standard deviation - X axis, frequency domain
53	Avg.Frequency.Body.Acceleration.Jerk.stddev.Y	        Numeric variable, average of Body Acceleration jerk standard deviation - Y axis, frequency domain
54	Avg.Frequency.Body.Acceleration.Jerk.stddev.Z	        Numeric variable, average of Body Acceleration jerk standard deviation - Z axis, frequency domain
55	Avg.Frequency.Body.Gyro.mean.X	                      Numeric variable, average of Body angular velocity from Gyroscope Mean - X axis, frequency domain
56	Avg.Frequency.Body.Gyro.mean.Y	                      Numeric variable, average of Body angular velocity from Gyroscope Mean - Y axis, frequency domain
57	Avg.Frequency.Body.Gyro.mean.Z	                      Numeric variable, average of Body angular velocity from Gyroscope Mean - Z axis, frequency domain
58	Avg.Frequency.Body.Gyro.stddev.X	                    Numeric variable, average of Body angular velocity from Gyroscope Standard Deviation - X axis, frequency domain
59	Avg.Frequency.Body.Gyro.stddev.Y	                    Numeric variable, average of Body angular velocity from Gyroscope Standard Deviation - Y axis, frequency domain
60	Avg.Frequency.Body.Gyro.stddev.Z	                    Numeric variable, average of Body angular velocity from Gyroscope Standard Deviation - Z axis, frequency domain
61	Avg.Frequency.Body.Acceleration.Magnitude.mean	      Numeric variable, average of Body Acceleration Magnitude mean, frequency domain
62	Avg.Frequency.Body.Acceleration.Magnitude.stddev	    Numeric variable, average of Body Acceleration Magnitude Standard Deviation, frequency domain
63	Avg.Frequency.Body.Acceleration.Jerk.Magnitude.mean	  Numeric variable, average of Body Acceleration Jerk Magnitude mean, frequency domain
64	Avg.Frequency.Body.Acceleration.Jerk.Magnitude.stddev	Numeric variable, average of Body Acceleration Jerk Magnitude Standard Deviation, frequency domain
65	Avg.Frequency.Body.Gyro.Magnitude.mean	              Numeric variable, average of Body angular velocity from Gyroscope magnitude mean, frequency domain
66	Avg.Frequency.Body.Gyro.Magnitude.stddev	            Numeric variable, average of Body angular velocity from Gyroscope Magnitude standard Deviation, frequency domain
67	Avg.Frequency.Body.Gyro.Jerk.Magnitude.mean	          Numeric variable, average of Body angular velocity from Gyroscope Jerk magnitude mean, frequency domain
68	Avg.Frequency.Body.Gyro.Jerk.Magnitude.stddev	        Numeric variable, average of Body angular velocity from Gyroscope Jerk magnitude standard deviation, frequency domain
